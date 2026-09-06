resolver_fixture_index <- function() {
  fields <- c(
    "Package",
    "Version",
    "Depends",
    "Imports",
    "LinkingTo",
    "Repository"
  )
  index <- matrix(
    NA_character_,
    nrow = 3L,
    ncol = length(fields),
    dimnames = list(
      c("zaktargetresolver", "zakdirectresolver", "zakleafresolver"),
      fields
    )
  )
  index[, "Package"] <- rownames(index)
  index[, "Version"] <- "1.0.0"
  index[, "Repository"] <- "file:///fixture"
  index["zaktargetresolver", "Imports"] <- "zakdirectresolver (>= 1.0.0)"
  index["zakdirectresolver", "Imports"] <- "zakleafresolver"
  index
}

# zak reimplements the selection `install.packages(dependencies = NA)` performs
# rather than reaching into `utils:::getDependencies`. This runs both over the
# same inputs and compares, so a divergence — in either direction — is caught.
utils_reference_resolver <- function() {
  tryCatch(
    get(
      "getDependencies",
      envir = environment(utils::install.packages),
      inherits = FALSE
    ),
    error = function(error) NULL
  )
}

resolver_outcome <- function(expression) {
  warned <- FALSE
  messaged <- FALSE
  value <- withCallingHandlers(
    expression,
    warning = function(condition) {
      warned <<- TRUE
      invokeRestart("muffleWarning")
    },
    message = function(condition) {
      messaged <<- TRUE
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, warned = warned, messaged = messaged)
}

resolver_index <- function(rows) {
  fields <- c(
    "Package",
    "Version",
    "Depends",
    "Imports",
    "LinkingTo",
    "Suggests",
    "Enhances"
  )
  index <- matrix(
    NA_character_,
    nrow = length(rows),
    ncol = length(fields),
    dimnames = list(names(rows), fields)
  )
  for (package in names(rows)) {
    index[package, "Package"] <- package
    index[package, "Version"] <- "1.0.0"
    for (field in names(rows[[package]])) {
      index[package, field] <- rows[[package]][[field]]
    }
  }
  index
}

test_that("resolution matches the selection install.packages performs", {
  reference <- utils_reference_resolver()
  testthat::skip_if(
    is.null(reference),
    "This R does not expose the internal resolver to compare against."
  )
  library <- .libPaths()[[1L]]

  scenarios <- list(
    chain = list(
      index = resolver_index(list(
        zakresolvetarget = list(Imports = "zakresolvedirect"),
        zakresolvedirect = list(Imports = "zakresolveleaf"),
        zakresolveleaf = list()
      )),
      package = "zakresolvetarget"
    ),
    diamond = list(
      index = resolver_index(list(
        zakresolvetarget = list(Imports = "zakresolvea, zakresolveb"),
        zakresolvea = list(Imports = "zakresolveleaf"),
        zakresolveb = list(Imports = "zakresolveleaf"),
        zakresolveleaf = list()
      )),
      package = "zakresolvetarget"
    ),
    all_strong_fields = list(
      index = resolver_index(list(
        zakresolvetarget = list(
          Depends = "R (>= 4.1.0), zakresolvea",
          Imports = "zakresolveb",
          LinkingTo = "zakresolvec",
          Suggests = "zakresolveskip",
          Enhances = "zakresolveskip"
        ),
        zakresolvea = list(),
        zakresolveb = list(),
        zakresolvec = list(),
        zakresolveskip = list()
      )),
      package = "zakresolvetarget"
    ),
    satisfiable_constraint = list(
      index = resolver_index(list(
        zakresolvetarget = list(Imports = "zakresolvedirect (>= 1.0.0)"),
        zakresolvedirect = list()
      )),
      package = "zakresolvetarget"
    ),
    unsatisfiable_constraint = list(
      index = resolver_index(list(
        zakresolvetarget = list(Imports = "zakresolvedirect (>= 9.9.9)"),
        zakresolvedirect = list()
      )),
      package = "zakresolvetarget"
    ),
    non_ge_operators = list(
      index = resolver_index(list(
        zakresolvetarget = list(
          Imports = "zakresolvedirect (== 2.0.0), zakresolvea (< 0.1)"
        ),
        zakresolvedirect = list(),
        zakresolvea = list()
      )),
      package = "zakresolvetarget"
    ),
    missing_dependency = list(
      index = resolver_index(list(
        zakresolvetarget = list(Imports = "zakresolveabsent"),
        zakresolvea = list()
      )),
      package = "zakresolvetarget"
    ),
    missing_target = list(
      index = resolver_index(list(zakresolvea = list())),
      package = "zakresolveabsenttarget"
    ),
    installed_and_satisfied = list(
      index = resolver_index(list(
        zakresolvetarget = list(Imports = "digest (>= 0.0.1)"),
        digest = list()
      )),
      package = "zakresolvetarget"
    ),
    # `digest` is installed, so these reach the branch where R enforces only
    # `>=` against an installed version and treats any other operator as
    # satisfied. Without an installed package the branch is unreachable.
    installed_non_ge_operator = list(
      index = resolver_index(list(
        zakresolvetarget = list(Imports = "digest (== 9999.0.0)"),
        digest = list()
      )),
      package = "zakresolvetarget"
    ),
    installed_lt_operator = list(
      index = resolver_index(list(
        zakresolvetarget = list(Imports = "digest (< 0.0.1)"),
        digest = list()
      )),
      package = "zakresolvetarget"
    ),
    installed_but_too_old = list(
      index = resolver_index(list(
        zakresolvetarget = list(Imports = "digest (>= 9999.0.0)"),
        digest = list()
      )),
      package = "zakresolvetarget"
    ),
    # The same package declared more than once, within one field and across
    # fields: R keeps the first declaration only.
    duplicate_declarations = list(
      index = resolver_index(list(
        zakresolvetarget = list(
          Imports = "zakresolvea (>= 1.0.0), zakresolvea",
          LinkingTo = "zakresolvea"
        ),
        zakresolvea = list()
      )),
      package = "zakresolvetarget"
    ),
    duplicate_unsatisfiable_first = list(
      index = resolver_index(list(
        zakresolvetarget = list(
          Imports = "zakresolvea (>= 9.9.9), zakresolvea (>= 1.0.0)"
        ),
        zakresolvea = list()
      )),
      package = "zakresolvetarget"
    ),
    no_dependencies = list(
      index = resolver_index(list(zakresolvetarget = list())),
      package = "zakresolvetarget"
    )
  )

  for (name in names(scenarios)) {
    scenario <- scenarios[[name]]
    mine <- resolver_outcome(
      zak:::resolve_dependencies(scenario$package, scenario$index, library)
    )
    theirs <- resolver_outcome(reference(
      pkgs = scenario$package,
      dependencies = NA,
      available = scenario$index,
      lib = library
    ))
    expect_identical(mine$value, theirs$value, info = name)
    expect_identical(mine$warned, theirs$warned, info = name)
    expect_identical(mine$messaged, theirs$messaged, info = name)
  }
})

test_that("the base resolver expands transitive dependencies in install order", {
  resolution <- zak:::resolve_dependencies(
    "zaktargetresolver",
    resolver_fixture_index(),
    .libPaths()[[1L]]
  )

  expect_identical(
    resolution,
    c("zakleafresolver", "zakdirectresolver", "zaktargetresolver")
  )
})

test_that("unavailable dependencies retain resolver warning behavior", {
  index <- resolver_fixture_index()["zaktargetresolver", , drop = FALSE]
  expect_warning(
    resolution <- zak:::resolve_dependencies(
      "zaktargetresolver",
      index,
      .libPaths()[[1L]]
    ),
    "not available"
  )
  expect_identical(resolution, "zaktargetresolver")
})

test_that("resolver version behavior remains aligned with base R", {
  index <- resolver_fixture_index()[
    c("zaktargetresolver", "zakdirectresolver"),
    ,
    drop = FALSE
  ]
  index["zakdirectresolver", "Imports"] <- NA_character_
  index["zaktargetresolver", "Imports"] <- "zakdirectresolver (>= 2.0.0)"
  expect_warning(
    zak:::resolve_dependencies(
      "zaktargetresolver",
      index,
      .libPaths()[[1L]]
    ),
    "not available"
  )

  for (operator in c(">", "==", "<=", "<")) {
    index["zaktargetresolver", "Imports"] <- sprintf(
      "zakdirectresolver (%s 2.0.0)",
      operator
    )
    observed <- zak:::resolve_dependencies(
      "zaktargetresolver",
      index,
      .libPaths()[[1L]]
    )
    expect_true("zakdirectresolver" %in% observed)
  }
})

test_that("strong fields are included and optional fields are excluded", {
  fields <- c(
    "Package",
    "Version",
    "Depends",
    "Imports",
    "LinkingTo",
    "Suggests",
    "Enhances",
    "Repository"
  )
  index <- matrix(
    NA_character_,
    nrow = 1L,
    ncol = length(fields),
    dimnames = list("zakfieldtarget", fields)
  )
  index[, "Package"] <- "zakfieldtarget"
  index[, "Version"] <- "1.0.0"
  index[, "Depends"] <- "R (>= 4.1.0), zakdepends"
  index[, "Imports"] <- "zakimports"
  index[, "LinkingTo"] <- "zaklinkingto"
  index[, "Suggests"] <- "zaksuggests"
  index[, "Enhances"] <- "zakenhances"

  expect_setequal(
    zak:::direct_dependencies("zakfieldtarget", index),
    c("zakdepends", "zakimports", "zaklinkingto")
  )
})

test_that("dependency reporting includes selected versions and constraints", {
  index <- resolver_fixture_index()
  dependencies <- c("zakleafresolver", "zakdirectresolver")
  constraints <- zak:::dependency_constraints(
    "zaktargetresolver",
    dependencies,
    index
  )

  expect_length(constraints$zakleafresolver, 0L)
  expect_identical(constraints$zakdirectresolver, ">= 1.0.0")
  expect_message(
    zak:::report_dependencies("zaktargetresolver", dependencies, index),
    "zakleafresolver 1.0.0 \\[no version constraint\\]"
  )
  expect_message(
    zak:::report_dependencies("zaktargetresolver", character(), index),
    "Dependencies: none need installation"
  )

  missing_index <- index["zaktargetresolver", , drop = FALSE]
  expect_message(
    zak:::report_dependencies("zaktargetresolver", character(), missing_index),
    "zakdirectresolver unavailable \\[requires >= 1.0.0\\]"
  )
})
