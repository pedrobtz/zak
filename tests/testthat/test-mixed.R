test_that("Remotes replace repository candidates for direct dependencies", {
  remote <- make_fixture_package("zakfixtureremotedep")
  remote_git <- make_fixture_git_repository(remote)
  target <- make_fixture_package(
    "zakfixturemixedtarget",
    fields = list(
      Imports = remote$package,
      Remotes = paste0("git::", remote_git$url, "@", remote_git$commit)
    )
  )
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(c(remote$root, target$root, library), recursive = TRUE),
    add = TRUE
  )

  result <- suppressMessages(zak::plan(target$source, lib = library))

  expect_identical(result$status, "ready")
  expect_identical(result$install_dependencies, remote$package)
  expect_identical(
    result$candidates$dependencies[[remote$package]]$source$type,
    "git"
  )
  expect_identical(
    result$candidates$dependencies[[remote$package]]$source$ref,
    remote_git$commit
  )
  expect_identical(
    result$candidates$dependencies[[remote$package]]$provenance$commit,
    remote_git$commit
  )
  expect_identical(
    result$dependency_sources[[remote$package]]$source$type,
    "git"
  )
  installed <- utils::installed.packages(lib.loc = library)
  expect_false(remote$package %in% installed[, "Package"])
})

test_that("mixed repository and Git dependencies share one graph", {
  repository_dependency <- make_fixture_package("zakfixturemixedrepo")
  remote <- make_fixture_package(
    "zakfixtureremotegraph",
    fields = list(Imports = repository_dependency$package)
  )
  remote_git <- make_fixture_git_repository(remote)
  target <- make_fixture_package(
    "zakfixturemixedinstall",
    fields = list(
      Imports = remote$package,
      Remotes = paste0("git::", remote_git$url, "@", remote_git$commit)
    )
  )
  repository <- make_fixture_repository(list(repository_dependency))
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(
      c(
        repository_dependency$root,
        remote$root,
        target$root,
        repository$root,
        library
      ),
      recursive = TRUE
    ),
    add = TRUE
  )

  with_fixture_options(repository$url, {
    result <- suppressMessages(zak::plan(target$source, lib = library))
    expect_identical(result$status, "ready")
    expect_true(all(
      c(remote$package, repository_dependency$package) %in%
        result$install_dependencies
    ))
    expect_identical(
      result$candidates$dependencies[[remote$package]]$source$type,
      "git"
    )
  })
})

test_that("Remotes can override a transitive repository dependency", {
  remote <- make_fixture_package("zakfixturetransitiveremote")
  remote_git <- make_fixture_git_repository(remote)
  repository_parent <- make_fixture_package(
    "zakfixturetransitiveparent",
    fields = list(Imports = remote$package)
  )
  target <- make_fixture_package(
    "zakfixturetransitivetarget",
    fields = list(
      Imports = repository_parent$package,
      Remotes = paste0("git::", remote_git$url, "@", remote_git$commit)
    )
  )
  repository <- make_fixture_repository(list(repository_parent))
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(
      c(
        remote$root,
        repository_parent$root,
        target$root,
        repository$root,
        library
      ),
      recursive = TRUE
    ),
    add = TRUE
  )

  with_fixture_options(repository$url, {
    result <- suppressMessages(zak::plan(target$source, lib = library))
  })

  expect_identical(result$status, "ready")
  expect_true(all(
    c(repository_parent$package, remote$package) %in%
      result$install_dependencies
  ))
  expect_identical(
    result$candidates$dependencies[[remote$package]]$source$type,
    "git"
  )
})

test_that("mixed repository and Git dependencies install transactionally", {
  repository_dependency <- make_fixture_package("zakfixturemixedrepo")
  remote <- make_fixture_package(
    "zakfixtureremotegraph",
    fields = list(Imports = repository_dependency$package)
  )
  remote_git <- make_fixture_git_repository(remote)
  target <- make_fixture_package(
    "zakfixturemixedinstall",
    fields = list(
      Imports = remote$package,
      Remotes = paste0("git::", remote_git$url, "@", remote_git$commit)
    )
  )
  repository <- make_fixture_repository(list(repository_dependency))
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(
      c(
        repository_dependency$root,
        remote$root,
        target$root,
        repository$root,
        library
      ),
      recursive = TRUE
    ),
    add = TRUE
  )

  with_fixture_options(repository$url, {
    expect_null(
      suppressMessages(zak::install(target$source, lib = library))
    )
  })

  installed <- utils::installed.packages(lib.loc = library)
  expect_true(all(
    c(target$package, remote$package, repository_dependency$package) %in%
      installed[, "Package"]
  ))
})

test_that("duplicate Remotes mappings fail before source acquisition", {
  expect_snapshot(error = TRUE, {
    zak:::remote_source_map(zak:::parse_remotes(c(
      Remotes = "github::owner/zakfixturedup, github::other/zakfixturedup"
    )))
  })
})

test_that("ambiguous remote identities fail before source acquisition", {
  expect_snapshot(error = TRUE, {
    zak:::remote_source_map(zak:::parse_remotes(c(
      Remotes = "url::https://example.test/123-download"
    )))
  })
})

test_that("remote preparation errors name the dependency and source", {
  testthat::local_mocked_bindings(
    prepare_package_source = function(...) {
      stop("fixture source unavailable", call. = FALSE)
    },
    .package = "zak"
  )

  expect_snapshot(error = TRUE, {
    zak:::prepare_remote_source_record(
      "zakfixtureunavailable",
      list(
        type = "url",
        url = "https://example.test/zakfixtureunavailable_1.0.0.tar.gz"
      ),
      lib = NULL,
      platform = zak:::current_platform_facts(),
      verbose = FALSE
    )
  })
})

test_that("conflicting nested remote declarations name both parents", {
  shared <- make_fixture_package("zakfixtureconflictshared")
  first <- make_fixture_package(
    "zakfixtureconflictfirst",
    fields = list(
      Imports = shared$package,
      Remotes = paste0(
        "url::https://first.test/",
        shared$package,
        "_1.0.0.tar.gz"
      )
    )
  )
  second <- make_fixture_package(
    "zakfixtureconflictsecond",
    fields = list(
      Imports = shared$package,
      Remotes = paste0(
        "url::https://second.test/",
        shared$package,
        "_1.0.0.tar.gz"
      )
    )
  )
  target <- make_fixture_package(
    "zakfixtureconflicttarget",
    fields = list(
      Imports = paste(first$package, second$package, sep = ", "),
      Remotes = paste(
        paste0("url::https://root.test/", first$package, "_1.0.0.tar.gz"),
        paste0("url::https://root.test/", second$package, "_1.0.0.tar.gz"),
        sep = ", "
      )
    )
  )
  fixtures <- list(shared, first, second)
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(
      c(shared$root, first$root, second$root, target$root, library),
      recursive = TRUE
    ),
    add = TRUE
  )
  testthat::local_mocked_bindings(
    acquire_archive = function(url) {
      matches <- vapply(
        fixtures,
        function(fixture) {
          grepl(paste0("/", fixture$package, "_"), url, fixed = TRUE)
        },
        logical(1L)
      )
      fixture <- fixtures[[which(matches)]]
      artifact <- acquire_local_archive(fixture$archive)
      artifact$url <- url
      artifact
    },
    .package = "zak"
  )

  expect_snapshot(error = TRUE, {
    suppressMessages(zak::plan(target$source, lib = library))
  })
})

test_that("URL remotes provide source-specific dependency candidates", {
  remote <- make_fixture_package("zakfixtureurlremote")
  target <- make_fixture_package(
    "zakfixtureurltarget",
    fields = list(
      Imports = remote$package,
      Remotes = paste0(
        "url::https://example.test/",
        remote$package,
        "_1.0.0.tar.gz"
      )
    )
  )
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(c(remote$root, target$root, library), recursive = TRUE),
    add = TRUE
  )
  testthat::local_mocked_bindings(
    acquire_archive = function(url) {
      artifact <- acquire_local_archive(remote$archive)
      artifact$url <- url
      artifact
    },
    .package = "zak"
  )

  result <- suppressMessages(zak::plan(target$source, lib = library))

  expect_identical(
    result$candidates$dependencies[[remote$package]]$source$type,
    "url"
  )
  expect_identical(
    result$candidates$dependencies[[remote$package]]$provenance$acquired_from,
    paste0(
      "https://example.test/",
      remote$package,
      "_1.0.0.tar.gz"
    )
  )
})

test_that("Bioconductor remotes install as mixed-source dependencies", {
  remote <- make_fixture_package("zakfixturebiocremote")
  repository <- make_fixture_repository(list(remote))
  target <- make_fixture_package(
    "zakfixturebiocmixedtarget",
    fields = list(
      Imports = remote$package,
      Remotes = paste0("bioc::", remote$package)
    )
  )
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(
      c(remote$root, repository$root, target$root, library),
      recursive = TRUE
    ),
    add = TRUE
  )
  testthat::local_mocked_bindings(
    bioc_repositories = function() c(BIOC = repository$url),
    .package = "zak"
  )

  with_fixture_options(repository$url, {
    expect_null(
      suppressMessages(zak::install(target$source, lib = library))
    )
  })

  installed <- utils::installed.packages(lib.loc = library)
  expect_true(all(
    c(target$package, remote$package) %in% installed[, "Package"]
  ))
})
