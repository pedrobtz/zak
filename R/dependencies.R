# Dependency declaration parsing and resolution.
#
# These reproduce the behavior of `utils::install.packages()`'s internal
# dependency resolution, which zak previously reached into. They are kept
# deliberately faithful to that behavior, including its quirks, so that a zak
# plan and a plain `install.packages()` call still select the same packages.
# `tests/testthat/test-resolver.R` pins the behavior that matters.

# Parses one declaration such as `jsonlite (>= 1.8.0)` into its name and, when
# present, its comparison operator and version. Mirrors the parsing R applies
# to `Depends`, `Imports` and `LinkingTo` fields.
split_dependency_requirement <- function(value) {
  pattern <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
  name <- sub(pattern, "\\1", value)
  constraint <- sub(pattern, "\\2", value)
  if (identical(constraint, name)) {
    return(list(name = name))
  }
  operator_pattern <- "[[:space:]]*([[<>=!]+)[[:space:]]+(.*)"
  version <- sub(operator_pattern, "\\2", constraint)
  # An "r12345" style SVN revision is not a package version.
  if (!startsWith(version, "r")) {
    version <- package_version(version)
  }
  list(
    name = name,
    op = sub(operator_pattern, "\\1", constraint),
    version = version
  )
}

# Splits a whole dependency field into one requirement per declaration, named
# by package. Entries are kept as declared, including `R`, because callers that
# report constraints need to see it.
split_dependency_field <- function(value) {
  if (!length(value)) {
    return(list())
  }
  value <- unlist(strsplit(value, ","))
  value <- sub("[[:space:]]+$", "", value)
  value <- unique(sub("^[[:space:]]*(.*)", "\\1", value))
  names(value) <- sub("^([[:alnum:].]+).*$", "\\1", value)
  lapply(value, split_dependency_requirement)
}

# The variant R uses when deciding what to install: `R` is dropped, empty
# entries are dropped, and a package declared twice is taken once.
split_installable_dependencies <- function(declarations) {
  if (!any(nzchar(declarations))) {
    return(list())
  }
  unlist(
    lapply(
      strsplit(declarations, ","),
      function(value) {
        value <- sub("[[:space:]]+$", "", value)
        value <- unique(sub("^[[:space:]]*(.*)", "\\1", value))
        names(value) <- sub("^([[:alnum:].]+).*$", "\\1", value)
        value <- value[names(value) != "R"]
        value <- value[nzchar(value)]
        value <- value[!duplicated(names(value))]
        lapply(value, split_dependency_requirement)
      }
    ),
    FALSE,
    FALSE
  )
}

requirement_is_satisfied <- function(requirement, packages, versions) {
  if (length(requirement) < 3L) {
    return(requirement$name %in% packages)
  }
  if (!requirement$name %in% packages) {
    return(FALSE)
  }
  # R only enforces `>=` against what is installed; any other operator is
  # treated as satisfied. Reproduced deliberately so zak and
  # `install.packages()` agree.
  if (!identical(requirement$op, ">=")) {
    return(TRUE)
  }
  current <- as.package_version(versions[packages == requirement$name])
  target <- as.package_version(requirement$version)
  isTRUE(any(do.call(requirement$op, list(current, target))))
}

# Splits declared dependencies into those that must be installed and those no
# repository can supply.
required_dependencies <- function(declarations, installed, available) {
  empty <- list(required = character(), unavailable = character())
  declarations <- declarations[!is.na(declarations)]
  if (!length(declarations)) {
    return(empty)
  }
  requirements <- split_installable_dependencies(declarations)
  if (!length(requirements)) {
    return(empty)
  }

  installed_packages <- installed[, "Package"]
  installed_versions <- installed[, "Version"]
  satisfied <- vapply(
    requirements,
    requirement_is_satisfied,
    logical(1L),
    packages = installed_packages,
    versions = installed_versions
  )
  requirements <- requirements[!satisfied]
  if (!length(requirements)) {
    return(empty)
  }

  available_packages <- rownames(available)
  available_versions <- available[, "Version"]
  required <- character()
  unavailable <- character()
  for (requirement in requirements) {
    if (!requirement$name %in% available_packages) {
      unavailable <- c(unavailable, requirement$name)
      next
    }
    if (length(requirement) < 3L || !identical(requirement$op, ">=")) {
      required <- c(required, requirement$name)
      next
    }
    current <- as.package_version(
      available_versions[available_packages == requirement$name]
    )
    target <- as.package_version(requirement$version)
    if (isTRUE(any(do.call(requirement$op, list(current, target))))) {
      required <- c(required, requirement$name)
    } else {
      unavailable <- c(
        unavailable,
        paste0(requirement$name, " (>= ", requirement$version, ")")
      )
    }
  }
  list(required = required, unavailable = unavailable)
}

is_base_package <- function(package) {
  priority <- tryCatch(
    utils::packageDescription(package, fields = "Priority", encoding = NA),
    error = function(error) NULL,
    warning = function(warning) NULL
  )
  identical(priority, "base")
}

dependency_resolution_libraries <- function(lib) {
  libraries <- .libPaths()
  if (is.null(lib)) {
    return(libraries)
  }
  if (lib %in% libraries) {
    return(libraries)
  }
  c(lib, libraries)
}
