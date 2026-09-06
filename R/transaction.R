installation_library <- function(lib) {
  if (is.null(lib)) {
    libraries <- .libPaths()
    if (!length(libraries)) {
      stop("R did not provide an installation library.", call. = FALSE)
    }
    lib <- libraries[[1L]]
  }
  if (!is.character(lib) || length(lib) != 1L || is.na(lib) || !nzchar(lib)) {
    stop("`lib` must be a single installation library path.", call. = FALSE)
  }
  lib <- path.expand(lib)
  if (!dir.exists(lib)) {
    stop("The installation library does not exist.", call. = FALSE)
  }
  normalizePath(lib, mustWork = TRUE)
}

create_staging_library <- function(lib) {
  staging <- tempfile("zak-staging-", tmpdir = dirname(lib))
  if (!dir.create(staging, recursive = FALSE)) {
    stop("R could not create a staging library.", call. = FALSE)
  }
  normalizePath(staging, mustWork = TRUE)
}

cleanup_staging_library <- function(staging) {
  if (!is.null(staging)) {
    unlink(staging, recursive = TRUE, force = TRUE)
  }
  invisible()
}

project_install_from_locks <- function(project_lock, lib, verbose) {
  entries <- project_lock_install_entries(project_lock)
  if (!length(entries$packages)) {
    report_step(verbose, "Project lockfile contains no packages to install.")
    return(NULL)
  }

  lib <- installation_library(lib)
  staging <- create_staging_library(lib)
  on.exit(cleanup_staging_library(staging), add = TRUE)
  report_step(
    verbose,
    sprintf(
      "Installing %d project package(s) in one staged transaction.",
      length(entries$packages)
    )
  )
  install_project_packages(
    entries$packages,
    entries$repositories,
    staging,
    entries$sources,
    entries$platform,
    verbose
  )
  problems <- verify_project_staged_packages(entries$versions, staging)
  if (length(problems)) {
    stop(staged_installation_error(problems), call. = FALSE)
  }
  commit_staged_packages(staging, lib, entries$packages)
  invisible(NULL)
}

project_lock_install_entries <- function(project_lock) {
  packages <- character()
  versions <- character()
  sources <- list()
  plans <- project_lock$plans
  repositories <- restore_repositories(plans[[1L]]$repositories)
  platform <- plans[[1L]]$platform

  add_version <- function(package, version) {
    if (package %in% names(versions)) {
      if (!identical(unname(versions[[package]]), version)) {
        stop(
          sprintf(
            "Project lockfile selects conflicting versions for `%s`.",
            package
          ),
          call. = FALSE
        )
      }
      return()
    }
    versions[[package]] <<- version
    packages <<- c(packages, package)
  }

  add_source <- function(package, source) {
    if (is.null(sources[[package]])) {
      sources[[package]] <<- source
      return()
    }
    if (!identical(sources[[package]], source)) {
      stop(
        sprintf(
          "Project lockfile selects conflicting sources for `%s`.",
          package
        ),
        call. = FALSE
      )
    }
  }

  for (package_lock in plans) {
    dependency_rows <- package_lock$dependencies
    dependency_rows <- dependency_rows[
      dependency_rows$Status == "install",
      ,
      drop = FALSE
    ]
    if (nrow(dependency_rows)) {
      for (row in seq_len(nrow(dependency_rows))) {
        package <- dependency_rows$Package[[row]]
        add_version(package, dependency_rows$Version[[row]])
      }
    }
    add_version(package_lock$target$package, package_lock$target$version)

    dependency_sources <- package_lock$dependency_sources
    if (length(dependency_sources)) {
      for (package in names(dependency_sources)) {
        add_source(
          package,
          project_dependency_source_record(dependency_sources[[package]])
        )
      }
    }
    if (!identical(package_lock$source$type, "repository")) {
      add_source(
        package_lock$target$package,
        project_target_source_record(package_lock)
      )
    }
  }

  list(
    packages = packages,
    versions = versions,
    sources = sources,
    repositories = repositories,
    platform = platform
  )
}

install_project_packages <- function(
  packages,
  repositories,
  lib,
  sources,
  platform,
  verbose
) {
  result <- NULL
  for (package in packages) {
    source <- sources[[package]]
    if (is.null(source)) {
      result <- install_repository_package(
        package,
        repositories,
        lib,
        dependencies = FALSE
      )
    } else {
      result <- install_source_dependency(
        package,
        source,
        lib,
        platform,
        verbose
      )
    }
  }
  invisible(result)
}

verify_project_staged_packages <- function(versions, staging) {
  staged_package_problems(staging, versions)
}

# `restore_*_reference()` produce reference text. `install_source_dependency()`
# needs a parsed reference, so parse here: that keeps its contract single and
# reports a bad reference while reading the lockfile rather than part-way
# through a staged install.
project_target_source_record <- function(package_lock) {
  list(
    source = parse_package_reference(restore_reference(package_lock)),
    acquisition = package_lock$acquisition
  )
}

project_dependency_source_record <- function(source) {
  list(
    source = parse_package_reference(restore_source_reference(source)),
    acquisition = list(
      commit = source$commit,
      sha256 = source$sha256
    )
  )
}

restore_source_reference <- function(source) {
  switch(
    source$type,
    url = source$url,
    local = source$path,
    local_archive = source$path,
    git = paste0(
      "git::",
      source$url,
      "@",
      if (is.null(source$commit)) source$ref else source$commit
    ),
    bioconductor = paste0("bioc::", source$package),
    repository = source$package,
    stop(
      sprintf("Unsupported project source type: %s.", source$type),
      call. = FALSE
    )
  )
}

# `utils::installed.packages()` reports only packages that have a
# `Meta/package.rds`, which a binary install does not write. Reading the
# staged `DESCRIPTION` directly reports what is actually there.
staged_package_version <- function(staging, package) {
  description <- file.path(staging, package, "DESCRIPTION")
  if (!file.exists(description)) {
    return(NA_character_)
  }
  fields <- tryCatch(read.dcf(description), error = function(error) NULL)
  if (is.null(fields) || !nrow(fields) || !"Version" %in% colnames(fields)) {
    return(NA_character_)
  }
  version <- unname(fields[1L, "Version"])
  if (is.na(version) || !nzchar(trimws(version))) {
    return(NA_character_)
  }
  trimws(version)
}

# `expected` is a named character vector of package -> required version, where
# `NA_character_` accepts any version. Returns one message per problem so the
# caller can report every missing package at once.
staged_package_problems <- function(staging, expected) {
  problems <- character()
  for (package in names(expected)) {
    staged <- staged_package_version(staging, package)
    if (is.na(staged)) {
      problems <- c(
        problems,
        sprintf("%s was not installed", sQuote(package))
      )
      next
    }
    required <- unname(expected[[package]])
    if (!is.na(required) && !identical(staged, required)) {
      problems <- c(
        problems,
        sprintf(
          "%s installed version %s but %s was selected",
          sQuote(package),
          staged,
          required
        )
      )
    }
  }
  problems
}

staged_installation_error <- function(problems) {
  paste(
    c(
      "Zak did not commit the installation because the staged library was incomplete:",
      paste0("- ", problems)
    ),
    collapse = "\n"
  )
}

verify_staged_packages <- function(plan, staging) {
  dependencies <- setdiff(plan$install_dependencies, plan$target$package)
  expected <- stats::setNames(
    c(
      rep(NA_character_, length(dependencies)),
      plan$target$version
    ),
    c(dependencies, plan$target$package)
  )
  staged_package_problems(staging, expected)
}

commit_staged_packages <- function(staging, lib, packages) {
  backup <- tempfile("zak-backup-", tmpdir = dirname(lib))
  if (!dir.create(backup, recursive = FALSE)) {
    stop("R could not create an installation backup area.", call. = FALSE)
  }

  committed <- character()
  backups <- character()
  complete <- FALSE
  on.exit(
    {
      unrestored <- if (complete) {
        character()
      } else {
        rollback_staged_packages(staging, lib, backup, committed, backups)
      }
      # Only discard the backup once every previous version is back in the
      # library. Deleting it after a failed restore would destroy the user's
      # only remaining copy.
      if (length(unrestored)) {
        warning(
          sprintf(
            paste0(
              "Zak could not restore %s after a failed installation. ",
              "The previous version(s) are kept in %s."
            ),
            paste(sQuote(unrestored), collapse = ", "),
            backup
          ),
          call. = FALSE
        )
      } else {
        unlink(backup, recursive = TRUE, force = TRUE)
      }
    },
    add = TRUE
  )

  for (package in packages) {
    source <- file.path(staging, package)
    destination <- file.path(lib, package)
    if (!dir.exists(source)) {
      stop(
        sprintf("The staged package %s is missing.", sQuote(package)),
        call. = FALSE
      )
    }

    if (file.exists(destination)) {
      backup_path <- file.path(backup, package)
      if (!file.rename(destination, backup_path)) {
        stop(
          sprintf(
            "R could not prepare the existing package %s.",
            sQuote(package)
          ),
          call. = FALSE
        )
      }
      backups <- c(backups, package)
    }

    if (!move_directory(source, destination)) {
      stop(
        sprintf("R could not commit the staged package %s.", sQuote(package)),
        call. = FALSE
      )
    }
    committed <- c(committed, package)
  }

  complete <- TRUE
  invisible()
}

move_directory <- function(source, destination) {
  if (file.rename(source, destination)) {
    return(TRUE)
  }

  created <- dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  if (!created && !dir.exists(destination)) {
    return(FALSE)
  }
  entries <- list.files(
    source,
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )
  copied <- if (length(entries)) {
    vapply(
      entries,
      function(entry) {
        file.copy(
          entry,
          file.path(destination, basename(entry)),
          recursive = TRUE
        )
      },
      logical(1L)
    )
  } else {
    logical()
  }
  if (length(copied) && !all(copied)) {
    unlink(destination, recursive = TRUE, force = TRUE)
    return(FALSE)
  }
  unlink(source, recursive = TRUE, force = TRUE)
  TRUE
}

rollback_staged_packages <- function(
  staging,
  lib,
  backup,
  committed,
  backups
) {
  affected <- rev(unique(c(committed, backups)))
  for (package in affected) {
    unlink(file.path(lib, package), recursive = TRUE, force = TRUE)
  }
  # Report which previous versions could not be put back so the caller can
  # keep their backups instead of deleting them.
  unrestored <- character()
  for (package in rev(backups)) {
    source <- file.path(backup, package)
    if (!file.exists(source)) {
      next
    }
    if (!file.rename(source, file.path(lib, package))) {
      unrestored <- c(unrestored, package)
    }
  }
  unrestored
}
