#' Restore packages from a zak lockfile
#'
#' `restore()` reads a validated zak lockfile, reconstructs its recorded
#' package source, and rebuilds the installation plan before changing a
#' library. The rebuilt plan must match the lockfile's stable source,
#' repository, platform, dependency, and artifact decisions. Acquired
#' archives and Git sources must have the SHA-256 checksums recorded by the
#' lockfile; the existing staged installer then verifies those artifacts again
#' before installation.
#'
#' Repository package versions are restored only while the current repository
#' metadata still matches the lockfile. Historical repository snapshots and
#' binary selection remain future extensions of the lock schema. Project
#' lockfiles aggregate their embedded package plans and commit all selected
#' packages in one staged transaction after every plan matches.
#'
#' Set `options(zak.offline = TRUE)` to restore only from valid cached
#' HTTP(S) archives and commit-resolved Git sources.
#'
#' @param file A single path to a zak JSON lockfile.
#' @param lib `NULL`, or a single character path to the installation library.
#'   The default leaves library selection to `utils::install.packages()`.
#' @param verbose A single logical value. When `TRUE`, report each restore and
#'   installation step.
#'
#' @return The return value of the target installation command for a package
#'   lock, invisibly. Project lock restoration returns `NULL` invisibly.
#' @seealso [lock()], [read_lock()], [install()]
#' @export
#'
#' @examples
#' \dontrun{
#' zak::restore("zak.lock")
#' }
restore <- function(file = "zak.lock", lib = NULL, verbose = FALSE) {
  validate_verbose(verbose)
  package_lock <- read_lock(file)
  if (inherits(package_lock, "zak_project_lock")) {
    return(invisible(restore_project_lock(package_lock, lib, verbose)))
  }
  validate_restore_lock(package_lock)

  reference <- restore_reference(package_lock)
  old_repositories <- options(
    repos = restore_repositories(package_lock$repositories)
  )
  on.exit(options(old_repositories), add = TRUE)

  built <- build_install_plan(reference, lib, verbose)
  on.exit(cleanup_plan_artifact(built$artifact), add = TRUE)

  comparison <- compare_lock(package_lock, built$plan)
  if (!identical(comparison$status, "current")) {
    stop(restore_drift_message(comparison), call. = FALSE)
  }

  plan <- built$plan
  report_dependencies(
    plan$archive_metadata$package,
    plan$install_dependencies,
    plan$available
  )
  invisible(install_from_plan(plan, built$artifact, verbose))
}

validate_restore_lock <- function(package_lock) {
  if (inherits(package_lock, "zak_project_lock")) {
    return(validate_project_restore_lock(package_lock))
  }
  source <- package_lock$source
  # Every recorded URL is redacted on the way in, so every recorded URL has to
  # be checked on the way out, not just the target's.
  redacted <- c(
    if (source$type %in% c("url", "git")) source$url,
    package_lock$acquisition$url,
    unlist(lapply(package_lock$dependency_sources, `[[`, "url"))
  )
  if (any(grepl("<redacted>", redacted, fixed = TRUE))) {
    stop(
      "Zak cannot restore a source URL containing redacted credentials or query parameters.",
      call. = FALSE
    )
  }
  if (!is.null(package_lock$acquisition)) {
    if (is.null(package_lock$acquisition$sha256)) {
      stop(
        "Zak cannot restore an acquired source without its SHA-256 checksum.",
        call. = FALSE
      )
    }
  }
  sources <- package_lock$dependency_sources
  if (length(sources)) {
    missing <- names(sources)[vapply(
      sources,
      function(source) {
        source$type %in% c("url", "git") && is.null(source$sha256)
      },
      logical(1L)
    )]
    if (length(missing)) {
      stop(
        sprintf(
          paste0(
            "Zak cannot restore remote dependencies without SHA-256 checksums: %s."
          ),
          paste(missing, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  invisible()
}

validate_project_restore_lock <- function(project_lock) {
  if (!inherits(project_lock, "zak_project_lock")) {
    stop("`project_lock` must be a zak project lock object.", call. = FALSE)
  }
  vapply(
    project_lock$plans,
    function(package_lock) {
      validate_restore_lock(package_lock)
      TRUE
    },
    logical(1L)
  )
  invisible()
}

restore_project_lock <- function(project_lock, lib, verbose) {
  validate_project_restore_lock(project_lock)
  lib <- project_restore_library(project_lock, lib)
  references <- project_lock$references
  plans <- project_lock$plans
  if (!length(references)) {
    report_step(verbose, "Project lockfile contains no package references.")
    return(NULL)
  }

  repositories <- restore_repositories(plans[[1L]]$repositories)
  old_repositories <- options(repos = repositories)
  on.exit(options(old_repositories), add = TRUE)
  current <- lapply(
    references,
    function(reference) plan(reference$reference, lib = lib, verbose = verbose)
  )
  comparisons <- Map(compare_lock, plans, current)
  drifted <- which(vapply(
    comparisons,
    function(comparison) !identical(comparison$status, "current"),
    logical(1L)
  ))
  if (length(drifted)) {
    stop(
      project_restore_drift_message(
        comparisons[drifted],
        references[drifted]
      ),
      call. = FALSE
    )
  }

  project_install_from_locks(project_lock, lib, verbose)
}

project_restore_library <- function(project_lock, lib) {
  if (!is.null(lib)) {
    return(lib)
  }
  library <- project_lock$project$library
  if (is.null(library)) {
    return(NULL)
  }
  project <- dirname(project_lock$file)
  library <- project_file_path(project, library)
  create_project_library(library)
  library
}

project_restore_drift_message <- function(comparisons, references) {
  details <- vapply(
    seq_along(comparisons),
    function(index) {
      comparison <- comparisons[[index]]
      changes <- comparison$changes
      change_text <- apply(
        changes,
        1L,
        function(change) {
          field <- change[["Field"]]
          if (field %in% c("acquisition", "dependency_sources")) {
            return(sprintf("%s drift", field))
          }
          sprintf(
            "%s changed (lock=%s; plan=%s)",
            field,
            change[["Lock"]],
            change[["Plan"]]
          )
        }
      )
      sprintf(
        "- %s: %s",
        references[[index]]$package,
        paste(change_text, collapse = "; ")
      )
    },
    character(1L)
  )
  paste(
    c("Zak project lockfile drift prevents restore:", details),
    collapse = "\n"
  )
}

restore_reference <- function(package_lock) {
  source <- package_lock$source
  switch(
    source$type,
    url = source$url,
    local = source$path,
    local_archive = source$path,
    git = paste0(
      "git::",
      source$url,
      "@",
      if (is.null(package_lock$acquisition$commit)) {
        source$ref
      } else {
        package_lock$acquisition$commit
      }
    ),
    bioconductor = paste0("bioc::", source$package),
    repository = source$package,
    stop(
      sprintf("Unsupported zak restore source type: %s.", source$type),
      call. = FALSE
    )
  )
}

restore_repositories <- function(repositories) {
  if (!length(repositories)) {
    return(getOption("repos"))
  }
  repositories <- unlist(repositories, use.names = TRUE)
  if (!length(repositories)) {
    return(getOption("repos"))
  }
  repositories
}

restore_drift_message <- function(comparison) {
  changes <- comparison$changes
  details <- apply(
    changes,
    1L,
    function(change) {
      field <- change[["Field"]]
      if (field %in% c("acquisition", "dependency_sources")) {
        return(
          sprintf(
            "- %s: locked provenance does not match the current plan.",
            field
          )
        )
      }
      sprintf(
        "- %s: lock=%s; plan=%s",
        field,
        change[["Lock"]],
        change[["Plan"]]
      )
    }
  )
  paste(
    c("Zak lockfile drift prevents restore:", details),
    collapse = "\n"
  )
}
