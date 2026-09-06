# Internal state is process-local and is intentionally not persisted in the
# project metadata or lockfile.
zak_project_state <- new.env(parent = emptyenv())

#' Discover dependencies declared or used by a project
#'
#' `dependencies()` performs a static scan of a project. Package metadata is
#' read from `DESCRIPTION` when present, and R, R Markdown, Quarto, and Sweave
#' files are inspected for namespace operators and common package-loading
#' calls. The result is evidence-oriented so callers can see why a package was
#' discovered.
#'
#' The scan is intentionally static. It does not evaluate code, follow
#' computed package names, inspect installed libraries, or infer dependencies
#' from arbitrary strings. Directories commonly used for generated or managed
#' dependencies, including `.git`, `.zak`, `renv`, and `packrat`, are skipped.
#'
#' @param path A single project directory.
#' @param include_suggests A single logical value. When `TRUE`, include
#'   `Suggests` from `DESCRIPTION`.
#' @param include_enhances A single logical value. When `TRUE`, include
#'   `Enhances` from `DESCRIPTION`.
#'
#' @return A data frame with columns `Package`, `Type`, `Requirement`,
#'   `Source`, `File`, `Line`, and `Evidence`, ordered deterministically by
#'   file, line, package, and type. An empty project returns an empty data
#'   frame with the same columns.
#' @seealso [zak::init()]
#' @export
#'
#' @examples
#' \dontrun{
#' zak::dependencies(".")
#' }
dependencies <- function(
  path = ".",
  include_suggests = FALSE,
  include_enhances = FALSE
) {
  project <- validate_project_directory(path)
  validate_logical_flag(include_suggests, "include_suggests")
  validate_logical_flag(include_enhances, "include_enhances")

  records <- project_description_dependencies(
    project,
    include_suggests,
    include_enhances
  )
  files <- project_source_files(project)
  if (length(files)) {
    records <- c(records, project_code_dependencies(files, project))
  }

  result <- project_dependency_data_frame(records)
  if (!nrow(result)) {
    return(result)
  }
  line_order <- ifelse(is.na(result$Line), 0L, result$Line)
  package_order <- ifelse(is.na(result$Package), "", result$Package)
  requirement_order <- ifelse(
    is.na(result$Requirement),
    "",
    result$Requirement
  )
  result <- result[
    order(
      result$File,
      line_order,
      package_order,
      result$Type,
      requirement_order,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
  rownames(result) <- NULL
  result
}

#' Initialize a project
#'
#' `init()` discovers the project's static dependencies and records them in
#' `.zak/project.json`. For a local R package project it resolves the package
#' and its required dependencies through [plan()] and writes the resulting
#' `zak.lock` through [lock()]. For a non-package project it resolves each
#' discovered package reference and writes a project-level lockfile. By
#' default it also creates an empty isolated `.zak/library`; it does not
#' install packages or activate the project library.
#'
#' The project lockfile contains one embedded package plan per root reference.
#' `restore()` validates every embedded plan, aggregates their selected
#' packages, and commits them in one staged installation transaction. Isolated
#' project libraries and activation remain future extensions.
#'
#' @param path A single project directory.
#' @param lockfile A single lockfile name or path. Relative paths are resolved
#'   from `path` and parent directories are created as needed.
#' @param project_file A single project metadata name or path. Relative paths
#'   are resolved from `path` and parent directories are created as needed.
#' @param lib `NULL`, or a single character path to the installation library
#'   used while resolving dependencies.
#' @param verbose A single logical value. When `TRUE`, report initialization
#'   and planning steps.
#' @param force A single logical value. When `TRUE`, replace existing project
#'   metadata and an existing lockfile. The default refuses to overwrite
#'   either.
#' @param isolated A single logical value. When `TRUE`, create the project's
#'   `.zak/library` and record it as the default project library. When `FALSE`,
#'   do not create or use an isolated library.
#'
#' @return An object of class `zak_project`, invisibly, containing the
#'   expanded project path, project metadata path, lockfile path, isolated
#'   library path (or `NULL`), discovered dependencies, and ready installation
#'   plan(s).
#' @seealso [zak::dependencies()], [plan()], [lock()]
#' @export
#'
#' @examples
#' \dontrun{
#' project <- zak::init(".")
#' project$lockfile
#' }
init <- function(
  path = ".",
  lockfile = "zak.lock",
  project_file = file.path(".zak", "project.json"),
  lib = NULL,
  verbose = FALSE,
  force = FALSE,
  isolated = TRUE
) {
  project <- validate_project_directory(path)
  validate_verbose(verbose)
  validate_logical_flag(force, "force")
  validate_logical_flag(isolated, "isolated")
  metadata_path <- project_file_path(project, project_file)
  if (file.exists(metadata_path) && !isTRUE(force)) {
    stop(
      sprintf(
        "Zak project metadata already exists: %s. Use `force = TRUE` to replace it.",
        metadata_path
      ),
      call. = FALSE
    )
  }
  description <- file.path(project, "DESCRIPTION")

  discovered <- dependencies(project)
  report_step(
    verbose,
    sprintf("Discovered %d project dependency record(s).", nrow(discovered))
  )
  library_path <- if (isTRUE(isolated)) {
    project_isolated_library_path(project)
  } else {
    NULL
  }
  package_plan <- NULL
  package_plans <- list()
  references <- list()
  lock_path <- project_lock_path(project, lockfile)
  if (file.exists(lock_path) && !isTRUE(force)) {
    stop(
      sprintf(
        "Zak lockfile already exists: %s. Use `force = TRUE` to replace it.",
        lock_path
      ),
      call. = FALSE
    )
  }
  dir.create(dirname(lock_path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(description)) {
    package_plan <- plan(project, lib = lib, verbose = verbose)
    lock(package_plan, lock_path)
    report_step(
      verbose,
      "Project package metadata and lockfile are up to date."
    )
  } else {
    references <- project_dependency_references(discovered)
    package_plans <- lapply(
      references,
      function(reference) {
        plan(reference$reference, lib = lib, verbose = verbose)
      }
    )
    project_lock <- project_lock_document(
      project,
      lock_path,
      references,
      package_plans,
      library_path
    )
    write_lock_document(project_lock, lock_path)
    report_step(
      verbose,
      sprintf("Locked %d project package reference(s).", length(references))
    )
  }
  if (!is.null(library_path)) {
    create_project_library(library_path)
  }
  write_project_metadata(
    project,
    metadata_path,
    lock_path,
    discovered,
    library_path
  )

  invisible(structure(
    list(
      path = project,
      project_file = metadata_path,
      lockfile = lock_path,
      library = library_path,
      dependencies = discovered,
      plan = package_plan,
      plans = package_plans,
      references = references
    ),
    class = c("zak_project", "list")
  ))
}

#' Record the current project resolution
#'
#' `snapshot()` re-scans an initialized project, resolves its current package
#' references, and replaces its lockfile without installing packages. The
#' project's isolated library is used while planning by default, so packages
#' already present there are recorded as available rather than selected for
#' installation again.
#'
#' @param path A single initialized project directory.
#' @param lockfile `NULL` to use the lockfile recorded in project metadata, or
#'   a single lockfile name or path.
#' @param project_file A single project metadata name or path.
#' @param lib `NULL` to use the project's isolated library when configured, or
#'   a single library path used while resolving dependencies.
#' @param verbose A single logical value. When `TRUE`, report snapshot and
#'   planning steps.
#'
#' @return An object of class `zak_project`, invisibly, containing the updated
#'   lockfile, library path, discovered dependencies, and ready plan(s).
#' @seealso [init()], [status()], [restore()]
#' @export
#'
#' @examples
#' \dontrun{
#' project <- zak::snapshot(".")
#' project$lockfile
#' }
snapshot <- function(
  path = ".",
  lockfile = NULL,
  project_file = file.path(".zak", "project.json"),
  lib = NULL,
  verbose = FALSE
) {
  project <- validate_project_directory(path)
  validate_verbose(verbose)
  metadata <- read_project_metadata(project, project_file)
  if (is.null(lockfile)) {
    lockfile <- metadata$project$lockfile
  }
  if (is.null(lockfile) || !length(lockfile)) {
    stop("Project metadata does not record a lockfile path.", call. = FALSE)
  }
  lock_path <- project_lock_path(project, lockfile)
  dir.create(dirname(lock_path), recursive = TRUE, showWarnings = FALSE)
  library_path <- if (is.null(lib)) {
    project_metadata_library(project, metadata, create = TRUE)
  } else {
    lib
  }
  discovered <- dependencies(project)
  report_step(
    verbose,
    sprintf("Discovered %d project dependency record(s).", nrow(discovered))
  )
  description <- file.path(project, "DESCRIPTION")
  package_plan <- NULL
  package_plans <- list()
  references <- list()
  if (file.exists(description)) {
    package_plan <- plan(project, lib = library_path, verbose = verbose)
    lock(package_plan, lock_path)
  } else {
    references <- project_dependency_references(discovered)
    package_plans <- lapply(
      references,
      function(reference) {
        plan(reference$reference, lib = library_path, verbose = verbose)
      }
    )
    write_lock_document(
      project_lock_document(
        project,
        lock_path,
        references,
        package_plans,
        library_path
      ),
      lock_path
    )
  }
  if (!is.null(library_path)) {
    create_project_library(library_path)
  }
  write_project_metadata(
    project,
    project_file_path(project, project_file),
    lock_path,
    discovered,
    library_path
  )
  report_step(verbose, "Project snapshot and lockfile are up to date.")
  invisible(structure(
    list(
      path = project,
      project_file = project_file_path(project, project_file),
      lockfile = lock_path,
      library = library_path,
      dependencies = discovered,
      plan = package_plan,
      plans = package_plans,
      references = references
    ),
    class = c("zak_project", "list")
  ))
}

#' Report project package status
#'
#' `status()` compares the selected package versions in an initialized
#' project's lockfile with the packages installed in its project library. It
#' reports `ok`, `missing`, `changed`, and, by default, `extra` packages. The
#' operation is read-only and does not resolve or install packages.
#'
#' @param path A single initialized project directory.
#' @param lockfile `NULL` to use the lockfile recorded in project metadata, or
#'   a single lockfile name or path.
#' @param project_file A single project metadata name or path.
#' @param lib `NULL` to use the project's isolated library when configured, or
#'   a single library path to inspect.
#' @param include_extra A single logical value. When `TRUE`, include installed
#'   packages that are not selected by the lockfile.
#'
#' @return An object of class `zak_status`, invisibly, containing `path`,
#'   `lockfile`, `library`, and a `packages` data frame with `Package`,
#'   `Expected`, `Installed`, and `Status` columns.
#' @seealso [snapshot()], [restore()], [project_library()]
#' @export
#'
#' @examples
#' \dontrun{
#' project_status <- zak::status(".")
#' project_status$packages
#' }
status <- function(
  path = ".",
  lockfile = NULL,
  project_file = file.path(".zak", "project.json"),
  lib = NULL,
  include_extra = TRUE
) {
  project <- validate_project_directory(path)
  validate_logical_flag(include_extra, "include_extra")
  metadata <- read_project_metadata(project, project_file)
  if (is.null(lockfile)) {
    lockfile <- metadata$project$lockfile
  }
  if (is.null(lockfile) || !length(lockfile)) {
    stop("Project metadata does not record a lockfile path.", call. = FALSE)
  }
  lock_path <- project_lock_path(project, lockfile)
  package_lock <- read_lock(lock_path)
  library <- if (is.null(lib)) {
    project_status_library(project, package_lock, metadata)
  } else {
    installation_library(lib)
  }
  expected <- project_status_expected(package_lock)
  installed <- project_installed_versions(library)
  packages <- names(expected)
  rows <- data.frame(
    Package = packages,
    Expected = unname(expected),
    Installed = unname(installed[packages]),
    Status = vapply(
      packages,
      function(package) {
        if (is.na(installed[package])) {
          return("missing")
        }
        if (identical(installed[[package]], expected[[package]])) {
          return("ok")
        }
        "changed"
      },
      character(1L)
    ),
    stringsAsFactors = FALSE
  )
  if (isTRUE(include_extra)) {
    extra <- setdiff(names(installed), names(expected))
    if (length(extra)) {
      rows <- rbind(
        rows,
        data.frame(
          Package = extra,
          Expected = rep(NA_character_, length(extra)),
          Installed = unname(installed[extra]),
          Status = rep("extra", length(extra)),
          stringsAsFactors = FALSE
        )
      )
    }
  }
  if (nrow(rows)) {
    rows <- rows[order(rows$Package, method = "radix"), , drop = FALSE]
    rownames(rows) <- NULL
  }
  invisible(structure(
    list(
      path = project,
      lockfile = package_lock$file,
      library = library,
      packages = rows
    ),
    class = c("zak_status", "list")
  ))
}

#' @export
print.zak_status <- function(x, ...) {
  statuses <- table(x$packages$Status)
  summary <- if (!length(statuses)) {
    "clean"
  } else {
    paste(
      sprintf("%s %d", names(statuses), as.integer(statuses)),
      collapse = ", "
    )
  }
  cat(sprintf("zak project status: %s\n", summary))
  if (nrow(x$packages)) {
    print(x$packages, row.names = FALSE)
  }
  invisible(x)
}

#' Resolve a project's isolated library path
#'
#' `project_library()` returns the isolated library recorded by [init()]. It
#' does not change `.libPaths()` or activate the project. Set `create = TRUE`
#' to create the library when it is missing.
#'
#' @param path A single initialized project directory.
#' @param project_file A single project metadata name or path.
#' @param create A single logical value. When `TRUE`, create the isolated
#'   library if it does not exist.
#'
#' @return An absolute path to the isolated project library.
#' @seealso [init()], [zak::activate()], [zak::deactivate()]
#' @export
#'
#' @examples
#' \dontrun{
#' zak::project_library(".")
#' }
project_library <- function(
  path = ".",
  project_file = file.path(".zak", "project.json"),
  create = FALSE
) {
  project <- validate_project_directory(path)
  validate_logical_flag(create, "create")
  metadata <- read_project_metadata(project, project_file)
  library <- metadata$project$library
  if (is.null(library) || !length(library)) {
    stop(
      "This project was initialized without an isolated library.",
      call. = FALSE
    )
  }
  if (
    !is.character(library) ||
      length(library) != 1L ||
      is.na(library) ||
      !nzchar(library)
  ) {
    stop(
      "Project metadata contains an invalid isolated library path.",
      call. = FALSE
    )
  }
  library <- project_file_path(project, library)
  if (isTRUE(create)) {
    create_project_library(library)
  }
  if (!dir.exists(library)) {
    stop(
      sprintf("The isolated project library does not exist: %s", library),
      call. = FALSE
    )
  }
  normalizePath(library, mustWork = TRUE)
}

#' Activate an isolated project library
#'
#' `activate()` prepends a project's isolated library to `.libPaths()`. It
#' saves the previous library paths so that [zak::deactivate()] can restore
#' them.
#' Activation is explicit and does not install or restore packages.
#'
#' @param path A single initialized project directory.
#' @param project_file A single project metadata name or path.
#'
#' @return An object of class `zak_activation`, invisibly.
#' @seealso [zak::project_library()], [zak::deactivate()], [restore()]
#' @export
#'
#' @examples
#' \dontrun{
#' zak::activate(".")
#' zak::deactivate()
#' }
activate <- function(
  path = ".",
  project_file = file.path(".zak", "project.json")
) {
  project <- validate_project_directory(path)
  library <- project_library(project, project_file, create = TRUE)
  active <- zak_project_state$active
  if (!is.null(active)) {
    if (identical(active$path, project)) {
      return(invisible(active$activation))
    }
    stop(
      sprintf(
        "Another zak project is active: %s. Call `deactivate()` first.",
        active$path
      ),
      call. = FALSE
    )
  }
  previous <- .libPaths()
  .libPaths(unique(c(library, previous)))
  activation <- structure(
    list(path = project, library = library),
    class = c("zak_activation", "list")
  )
  zak_project_state$active <- list(
    path = project,
    library = library,
    previous = previous,
    activation = activation
  )
  invisible(activation)
}

#' Deactivate the active isolated project library
#'
#' `deactivate()` restores the `.libPaths()` captured by the most recent
#' [zak::activate()] call. It is a no-op when no zak project is active.
#'
#' @return `NULL`, invisibly.
#' @seealso [zak::activate()], [zak::project_library()]
#' @export
#'
#' @examples
#' \dontrun{
#' zak::deactivate()
#' }
deactivate <- function() {
  active <- zak_project_state$active
  if (is.null(active)) {
    return(invisible(NULL))
  }
  .libPaths(active$previous)
  zak_project_state$active <- NULL
  invisible(NULL)
}

#' @export
print.zak_activation <- function(x, ...) {
  cat(sprintf("zak project active: %s\n", x$path))
  cat(sprintf("Library: %s\n", x$library))
  invisible(x)
}

validate_project_directory <- function(path) {
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
  ) {
    stop("`path` must be a single existing project directory.", call. = FALSE)
  }
  path <- path.expand(path)
  if (!dir.exists(path)) {
    stop(sprintf("Project directory does not exist: %s", path), call. = FALSE)
  }
  normalizePath(path, mustWork = TRUE)
}

validate_logical_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop(sprintf("`%s` must be `TRUE` or `FALSE`.", name), call. = FALSE)
  }
  invisible()
}

project_lock_path <- function(project, lockfile) {
  project_file_path(project, lockfile)
}

project_file_path <- function(project, file) {
  if (
    !is.character(file) ||
      length(file) != 1L ||
      is.na(file) ||
      !nzchar(file)
  ) {
    stop("Project file paths must be a single non-empty path.", call. = FALSE)
  }
  file <- path.expand(file)
  if (!startsWith(file, "/") && !grepl("^[A-Za-z]:[/\\\\]", file)) {
    file <- file.path(project, file)
  }
  normalizePath(file, mustWork = FALSE)
}

project_isolated_library_path <- function(project) {
  project_file_path(project, file.path(".zak", "library"))
}

create_project_library <- function(library) {
  if (!dir.exists(library)) {
    dir.create(library, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(library)) {
    stop(
      sprintf("R could not create the isolated project library: %s", library),
      call. = FALSE
    )
  }
  invisible(normalizePath(library, mustWork = TRUE))
}

# Resolve the isolated library recorded in already-read project metadata.
# Returns NULL when the project was initialized without one.
project_metadata_library <- function(project, metadata, create = FALSE) {
  library <- metadata$project$library
  if (is.null(library) || !length(library)) {
    return(NULL)
  }
  library <- library[[1L]]
  if (is.na(library)) {
    return(NULL)
  }
  if (!is.character(library) || !nzchar(library)) {
    stop(
      "Project metadata contains an invalid isolated library path.",
      call. = FALSE
    )
  }
  library <- project_file_path(project, library)
  if (isTRUE(create)) {
    create_project_library(library)
  }
  library
}

# `status()` is read-only, so this never creates a library. It falls back to
# the lockfile's recorded library, then to R's first library path.
project_status_library <- function(project, package_lock, metadata) {
  library <- project_metadata_library(project, metadata, create = FALSE)
  if (is.null(library)) {
    recorded <- package_lock$project$library
    if (
      !is.null(recorded) &&
        length(recorded) &&
        is.character(recorded[[1L]]) &&
        !is.na(recorded[[1L]]) &&
        nzchar(recorded[[1L]])
    ) {
      library <- project_file_path(project, recorded[[1L]])
    }
  }
  if (is.null(library)) {
    libraries <- .libPaths()
    if (!length(libraries)) {
      stop("R did not provide a library to inspect.", call. = FALSE)
    }
    library <- libraries[[1L]]
  }
  library
}

# Package versions a lockfile selects, as package -> version.
project_status_expected <- function(package_lock) {
  if (inherits(package_lock, "zak_project_lock")) {
    return(project_lock_install_entries(package_lock)$versions)
  }
  versions <- character()
  dependencies <- package_lock$dependencies
  if (nrow(dependencies)) {
    selected <- dependencies[
      dependencies$Status %in% c("install", "available") &
        !is.na(dependencies$Version),
      ,
      drop = FALSE
    ]
    if (nrow(selected)) {
      versions <- stats::setNames(selected$Version, selected$Package)
    }
  }
  versions[[package_lock$target$package]] <- package_lock$target$version
  versions
}

# Reads each package's DESCRIPTION rather than using
# `utils::installed.packages()`, which omits packages with no
# `Meta/package.rds` such as installed binaries.
project_installed_versions <- function(library) {
  empty <- stats::setNames(character(), character())
  if (is.null(library) || !length(library) || !dir.exists(library)) {
    return(empty)
  }
  packages <- list.dirs(library, full.names = FALSE, recursive = FALSE)
  packages <- packages[nzchar(packages)]
  if (!length(packages)) {
    return(empty)
  }
  versions <- vapply(
    packages,
    function(package) staged_package_version(library, package),
    character(1L)
  )
  versions[!is.na(versions)]
}

read_project_metadata <- function(project, file) {
  require_jsonlite()
  file <- project_file_path(project, file)
  if (!file.exists(file) || dir.exists(file)) {
    stop(
      sprintf("Zak project metadata does not exist: %s", file),
      call. = FALSE
    )
  }
  document <- tryCatch(
    jsonlite::read_json(file, simplifyVector = FALSE),
    error = function(error) {
      stop(
        sprintf(
          "Could not read zak project metadata: %s",
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
  if (!is.list(document) || !is.list(document$project)) {
    stop("Zak project metadata has an invalid project record.", call. = FALSE)
  }
  document
}

write_project_metadata <- function(
  project,
  file,
  lock_path,
  dependencies,
  library_path
) {
  require_jsonlite()
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  document <- list(
    project = list(
      name = basename(project),
      schema = 1L,
      lockfile = if (is.null(lock_path)) {
        NA_character_
      } else {
        project_relative_path(project, lock_path)
      },
      library = if (is.null(library_path)) {
        NA_character_
      } else {
        project_relative_path(project, library_path)
      }
    ),
    dependencies = dependencies
  )
  contents <- jsonlite::toJSON(
    document,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
  temporary <- tempfile("zak-project-", tmpdir = dirname(file))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  writeLines(contents, temporary, useBytes = TRUE)
  if (!file.rename(temporary, file)) {
    stop("R could not replace the project metadata file.", call. = FALSE)
  }
  invisible(file)
}

project_dependency_references <- function(dependencies) {
  if (!nrow(dependencies)) {
    return(list())
  }
  remote_rows <- dependencies[dependencies$Type == "Remotes", , drop = FALSE]
  if (nrow(remote_rows) && any(is.na(remote_rows$Package))) {
    stop(
      "Project initialization cannot resolve a remote without an inferable package name.",
      call. = FALSE
    )
  }
  remote_map <- list()
  if (nrow(remote_rows)) {
    for (package in unique(remote_rows$Package)) {
      values <- unique(
        remote_rows$Requirement[remote_rows$Package == package]
      )
      if (length(values) > 1L) {
        stop(
          sprintf(
            "Project initialization found conflicting remote references for `%s`: %s.",
            package,
            paste(values, collapse = ", ")
          ),
          call. = FALSE
        )
      }
      remote_map[[package]] <- project_remote_reference(values[[1L]])
    }
  }
  packages <- unique(dependencies$Package[!is.na(dependencies$Package)])
  packages <- setdiff(packages, project_base_packages())
  packages <- sort(packages, method = "radix")
  lapply(packages, function(package) {
    list(
      package = package,
      reference = if (is.null(remote_map[[package]])) {
        package
      } else {
        remote_map[[package]]
      }
    )
  })
}

project_remote_reference <- function(value) {
  source <- parse_remote_reference(value)
  switch(
    source$type,
    git = paste0("git::", source$url, "@", source$ref),
    url = paste0("url::", source$url),
    bioconductor = paste0("bioc::", source$package),
    stop(
      sprintf("Unsupported project remote source: %s.", value),
      call. = FALSE
    )
  )
}

project_base_packages <- function() {
  c(
    "R",
    "base",
    "compiler",
    "datasets",
    "grDevices",
    "graphics",
    "grid",
    "methods",
    "parallel",
    "splines",
    "stats",
    "stats4",
    "tcltk",
    "tools",
    "utils"
  )
}

project_lock_document <- function(
  project,
  file,
  references,
  plans,
  library_path
) {
  if (length(references) != length(plans)) {
    stop(
      "Project references and package plans must have the same length.",
      call. = FALSE
    )
  }
  if (length(plans)) {
    vapply(
      plans,
      function(package_plan) {
        validate_lock_plan(package_plan)
        TRUE
      },
      logical(1L)
    )
  }
  list(
    lockfile = list(name = "zak", version = 2L, type = "project"),
    project = list(
      name = basename(project),
      root = ".",
      lockfile = project_relative_path(project, file),
      library = if (is.null(library_path)) {
        NA_character_
      } else {
        project_relative_path(project, library_path)
      }
    ),
    references = lapply(
      references,
      function(reference) {
        list(
          package = reference$package,
          reference = project_lock_reference_text(reference$reference)
        )
      }
    ),
    plans = lapply(plans, lock_document)
  )
}

project_lock_reference_text <- function(reference) {
  if (startsWith(reference, "url::")) {
    return(paste0("url::", redact_lock_url(sub("^url::", "", reference))))
  }
  if (startsWith(reference, "git::")) {
    value <- sub("^git::", "", reference)
    parsed <- parse_git_reference(value)
    return(paste0(
      "git::",
      redact_lock_url(parsed$url),
      "@",
      parsed$ref
    ))
  }
  reference
}

project_relative_path <- function(project, file) {
  prefix <- paste0(project, .Platform$file.sep)
  if (startsWith(file, prefix)) {
    return(substring(file, nchar(prefix) + 1L))
  }
  file
}

project_description_dependencies <- function(
  project,
  include_suggests,
  include_enhances
) {
  description <- file.path(project, "DESCRIPTION")
  if (!file.exists(description)) {
    return(list())
  }
  fields <- tryCatch(
    read.dcf(description, all = TRUE)[1L, , drop = TRUE],
    error = function(error) {
      stop(
        sprintf(
          "Could not read project DESCRIPTION: %s",
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
  dependency_fields <- c("Depends", "Imports", "LinkingTo")
  if (isTRUE(include_suggests)) {
    dependency_fields <- c(dependency_fields, "Suggests")
  }
  if (isTRUE(include_enhances)) {
    dependency_fields <- c(dependency_fields, "Enhances")
  }

  records <- list()
  for (field in dependency_fields) {
    value <- fields[[field]]
    if (!length(value) || is.na(value) || !nzchar(trimws(value))) {
      next
    }
    requirements <- split_project_requirements(value)
    if (!length(requirements)) {
      next
    }
    records <- c(
      records,
      lapply(requirements, function(requirement) {
        list(
          Package = requirement$name,
          Type = field,
          Requirement = format_project_requirement(requirement),
          Source = "DESCRIPTION",
          File = "DESCRIPTION",
          Line = NA_integer_,
          Evidence = trimws(requirement$raw)
        )
      })
    )
  }

  remote_value <- fields[["Remotes"]]
  if (
    length(remote_value) && !is.na(remote_value) && nzchar(trimws(remote_value))
  ) {
    remote_values <- trimws(unlist(strsplit(remote_value, ",", fixed = TRUE)))
    remote_values <- remote_values[nzchar(remote_values)]
    remotes <- parse_remotes(unlist(fields, use.names = TRUE))
    for (index in seq_along(remote_values)) {
      remote <- remotes[[index]]
      records[[length(records) + 1L]] <- list(
        Package = if (is.null(remote$package)) {
          NA_character_
        } else {
          remote$package
        },
        Type = "Remotes",
        Requirement = remote_values[[index]],
        Source = "DESCRIPTION",
        File = "DESCRIPTION",
        Line = NA_integer_,
        Evidence = remote_values[[index]]
      )
    }
  }
  records
}

split_project_requirements <- function(value) {
  requirements <- split_dependency_field(value)
  unname(lapply(requirements, function(requirement) {
    requirement$raw <- project_requirement_text(requirement)
    requirement
  }))
}

project_requirement_text <- function(requirement) {
  if (length(requirement$op) && length(requirement$version)) {
    return(sprintf(
      "%s %s %s",
      requirement$name,
      requirement$op,
      as.character(requirement$version)
    ))
  }
  requirement$name
}

format_project_requirement <- function(requirement) {
  project_requirement_text(requirement)
}

project_source_files <- function(project) {
  files <- list.files(
    project,
    recursive = TRUE,
    all.files = FALSE,
    full.names = TRUE
  )
  if (!length(files)) {
    return(character())
  }
  ignored <- c(".git", ".zak", "renv", "packrat", "node_modules")
  relative <- substring(files, nchar(project) + 2L)
  parts <- strsplit(relative, "[/\\\\]", perl = TRUE)
  keep <- !vapply(parts, function(value) any(value %in% ignored), logical(1L))
  files <- files[keep & !dir.exists(files)]
  extensions <- tools::file_ext(files)
  files[extensions %in% c("R", "Rmd", "qmd", "Rnw")]
}

project_code_dependencies <- function(files, project) {
  records <- list()
  for (file in files) {
    lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
    relative <- substring(file, nchar(project) + 2L)
    source <- tools::file_ext(file)
    for (line_number in seq_along(lines)) {
      line <- lines[[line_number]]
      namespace_matches <- regmatches(
        line,
        gregexpr(
          "[A-Za-z][A-Za-z0-9.]*\\s*:::{0,1}",
          line,
          perl = TRUE
        )
      )[[1L]]
      if (length(namespace_matches) && namespace_matches[[1L]] != -1L) {
        namespace_matches <- sub(
          "\\s*:::{0,1}$",
          "",
          namespace_matches,
          perl = TRUE
        )
        records <- c(
          records,
          lapply(namespace_matches, function(package) {
            project_code_record(
              package,
              "namespace",
              source,
              relative,
              line_number,
              line
            )
          })
        )
      }

      records <- c(
        records,
        project_call_records(
          line,
          "(?:library|require)",
          "library",
          source,
          relative,
          line_number
        ),
        project_call_records(
          line,
          "(?:requireNamespace|loadNamespace)",
          "namespace",
          source,
          relative,
          line_number
        )
      )
    }
  }
  records
}

project_call_records <- function(
  line,
  function_pattern,
  type,
  source,
  file,
  line_number
) {
  matches <- regmatches(
    line,
    gregexpr(
      sprintf(
        "\\b%s\\s*\\(\\s*[\\\"']?[A-Za-z][A-Za-z0-9.]*",
        function_pattern
      ),
      line,
      perl = TRUE
    )
  )[[1L]]
  if (!length(matches) || identical(matches[[1L]], -1L)) {
    return(list())
  }
  packages <- sub("^.*\\(\\s*[\\\"']?", "", matches, perl = TRUE)
  lapply(packages, function(package) {
    project_code_record(package, type, source, file, line_number, line)
  })
}

project_code_record <- function(
  package,
  type,
  source,
  file,
  line_number,
  evidence
) {
  list(
    Package = package,
    Type = type,
    Requirement = NA_character_,
    Source = source,
    File = file,
    Line = as.integer(line_number),
    Evidence = trimws(evidence)
  )
}

project_dependency_data_frame <- function(records) {
  columns <- c(
    "Package",
    "Type",
    "Requirement",
    "Source",
    "File",
    "Line",
    "Evidence"
  )
  if (!length(records)) {
    result <- list(
      character(),
      character(),
      character(),
      character(),
      character(),
      integer(),
      character()
    )
    names(result) <- columns
    return(as.data.frame(result, stringsAsFactors = FALSE))
  }
  result <- as.data.frame(
    do.call(rbind, lapply(records, as.data.frame, stringsAsFactors = FALSE)),
    stringsAsFactors = FALSE
  )
  result$Line <- as.integer(result$Line)
  result[, columns, drop = FALSE]
}
