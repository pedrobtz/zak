#' Write a lockfile from an installation plan
#'
#' `lock()` writes a versioned JSON record of a ready [plan()]. The record
#' contains platform facts, repositories, source provenance, selected target
#' and dependency versions, per-dependency remote sources, acquisition
#' metadata including SHA-256 checksums, and system requirements. It does not
#' install packages; [restore()] consumes the resulting lockfile when a
#' library should be reconciled.
#'
#' Temporary acquired-artifact paths are never written. Before URL provenance
#' is recorded, any URL userinfo (`scheme://user:password@host`) is replaced
#' wholesale, and query or fragment parameters whose name matches a known
#' credential name are replaced with `<redacted>`. This is a best-effort
#' filter over parameter *names*: a secret carried in an unrecognized parameter
#' is still written to the lockfile, so treat lockfiles as sensitive and prefer
#' credential helpers over credentials embedded in URLs. A redacted URL cannot
#' be restored from, and [restore()] refuses it rather than fetching a
#' different URL than the one that was locked.
#'
#' @param plan A ready object returned by [plan()]. Blocked plans cannot be
#'   locked.
#' @param file A single path where the JSON lockfile should be written.
#'
#' @return `file`, invisibly, expanded to an absolute path.
#' @seealso [plan()]
#' @export
#'
#' @examples
#' \dontrun{
#' package_plan <- zak::plan("jsonlite")
#' zak::lock(package_plan, "zak.lock")
#' }
lock <- function(plan, file = "zak.lock") {
  validate_lock_plan(plan)
  file <- validate_lock_file(file)
  require_jsonlite()

  write_lock_document(lock_document(plan), file)
  invisible(file)
}

write_lock_document <- function(document, file) {
  require_jsonlite()
  file <- validate_lock_file(file)
  contents <- jsonlite::toJSON(
    document,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
  temporary <- tempfile("zak-lock-", tmpdir = dirname(file))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  writeLines(contents, temporary, useBytes = TRUE)
  if (!file.rename(temporary, file)) {
    stop("R could not replace the lockfile.", call. = FALSE)
  }
  invisible(file)
}

#' Read and validate a zak lockfile
#'
#' `read_lock()` parses a versioned package or project JSON lockfile and
#' returns a validated, typed lock object. It does not install packages or
#' change any library.
#'
#' @param file A single path to a zak JSON lockfile.
#'
#' @return An object of class `zak_lock` or `zak_project_lock` containing the
#'   validated lockfile records and the expanded lockfile path.
#' @seealso [lock()], [compare_lock()]
#' @export
#'
#' @examples
#' \dontrun{
#' package_lock <- zak::read_lock("zak.lock")
#' package_lock$dependencies
#' }
read_lock <- function(file) {
  require_jsonlite()
  file <- validate_lock_read_file(file)
  document <- tryCatch(
    jsonlite::read_json(
      file,
      simplifyVector = FALSE,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    ),
    error = function(error) {
      stop(
        sprintf("Could not read zak lockfile: %s", conditionMessage(error)),
        call. = FALSE
      )
    }
  )
  if (is_project_lock_document(document)) {
    validate_project_lock_document(document)
    return(new_zak_project_lock(document, file))
  }
  validate_lock_document(document)
  new_zak_lock(document, file)
}

#' Compare a lockfile with an installation plan
#'
#' `compare_lock()` compares stable resolution decisions in a validated
#' lockfile with a newly generated [plan()]. It is read-only: no packages are
#' installed and no lockfile is changed. Acquisition timestamps and methods
#' are intentionally ignored because they describe when and how an archive
#' was fetched rather than which artifact was selected.
#'
#' @param lock A `zak_lock` returned by [read_lock()], or a path to a zak JSON
#'   lockfile.
#' @param plan A ready object returned by [plan()].
#'
#' @return An object of class `zak_lock_comparison` with `status` equal to
#'   `"current"` or `"drifted"`, and a `changes` data frame containing one
#'   row per changed decision.
#' @seealso [read_lock()], [plan()]
#' @export
#'
#' @examples
#' \dontrun{
#' package_plan <- zak::plan("jsonlite")
#' package_lock <- zak::read_lock("zak.lock")
#' zak::compare_lock(package_lock, package_plan)
#' }
compare_lock <- function(lock, plan) {
  lock <- as_zak_lock(lock)
  if (inherits(lock, "zak_project_lock")) {
    stop(
      "`compare_lock()` currently supports package lockfiles only.",
      call. = FALSE
    )
  }
  validate_lock_plan(plan)
  require_jsonlite()

  fields <- c(
    "platform",
    "repositories",
    "source",
    "target",
    "dependencies",
    "dependency_sources",
    "acquisition"
  )
  expected <- plan_comparison_document(plan)
  actual <- lock_comparison_document(lock)
  changed <- fields[
    !vapply(
      fields,
      function(field) {
        identical(actual[[field]], expected[[field]])
      },
      logical(1L)
    )
  ]
  changes <- data.frame(
    Field = changed,
    Lock = vapply(
      changed,
      function(field) format_lock_value(actual[[field]]),
      character(1L)
    ),
    Plan = vapply(
      changed,
      function(field) format_lock_value(expected[[field]]),
      character(1L)
    ),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      status = if (length(changed)) "drifted" else "current",
      changes = changes,
      lock = lock,
      plan = plan
    ),
    class = c("zak_lock_comparison", "list")
  )
}

validate_lock_plan <- function(plan) {
  if (!inherits(plan, "zak_plan")) {
    stop("`plan` must be a zak installation plan.", call. = FALSE)
  }
  if (!identical(plan$status, "ready")) {
    stop("Only ready installation plans can be locked.", call. = FALSE)
  }
  invisible()
}

validate_lock_file <- function(file) {
  if (
    !is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)
  ) {
    stop("`file` must be a single non-empty path.", call. = FALSE)
  }
  path <- path.expand(file)
  parent <- normalizePath(dirname(path), mustWork = TRUE)
  file.path(parent, basename(path))
}

validate_lock_read_file <- function(file) {
  path <- validate_lock_file(file)
  if (!file.exists(path) || dir.exists(path)) {
    stop(sprintf("Zak lockfile does not exist: %s", path), call. = FALSE)
  }
  path
}

require_jsonlite <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "The `jsonlite` package is required to read or write zak lockfiles.",
      call. = FALSE
    )
  }
  invisible()
}

lock_document <- function(plan) {
  list(
    lockfile = list(name = "zak", version = 1L),
    platform = unclass(plan$platform),
    repositories = lock_repositories(plan$repositories),
    source = lock_source(plan$source),
    target = list(
      package = plan$target$package,
      version = plan$target$version,
      type = plan$target$type,
      format = plan$target$format,
      system_requirements = lock_system_requirements(
        plan$archive_metadata$fields
      )
    ),
    dependencies = lock_dependencies(plan$dependencies),
    dependency_sources = lock_dependency_sources(
      plan$install_dependencies,
      plan$dependency_sources
    ),
    acquisition = lock_acquisition(plan$acquisition),
    resolution = list(
      status = plan$status,
      unavailable = unname(plan$unavailable)
    )
  )
}

lock_repositories <- function(repositories) {
  if (is.null(repositories)) {
    return(list())
  }
  repositories <- as.character(repositories)
  if (is.null(names(repositories))) {
    return(as.list(unname(repositories)))
  }
  as.list(repositories)
}

lock_source <- function(source) {
  switch(
    source$type,
    url = list(type = "url", url = redact_lock_url(source$url)),
    local = list(type = "local", path = source$path),
    local_archive = list(
      type = "local_archive",
      path = source$path,
      format = source$format
    ),
    git = list(
      type = "git",
      url = redact_lock_url(source$url),
      ref = source$ref
    ),
    bioconductor = list(
      type = "bioconductor",
      package = source$package
    ),
    repository = list(type = "repository", package = source$package)
  )
}

lock_system_requirements <- function(fields) {
  value <- unname(fields["SystemRequirements"])
  if (!length(value) || is.na(value) || !nzchar(trimws(value))) {
    return(NULL)
  }
  gsub("[[:cntrl:]]+[[:space:]]*", " ", trimws(value))
}

lock_dependencies <- function(dependencies) {
  if (!nrow(dependencies)) {
    return(list())
  }
  fields <- c(
    "Package",
    "Version",
    "Repository",
    "Status",
    "Direct",
    "Constraint"
  )
  lapply(
    seq_len(nrow(dependencies)),
    function(row) {
      stats::setNames(
        lapply(fields, function(field) unname(dependencies[row, field])),
        fields
      )
    }
  )
}

lock_dependency_sources <- function(dependencies, sources) {
  if (!length(sources)) {
    return(list())
  }
  packages <- intersect(dependencies, names(sources))
  stats::setNames(
    lapply(packages, function(package) {
      record <- sources[[package]]
      source <- lock_source(record$source)
      if (
        identical(source$type, "git") &&
          !is.null(record$acquisition)
      ) {
        source$commit <- record$acquisition$commit
      }
      if (!is.null(record$acquisition)) {
        source$sha256 <- record$acquisition$sha256
      }
      source
    }),
    packages
  )
}

lock_acquisition <- function(acquisition) {
  if (is.null(acquisition)) {
    return(NULL)
  }
  if (identical(acquisition$type, "git")) {
    return(list(
      type = "git",
      url = redact_lock_url(acquisition$url),
      ref = acquisition$ref,
      commit = acquisition$commit,
      format = acquisition$format,
      size = acquisition$size,
      sha256 = acquisition$sha256,
      retrieved_at = acquisition$retrieved_at,
      method = acquisition$method
    ))
  }
  list(
    type = "archive",
    url = redact_lock_url(acquisition$url),
    format = acquisition$format,
    size = acquisition$size,
    sha256 = acquisition$sha256,
    retrieved_at = acquisition$retrieved_at,
    method = acquisition$method
  )
}

validate_lock_document <- function(document) {
  if (!is.list(document) || is.null(names(document))) {
    stop("Zak lockfile must contain a JSON object.", call. = FALSE)
  }
  lockfile <- require_lock_record(document, "lockfile")
  require_lock_text(lockfile, "name", "lockfile.name")
  version <- require_lock_number(lockfile, "version", "lockfile.version")
  if (!isTRUE(version == 1)) {
    stop(
      sprintf(
        "Unsupported zak lockfile schema version: %s.",
        format(version, trim = TRUE)
      ),
      call. = FALSE
    )
  }
  if (!identical(lockfile$name, "zak")) {
    stop("Zak lockfile has an unknown lockfile name.", call. = FALSE)
  }

  platform <- require_lock_record(document, "platform")
  validate_lock_platform(platform)
  repositories <- require_lock_list(document, "repositories")
  validate_lock_repositories(repositories)
  source <- require_lock_record(document, "source")
  validate_lock_source(source)
  target <- require_lock_record(document, "target")
  validate_lock_target(target)
  dependencies <- require_lock_list(document, "dependencies")
  validate_lock_dependencies(dependencies)
  if (
    !is.null(document$dependency_sources) &&
      length(document$dependency_sources)
  ) {
    validate_lock_dependency_sources(document$dependency_sources)
  }
  if (!is.null(document$acquisition) && length(document$acquisition)) {
    acquisition <- require_lock_record(document, "acquisition")
    validate_lock_acquisition(acquisition)
  }
  resolution <- require_lock_record(document, "resolution")
  require_lock_text(resolution, "status", "resolution.status")
  if (!identical(resolution$status, "ready")) {
    stop(
      "Zak lockfiles must record a ready resolution.",
      call. = FALSE
    )
  }
  unavailable <- require_lock_list(resolution, "unavailable")
  if (length(unavailable)) {
    vapply(
      unavailable,
      function(package) {
        validate_lock_package(package, "resolution.unavailable")
      },
      logical(1L)
    )
  }
  invisible(document)
}

is_project_lock_document <- function(document) {
  is.list(document) &&
    is.list(document$lockfile) &&
    identical(document$lockfile$type, "project")
}

validate_project_lock_document <- function(document) {
  if (!is.list(document) || is.null(names(document))) {
    stop("Zak project lockfile must contain a JSON object.", call. = FALSE)
  }
  lockfile <- require_lock_record(document, "lockfile")
  require_lock_text(lockfile, "name", "lockfile.name")
  version <- require_lock_number(lockfile, "version", "lockfile.version")
  if (!isTRUE(version == 2)) {
    stop(
      sprintf(
        "Unsupported zak project lockfile schema version: %s.",
        format(version, trim = TRUE)
      ),
      call. = FALSE
    )
  }
  if (!identical(lockfile$name, "zak")) {
    stop("Zak project lockfile has an unknown lockfile name.", call. = FALSE)
  }
  project <- require_lock_record(document, "project")
  require_lock_text(project, "name", "project.name")
  require_lock_text(project, "root", "project.root")
  require_lock_text(project, "lockfile", "project.lockfile")
  if (!is.null(project$library)) {
    require_lock_text(project, "library", "project.library")
  }
  references <- require_lock_list(document, "references")
  vapply(
    references,
    function(reference) {
      if (!is.list(reference) || is.null(names(reference))) {
        stop(
          "Each zak project lockfile reference must be an object.",
          call. = FALSE
        )
      }
      validate_lock_package(reference$package, "reference.package")
      require_lock_text(reference, "reference", "reference.reference")
      TRUE
    },
    logical(1L)
  )
  plans <- require_lock_list(document, "plans")
  if (length(plans) != length(references)) {
    stop(
      "Zak project lockfile references and plans must have the same length.",
      call. = FALSE
    )
  }
  vapply(
    plans,
    function(package_lock) {
      validate_lock_document(package_lock)
      TRUE
    },
    logical(1L)
  )
  invisible(document)
}

require_lock_record <- function(record, field) {
  value <- record[[field]]
  if (!is.list(value) || is.null(names(value))) {
    stop(
      sprintf("Zak lockfile field `%s` must be an object.", field),
      call. = FALSE
    )
  }
  value
}

require_lock_list <- function(record, field) {
  value <- record[[field]]
  if (!is.list(value)) {
    stop(
      sprintf("Zak lockfile field `%s` must be an array.", field),
      call. = FALSE
    )
  }
  value
}

require_lock_text <- function(record, field, label = field) {
  value <- record[[field]]
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value))
  ) {
    stop(
      sprintf("Zak lockfile field `%s` must be non-empty text.", label),
      call. = FALSE
    )
  }
  value
}

require_lock_text_or_empty <- function(record, field, label = field) {
  value <- record[[field]]
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop(
      sprintf("Zak lockfile field `%s` must be text.", label),
      call. = FALSE
    )
  }
  value
}

require_lock_number <- function(record, field, label = field) {
  value <- record[[field]]
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value)
  ) {
    stop(
      sprintf("Zak lockfile field `%s` must be a finite number.", label),
      call. = FALSE
    )
  }
  value
}

validate_lock_platform <- function(platform) {
  os <- require_lock_text(platform, "os", "platform.os")
  if (!os %in% c("windows", "macos", "linux", "unix")) {
    stop(sprintf("Unknown zak lockfile platform OS: %s.", os), call. = FALSE)
  }
  require_lock_text(platform, "architecture", "platform.architecture")
  require_lock_text(platform, "r_version", "platform.r_version")
  require_lock_text(platform, "package_type", "platform.package_type")
  invisible()
}

validate_lock_repositories <- function(repositories) {
  if (!length(repositories)) {
    return(invisible())
  }
  vapply(
    repositories,
    function(repository) {
      require_lock_text(list(value = repository), "value", "repository")
      TRUE
    },
    logical(1L)
  )
  invisible()
}

validate_lock_source <- function(source) {
  type <- require_lock_text(source, "type", "source.type")
  switch(
    type,
    url = {
      url <- require_lock_text(source, "url", "source.url")
      if (!grepl("^https?://", url, ignore.case = TRUE)) {
        stop("Zak lockfile source.url must be an HTTP(S) URL.", call. = FALSE)
      }
    },
    local = require_lock_text(source, "path", "source.path"),
    local_archive = {
      require_lock_text(source, "path", "source.path")
      format <- require_lock_text(source, "format", "source.format")
      if (!format %in% c("tar.gz", "zip")) {
        stop(
          sprintf("Unknown zak lockfile source format: %s.", format),
          call. = FALSE
        )
      }
    },
    git = {
      url <- require_lock_text(source, "url", "source.url")
      validate_git_url(url)
      require_lock_text(source, "ref", "source.ref")
    },
    bioconductor = validate_lock_package(
      source$package,
      "source.package"
    ),
    repository = validate_lock_package(source$package, "source.package"),
    stop(sprintf("Unknown zak lockfile source type: %s.", type), call. = FALSE)
  )
  invisible()
}

validate_lock_target <- function(target) {
  validate_lock_package(target$package, "target.package")
  require_lock_text(target, "version", "target.version")
  type <- require_lock_text(target, "type", "target.type")
  if (!type %in% c("source", "binary")) {
    stop(sprintf("Unknown zak lockfile target type: %s.", type), call. = FALSE)
  }
  format <- require_lock_text(target, "format", "target.format")
  if (!format %in% c("tar.gz", "zip", "directory", "repository")) {
    stop(
      sprintf("Unknown zak lockfile target format: %s.", format),
      call. = FALSE
    )
  }
  if (!"system_requirements" %in% names(target)) {
    stop(
      "Zak lockfile field `target.system_requirements` is required.",
      call. = FALSE
    )
  }
  requirements <- target$system_requirements
  if (!is.null(requirements) && length(requirements)) {
    require_lock_text(
      target,
      "system_requirements",
      "target.system_requirements"
    )
  }
  invisible()
}

validate_lock_dependencies <- function(dependencies) {
  if (!length(dependencies)) {
    return(invisible())
  }
  vapply(
    dependencies,
    function(dependency) {
      if (!is.list(dependency) || is.null(names(dependency))) {
        stop("Each zak lockfile dependency must be an object.", call. = FALSE)
      }
      validate_lock_package(dependency$Package, "dependency.Package")
      require_lock_text(dependency, "Version", "dependency.Version")
      if (!is.null(dependency$Repository)) {
        require_lock_text(dependency, "Repository", "dependency.Repository")
      }
      status <- require_lock_text(dependency, "Status", "dependency.Status")
      if (!status %in% c("install", "available")) {
        stop(
          sprintf("Unknown zak lockfile dependency status: %s.", status),
          call. = FALSE
        )
      }
      require_lock_logical(dependency, "Direct", "dependency.Direct")
      require_lock_text_or_empty(
        dependency,
        "Constraint",
        "dependency.Constraint"
      )
      TRUE
    },
    logical(1L)
  )
  invisible()
}

validate_lock_dependency_sources <- function(sources) {
  if (!is.list(sources) || is.null(names(sources))) {
    stop(
      "Zak lockfile field `dependency_sources` must be an object.",
      call. = FALSE
    )
  }
  if (!length(sources)) {
    return(invisible())
  }
  vapply(
    sources,
    function(source) {
      validate_lock_source(source)
      if (
        identical(source$type, "git") &&
          !is.null(source$commit)
      ) {
        commit <- require_lock_text(
          source,
          "commit",
          "dependency_sources.commit"
        )
        if (!grepl("^[0-9a-f]+$", commit, ignore.case = TRUE)) {
          stop(
            "Zak lockfile dependency source commit must be a Git commit hash.",
            call. = FALSE
          )
        }
      }
      validate_lock_optional_sha256(
        source,
        "sha256",
        "dependency_sources.sha256"
      )
      TRUE
    },
    logical(1L)
  )
  invisible()
}

validate_lock_acquisition <- function(acquisition) {
  type <- require_lock_text(acquisition, "type", "acquisition.type")
  if (!type %in% c("archive", "git")) {
    stop(
      sprintf("Unknown zak lockfile acquisition type: %s.", type),
      call. = FALSE
    )
  }
  url <- require_lock_text(acquisition, "url", "acquisition.url")
  format <- require_lock_text(acquisition, "format", "acquisition.format")
  if (identical(type, "archive")) {
    if (!grepl("^(https?|file)://", url, ignore.case = TRUE)) {
      stop(
        "Zak lockfile acquisition.url must be an HTTP(S) or file URL.",
        call. = FALSE
      )
    }
  } else {
    validate_git_url(url)
    require_lock_text(acquisition, "ref", "acquisition.ref")
    commit <- require_lock_text(acquisition, "commit", "acquisition.commit")
    if (!grepl("^[0-9a-f]+$", commit, ignore.case = TRUE)) {
      stop(
        "Zak lockfile acquisition.commit must be a Git commit hash.",
        call. = FALSE
      )
    }
  }
  valid_formats <- if (identical(type, "git")) {
    "directory"
  } else {
    c("tar.gz", "zip")
  }
  if (!format %in% valid_formats) {
    stop(
      sprintf("Unknown zak lockfile acquisition format: %s.", format),
      call. = FALSE
    )
  }
  size <- require_lock_number(acquisition, "size", "acquisition.size")
  if (size < 0) {
    stop("Zak lockfile acquisition.size cannot be negative.", call. = FALSE)
  }
  validate_lock_optional_sha256(acquisition, "sha256", "acquisition.sha256")
  require_lock_text(acquisition, "retrieved_at", "acquisition.retrieved_at")
  require_lock_text(acquisition, "method", "acquisition.method")
  invisible()
}

validate_lock_optional_sha256 <- function(record, field, label = field) {
  value <- record[[field]]
  if (is.null(value)) {
    return(invisible())
  }
  if (!is_sha256(value)) {
    stop(
      sprintf("Zak lockfile field `%s` must be a SHA-256 checksum.", label),
      call. = FALSE
    )
  }
  invisible()
}

require_lock_logical <- function(record, field, label = field) {
  value <- record[[field]]
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop(
      sprintf("Zak lockfile field `%s` must be logical.", label),
      call. = FALSE
    )
  }
  value
}

validate_lock_package <- function(package, label) {
  if (
    !is.character(package) ||
      length(package) != 1L ||
      is.na(package) ||
      !grepl("^[A-Za-z][A-Za-z0-9.]*$", package)
  ) {
    stop(
      sprintf("Zak lockfile field `%s` must be a package name.", label),
      call. = FALSE
    )
  }
  TRUE
}

new_zak_lock <- function(document, file) {
  dependencies <- lock_dependencies_data_frame(document$dependencies)
  platform <- structure(
    document$platform,
    class = c("zak_platform_facts", "list")
  )
  target <- document$target
  if (
    is.list(target$system_requirements) && !length(target$system_requirements)
  ) {
    target["system_requirements"] <- list(NULL)
  }
  acquisition <- document$acquisition
  if (is.list(acquisition) && !length(acquisition)) {
    acquisition <- NULL
  }
  dependency_sources <- document$dependency_sources
  if (is.null(dependency_sources)) {
    dependency_sources <- list()
  }
  structure(
    list(
      file = file,
      lockfile = document$lockfile,
      platform = platform,
      repositories = document$repositories,
      source = document$source,
      target = target,
      dependencies = dependencies,
      dependency_sources = dependency_sources,
      acquisition = acquisition,
      resolution = document$resolution
    ),
    class = c("zak_lock", "list")
  )
}

new_zak_project_lock <- function(document, file) {
  plans <- lapply(document$plans, new_zak_lock, file = file)
  structure(
    list(
      file = file,
      lockfile = document$lockfile,
      project = document$project,
      references = document$references,
      plans = plans
    ),
    class = c("zak_project_lock", "list")
  )
}

lock_dependencies_data_frame <- function(dependencies) {
  if (!length(dependencies)) {
    return(data.frame(
      Package = character(),
      Version = character(),
      Repository = character(),
      Status = character(),
      Direct = logical(),
      Constraint = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    Package = vapply(dependencies, `[[`, character(1L), "Package"),
    Version = vapply(dependencies, `[[`, character(1L), "Version"),
    Repository = vapply(
      dependencies,
      function(dependency) {
        if (is.null(dependency$Repository)) {
          NA_character_
        } else {
          dependency$Repository
        }
      },
      character(1L)
    ),
    Status = vapply(dependencies, `[[`, character(1L), "Status"),
    Direct = vapply(dependencies, `[[`, logical(1L), "Direct"),
    Constraint = vapply(dependencies, `[[`, character(1L), "Constraint"),
    stringsAsFactors = FALSE
  )
}

as_zak_lock <- function(lock) {
  if (inherits(lock, c("zak_lock", "zak_project_lock"))) {
    return(lock)
  }
  if (is.character(lock) && length(lock) == 1L && !is.na(lock)) {
    return(read_lock(lock))
  }
  stop("`lock` must be a zak lock object or lockfile path.", call. = FALSE)
}

#' @export
print.zak_project_lock <- function(x, ...) {
  cat(sprintf("zak project lockfile: %s\n", x$file))
  cat(sprintf("Schema: %s v%s\n", x$lockfile$name, x$lockfile$version))
  cat(sprintf("Project: %s\n", x$project$name))
  cat(sprintf("Dependencies: %d\n", length(x$references)))
  invisible(x)
}

plan_comparison_document <- function(plan) {
  document <- lock_document(plan)
  if (!is.null(document$acquisition)) {
    fields <- c("type", "url", "format", "size", "sha256")
    if (identical(document$acquisition$type, "git")) {
      fields <- c(
        "type",
        "url",
        "ref",
        "commit",
        "format",
        "size",
        "sha256"
      )
    }
    document$acquisition <- document$acquisition[fields]
  }
  document[c(
    "platform",
    "repositories",
    "source",
    "target",
    "dependencies",
    "dependency_sources",
    "acquisition"
  )]
}

lock_comparison_document <- function(lock) {
  target <- lock$target
  if (is.null(target$system_requirements)) {
    target["system_requirements"] <- list(NULL)
  }
  acquisition <- lock$acquisition
  if (!is.null(acquisition)) {
    acquisition$size <- as.numeric(acquisition$size)
    fields <- c("type", "url", "format", "size", "sha256")
    if (identical(acquisition$type, "git")) {
      fields <- c(
        "type",
        "url",
        "ref",
        "commit",
        "format",
        "size",
        "sha256"
      )
    }
    acquisition <- acquisition[fields]
  }
  list(
    platform = unclass(lock$platform),
    repositories = lock$repositories,
    source = lock$source,
    target = target,
    dependencies = lock_dependencies(lock$dependencies),
    dependency_sources = if (is.null(lock$dependency_sources)) {
      list()
    } else {
      lock$dependency_sources
    },
    acquisition = acquisition
  )
}

format_lock_value <- function(value) {
  jsonlite::toJSON(value, auto_unbox = TRUE, na = "null")
}

#' @export
print.zak_lock <- function(x, ...) {
  cat(sprintf("zak lockfile: %s\n", x$file))
  cat(sprintf("Schema: %s v%s\n", x$lockfile$name, x$lockfile$version))
  cat(sprintf(
    "Target: %s %s (%s)\n",
    x$target$package,
    x$target$version,
    x$target$type
  ))
  cat(sprintf("Dependencies: %d\n", nrow(x$dependencies)))
  invisible(x)
}

#' @export
print.zak_lock_comparison <- function(x, ...) {
  cat(sprintf("zak lock comparison: %s\n", x$status))
  if (nrow(x$changes)) {
    print(x$changes, row.names = FALSE)
  } else {
    cat("No drift detected.\n")
  }
  invisible(x)
}

# Parameter names whose value is a credential. A token is frequently carried
# in a name that is not obviously secret (`sig` for an Azure SAS URL,
# `X-Amz-Signature` for an S3 presigned URL), so this list is deliberately
# broader than "token" and friends. Over-redacting is recoverable: the lockfile
# records `<redacted>` and `validate_restore_lock()` refuses to restore from it.
lock_sensitive_parameters <- function() {
  paste(
    c(
      "token",
      "access[_-]?token",
      "refresh[_-]?token",
      "id[_-]?token",
      "api[_-]?key",
      "apikey",
      "key",
      "secret",
      "client[_-]?secret",
      "password",
      "passwd",
      "pwd",
      "auth",
      "authorization",
      "bearer",
      "credential",
      "session",
      "signature",
      "sig",
      "sas",
      "x-amz-signature",
      "x-amz-credential",
      "x-amz-security-token",
      "x-goog-signature"
    ),
    collapse = "|"
  )
}

redact_url_parameters <- function(value) {
  sensitive <- lock_sensitive_parameters()
  parameters <- strsplit(value, "&", fixed = TRUE)[[1L]]
  if (!length(parameters)) {
    return(value)
  }
  parameters <- vapply(
    parameters,
    function(parameter) {
      if (grepl(sprintf("^(?i:%s)=", sensitive), parameter, perl = TRUE)) {
        paste0(sub("=.*$", "", parameter), "=<redacted>")
      } else {
        parameter
      }
    },
    character(1L)
  )
  paste(parameters, collapse = "&")
}

# Replaces the whole userinfo rather than only a password: a token is often
# carried in the user position (`https://<token>@host/...`). Applies to any
# scheme so `ssh://user:secret@host` is covered, and leaves SCP-style
# `git@host:path` alone because it has no `://` and carries no secret.
redact_url_userinfo <- function(url) {
  sub(
    "^([A-Za-z][A-Za-z0-9+.-]*://)[^/@]+@",
    "\\1<redacted>@",
    url,
    perl = TRUE
  )
}

# Redacts userinfo in URLs that appear inside arbitrary text, such as a
# subprocess's diagnostics. Tool output routinely echoes the remote URL, which
# is exactly where an embedded credential would be.
redact_embedded_urls <- function(text) {
  gsub(
    "([A-Za-z][A-Za-z0-9+.-]*://)[^/@[:space:]]+@",
    "\\1<redacted>@",
    text,
    perl = TRUE
  )
}

redact_lock_url <- function(url) {
  url <- redact_url_userinfo(url)

  # Split at the *first* separator only. Splitting on every "?" used to drop
  # everything after a second one, silently recording a different URL than the
  # one that was fetched.
  fragment <- NULL
  hash <- regexpr("#", url, fixed = TRUE)
  if (hash > 0L) {
    fragment <- substring(url, hash + 1L)
    url <- substr(url, 1L, hash - 1L)
  }
  query <- NULL
  marker <- regexpr("?", url, fixed = TRUE)
  if (marker > 0L) {
    query <- substring(url, marker + 1L)
    url <- substr(url, 1L, marker - 1L)
  }

  if (!is.null(query)) {
    url <- paste0(url, "?", redact_url_parameters(query))
  }
  if (!is.null(fragment)) {
    url <- paste0(url, "#", redact_url_parameters(fragment))
  }
  url
}
