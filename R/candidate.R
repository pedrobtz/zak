new_package_candidate <- function(
  package,
  version,
  source,
  repository,
  platform,
  type,
  format,
  fields,
  acquisition = NULL
) {
  package <- normalize_candidate_package(package, "package")
  version <- normalize_candidate_text(version, "version")
  source <- normalize_candidate_source(source)
  repository <- normalize_candidate_repository(repository)
  platform <- normalize_candidate_platform(platform)
  type <- normalize_candidate_type(type)
  format <- normalize_candidate_format(format)
  if (!is.null(acquisition)) {
    acquisition <- normalize_candidate_acquisition(acquisition)
  }

  size <- if (is.null(acquisition)) NULL else as.numeric(acquisition$size)
  provenance <- list(
    source = source$type,
    reference = source$reference,
    repository = repository,
    acquired_from = if (is.null(acquisition)) {
      NULL
    } else {
      acquisition$url
    }
  )
  if (identical(source$type, "git")) {
    provenance$ref <- source$ref
    provenance$commit <- acquisition$commit
  }

  structure(
    list(
      package = package,
      version = version,
      source = source,
      repository = repository,
      platform = platform,
      r_compatibility = list(
        r_version = platform$r_version,
        constraint = candidate_r_constraint(fields)
      ),
      type = type,
      format = format,
      hashes = if (is.null(acquisition)) {
        list()
      } else {
        list(sha256 = acquisition$sha256)
      },
      size = size,
      system_requirements = lock_system_requirements(fields),
      provenance = provenance
    ),
    class = c("zak_candidate", "list")
  )
}

plan_candidates <- function(
  source,
  metadata,
  dependencies,
  available,
  platform,
  acquisition,
  dependency_sources = list()
) {
  target_repository <- candidate_target_repository(source, metadata)
  dependency_candidates <- lapply(
    dependencies,
    function(package) {
      position <- match(package, available[, "Package"])
      fields <- available[position, , drop = TRUE]
      record <- dependency_sources[[package]]
      if (is.null(record)) {
        dependency_source <- list(type = "repository", package = package)
        dependency_version <- fields[["Version"]]
        dependency_repository <- candidate_repository_value(fields)
        dependency_type <- "source"
        dependency_format <- "repository"
        dependency_acquisition <- NULL
        dependency_fields <- fields
      } else {
        dependency_source <- record$source
        dependency_version <- record$metadata$version
        dependency_repository <- candidate_target_repository(
          record$source,
          record$metadata
        )
        dependency_type <- record$metadata$type
        dependency_format <- record$metadata$format
        dependency_acquisition <- record$acquisition
        dependency_fields <- record$metadata$fields
      }
      new_package_candidate(
        package = package,
        version = dependency_version,
        source = dependency_source,
        repository = dependency_repository,
        platform = platform,
        type = dependency_type,
        format = dependency_format,
        fields = dependency_fields,
        acquisition = dependency_acquisition
      )
    }
  )
  names(dependency_candidates) <- dependencies

  list(
    target = new_package_candidate(
      package = metadata$package,
      version = metadata$version,
      source = source,
      repository = target_repository,
      platform = platform,
      type = metadata$type,
      format = metadata$format,
      fields = metadata$fields,
      acquisition = acquisition
    ),
    dependencies = dependency_candidates
  )
}

candidate_target_repository <- function(source, metadata) {
  if (!source$type %in% c("repository", "bioconductor")) {
    return(NULL)
  }
  candidate_repository_value(metadata$fields)
}

candidate_repository_value <- function(fields) {
  value <- unname(fields["Repository"])
  if (!length(value) || is.na(value) || !nzchar(trimws(value))) {
    return(NULL)
  }
  normalize_candidate_repository(value)
}

normalize_candidate_source <- function(source) {
  if (!is.list(source) || is.null(source$type)) {
    stop("A package candidate needs a normalized source.", call. = FALSE)
  }
  type <- normalize_candidate_text(source$type, "source type")
  if (identical(type, "local_archive")) {
    format <- normalize_candidate_text(source$format, "archive format")
    if (!format %in% c("tar.gz", "zip")) {
      stop(
        sprintf("Unknown local archive format: %s.", format),
        call. = FALSE
      )
    }
    return(list(
      type = type,
      reference = normalize_candidate_path(source$path),
      format = format
    ))
  }
  if (identical(type, "git")) {
    return(list(
      type = type,
      reference = normalize_git_candidate_url(source$url),
      ref = normalize_candidate_text(source$ref, "Git ref")
    ))
  }
  if (identical(type, "bioconductor")) {
    return(list(
      type = type,
      reference = normalize_candidate_package(
        source$package,
        "source package"
      )
    ))
  }
  reference <- switch(
    type,
    url = normalize_candidate_url(source$url),
    local = normalize_candidate_path(source$path),
    repository = normalize_candidate_package(source$package, "source package"),
    stop(sprintf("Unknown candidate source type: %s.", type), call. = FALSE)
  )
  list(type = type, reference = reference)
}

normalize_candidate_url <- function(url) {
  url <- normalize_candidate_text(url, "source URL")
  validate_install_url(url)
  url
}

normalize_candidate_path <- function(path) {
  path <- normalize_candidate_text(path, "source path")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

normalize_candidate_repository <- function(repository) {
  if (is.null(repository)) {
    return(NULL)
  }
  normalize_candidate_text(repository, "repository")
}

normalize_candidate_platform <- function(platform) {
  if (!inherits(platform, "zak_platform_facts")) {
    stop(
      "Package candidates need zak platform facts.",
      call. = FALSE
    )
  }
  required <- c("os", "architecture", "r_version", "package_type")
  if (!identical(names(platform), required)) {
    stop("Package candidates need complete platform facts.", call. = FALSE)
  }
  structure(unclass(platform), class = c("zak_platform_facts", "list"))
}

normalize_candidate_type <- function(type) {
  type <- normalize_candidate_text(type, "candidate type")
  if (!type %in% c("source", "binary")) {
    stop(sprintf("Unknown package candidate type: %s.", type), call. = FALSE)
  }
  type
}

normalize_candidate_format <- function(format) {
  format <- normalize_candidate_text(format, "candidate format")
  if (!format %in% c("tar.gz", "zip", "directory", "repository")) {
    stop(
      sprintf("Unknown package candidate format: %s.", format),
      call. = FALSE
    )
  }
  format
}

normalize_candidate_acquisition <- function(acquisition) {
  if (!is.list(acquisition)) {
    stop("Package candidate acquisition must be a record.", call. = FALSE)
  }
  result <- list(
    type = if (is.null(acquisition$type)) {
      "archive"
    } else {
      normalize_candidate_text(acquisition$type, "acquisition type")
    },
    url = normalize_candidate_acquisition_url(acquisition$url),
    format = normalize_candidate_text(acquisition$format, "archive format"),
    size = normalize_candidate_size(acquisition$size),
    sha256 = normalize_candidate_sha256(acquisition$sha256)
  )
  if (identical(result$type, "git")) {
    result$ref <- normalize_candidate_text(acquisition$ref, "Git ref")
    result$commit <- normalize_candidate_text(
      acquisition$commit,
      "Git commit"
    )
  }
  result
}

normalize_candidate_sha256 <- function(sha256) {
  if (!is_sha256(sha256)) {
    stop("Package candidate SHA-256 checksums must be valid.", call. = FALSE)
  }
  tolower(sha256)
}

normalize_candidate_acquisition_url <- function(url) {
  url <- normalize_candidate_text(url, "acquisition URL")
  if (
    !grepl("^(https?|file)://", url, ignore.case = TRUE) &&
      !is_git_candidate_url(url)
  ) {
    stop(
      "Package candidate acquisition URL must be an HTTP(S), file, or Git URL.",
      call. = FALSE
    )
  }
  url
}

normalize_git_candidate_url <- function(url) {
  url <- normalize_candidate_text(url, "Git URL")
  validate_git_url(url)
  url
}

is_git_candidate_url <- function(url) {
  isTRUE(
    tryCatch(
      {
        validate_git_url(url)
        TRUE
      },
      error = function(error) FALSE
    )
  )
}

normalize_candidate_size <- function(size) {
  if (
    !is.numeric(size) ||
      length(size) != 1L ||
      is.na(size) ||
      !is.finite(size) ||
      size < 0
  ) {
    stop("Package candidate size must be a non-negative number.", call. = FALSE)
  }
  size
}

normalize_candidate_package <- function(package, label) {
  package <- normalize_candidate_text(package, label)
  if (!grepl("^[A-Za-z][A-Za-z0-9.]*$", package)) {
    stop(sprintf("Candidate %s must be a package name.", label), call. = FALSE)
  }
  package
}

normalize_candidate_text <- function(value, label) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value))
  ) {
    stop(sprintf("Candidate %s must be non-empty text.", label), call. = FALSE)
  }
  trimws(value)
}

candidate_r_constraint <- function(fields) {
  declaration <- unname(fields["Depends"])
  if (!length(declaration) || is.na(declaration) || !nzchar(declaration)) {
    return("")
  }
  requirements <- split_dependency_field(declaration)
  requirements <- Filter(
    function(requirement) identical(requirement$name, "R"),
    requirements
  )
  if (!length(requirements)) {
    return("")
  }
  paste(
    vapply(
      requirements,
      function(requirement) {
        paste(requirement$op, as.character(requirement$version))
      },
      character(1L)
    ),
    collapse = ", "
  )
}
