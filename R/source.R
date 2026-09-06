source_adapter <- function(reference) {
  if (!is.list(reference) || is.null(reference$type)) {
    stop("A source adapter needs a parsed package reference.", call. = FALSE)
  }
  adapter <- switch(
    reference$type,
    url = list(type = "url", prepare = prepare_url_source),
    local = list(type = "local", prepare = prepare_local_source),
    local_archive = list(
      type = "local_archive",
      prepare = prepare_local_archive_source
    ),
    git = list(type = "git", prepare = prepare_git_source),
    bioconductor = list(
      type = "bioconductor",
      prepare = prepare_bioconductor_source
    ),
    repository = list(
      type = "repository",
      prepare = prepare_repository_source
    ),
    stop(
      sprintf("No source adapter is registered for type: %s.", reference$type),
      call. = FALSE
    )
  )
  structure(adapter, class = c("zak_source_adapter", "list"))
}

prepare_package_source <- function(reference, lib, verbose, platform) {
  adapter <- source_adapter(reference)
  result <- adapter$prepare(reference, lib, verbose, platform)
  validate_source_result(result)
  result
}

new_source_result <- function(
  source,
  metadata,
  target,
  acquisition,
  artifact,
  repositories,
  configured
) {
  structure(
    list(
      source = source,
      metadata = metadata,
      target = target,
      acquisition = acquisition,
      artifact = artifact,
      repositories = repositories,
      configured = configured
    ),
    class = c("zak_source_result", "list")
  )
}

validate_source_result <- function(result) {
  required <- c(
    "source",
    "metadata",
    "target",
    "acquisition",
    "artifact",
    "repositories",
    "configured"
  )
  if (
    !inherits(result, "zak_source_result") ||
      !identical(names(result), required)
  ) {
    stop(
      "A source adapter returned an invalid preparation result.",
      call. = FALSE
    )
  }
  invisible(result)
}

# Reads a package DESCRIPTION from a directory. Both the local-directory and
# Git adapters need the same guards: a missing file otherwise surfaces as a raw
# file-connection warning, and an empty one as a subscript error.
read_source_description <- function(directory, subject) {
  path <- file.path(directory, "DESCRIPTION")
  if (!file.exists(path)) {
    stop(
      sprintf(
        "%s must contain a top-level package DESCRIPTION file: %s.",
        subject,
        directory
      ),
      call. = FALSE
    )
  }
  fields <- tryCatch(read.dcf(path), error = function(error) NULL)
  if (is.null(fields) || !nrow(fields) || !ncol(fields)) {
    stop(
      sprintf("%s has an unreadable DESCRIPTION file: %s.", subject, path),
      call. = FALSE
    )
  }
  fields[1L, , drop = TRUE]
}

prepare_url_source <- function(reference, lib, verbose, platform) {
  report_step(verbose, "Downloading package archive.")
  artifact <- acquire_archive(reference$url)
  cleanup <- TRUE
  on.exit(
    {
      if (cleanup) {
        release_acquired_artifact(artifact)
      }
    },
    add = TRUE
  )

  report_step(verbose, "Inspecting package archive.")
  metadata <- inspect_archive(artifact$path, source = reference$url, verbose = verbose)
  report_step(verbose, "Checking archive compatibility.")
  validate_archive_compatibility(metadata, platform)
  target <- build_target_index(metadata)
  result <- new_source_result(
    source = reference,
    metadata = metadata,
    target = target,
    acquisition = archive_acquisition(artifact),
    artifact = artifact,
    repositories = NULL,
    configured = NULL
  )
  cleanup <- FALSE
  result
}

prepare_local_source <- function(reference, lib, verbose, platform) {
  report_step(verbose, "Inspecting local package directory.")
  fields <- read_source_description(
    reference$path,
    "A local package directory"
  )
  metadata <- list(
    package = required_description_field(fields, "Package"),
    version = required_description_field(fields, "Version"),
    type = "source",
    format = "directory",
    fields = fields
  )
  new_source_result(
    source = reference,
    metadata = metadata,
    target = build_target_index(metadata),
    acquisition = NULL,
    artifact = NULL,
    repositories = NULL,
    configured = NULL
  )
}

prepare_local_archive_source <- function(reference, lib, verbose, platform) {
  report_step(verbose, "Inspecting local package archive.")
  artifact <- acquire_local_archive(reference$path)
  cleanup <- TRUE
  on.exit(
    {
      if (cleanup) {
        release_acquired_artifact(artifact)
      }
    },
    add = TRUE
  )
  if (!identical(artifact$format, reference$format)) {
    stop(
      sprintf(
        "Local archive extension indicates `%s`, but its content is `%s`.",
        reference$format,
        artifact$format
      ),
      call. = FALSE
    )
  }

  report_step(verbose, "Inspecting package archive.")
  metadata <- inspect_archive(artifact$path, source = reference$path, verbose = verbose)
  report_step(verbose, "Checking archive compatibility.")
  validate_archive_compatibility(metadata, platform)
  target <- build_target_index(metadata)
  result <- new_source_result(
    source = reference,
    metadata = metadata,
    target = target,
    acquisition = archive_acquisition(artifact),
    artifact = artifact,
    repositories = NULL,
    configured = NULL
  )
  cleanup <- FALSE
  result
}

prepare_git_source <- function(reference, lib, verbose, platform) {
  report_step(verbose, "Cloning Git repository.")
  artifact <- acquire_git_repository(reference$url, reference$ref)
  cleanup <- TRUE
  on.exit(
    {
      if (cleanup) {
        release_acquired_artifact(artifact)
      }
    },
    add = TRUE
  )
  report_step(verbose, "Inspecting Git package metadata.")
  fields <- read_source_description(artifact$path, "A Git checkout")
  metadata <- list(
    package = required_description_field(fields, "Package"),
    version = required_description_field(fields, "Version"),
    type = "source",
    format = "directory",
    fields = fields
  )
  result <- new_source_result(
    source = reference,
    metadata = metadata,
    target = build_target_index(metadata),
    acquisition = git_acquisition(artifact),
    artifact = artifact,
    repositories = NULL,
    configured = NULL
  )
  cleanup <- FALSE
  result
}

prepare_bioconductor_source <- function(reference, lib, verbose, platform) {
  report_step(verbose, "Reading Bioconductor repositories.")
  repositories <- bioc_repositories()
  configured <- configured_source_index(repositories)
  package <- reference$package
  if (!package %in% configured[, "Package"]) {
    stop(
      sprintf(
        "Bioconductor package '%s' is not available in the configured repositories.",
        package
      ),
      call. = FALSE
    )
  }
  target <- configured[package, , drop = FALSE]
  metadata <- list(
    package = package,
    version = unname(target[1L, "Version"]),
    type = "source",
    format = "repository",
    fields = target[1L, , drop = TRUE]
  )
  new_source_result(
    source = reference,
    metadata = metadata,
    target = target,
    acquisition = NULL,
    artifact = NULL,
    repositories = repositories,
    configured = configured
  )
}

prepare_repository_source <- function(reference, lib, verbose, platform) {
  report_step(verbose, "Reading configured package repositories.")
  repositories <- configured_repositories()
  configured <- configured_source_index(repositories)
  package <- reference$package
  if (!package %in% configured[, "Package"]) {
    stop(
      sprintf(
        "Package '%s' is not available in the configured repositories.",
        package
      ),
      call. = FALSE
    )
  }

  target <- configured[package, , drop = FALSE]
  metadata <- list(
    package = package,
    version = unname(target[1L, "Version"]),
    type = "source",
    format = "repository",
    fields = target[1L, , drop = TRUE]
  )
  new_source_result(
    source = reference,
    metadata = metadata,
    target = target,
    acquisition = NULL,
    artifact = NULL,
    repositories = repositories,
    configured = configured
  )
}
