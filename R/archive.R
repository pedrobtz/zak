download_archive <- function(url) {
  download <- tempfile("zak-download-")
  on.exit(unlink(download, force = TRUE), add = TRUE)

  # `download.file()` reports failure differently depending on the method in
  # use: some raise a condition, others only return a non-zero status. Both
  # have to be handled, or an error page gets treated as a package archive.
  status <- tryCatch(
    suppressWarnings(utils::download.file(url, download, mode = "wb")),
    error = function(error) {
      stop(
        sprintf(
          "Zak could not download %s: %s",
          redact_lock_url(url),
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    stop(
      sprintf(
        "Zak could not download %s: the transfer reported status %s.",
        redact_lock_url(url),
        format(status, trim = TRUE)
      ),
      call. = FALSE
    )
  }
  if (!file.exists(download) || !isTRUE(file.info(download)$size > 0)) {
    stop(
      sprintf("Zak downloaded no content from %s.", redact_lock_url(url)),
      call. = FALSE
    )
  }

  format <- archive_format(download, source = url)
  archive <- paste0(download, archive_extension(format))

  if (!file.rename(download, archive)) {
    copied <- file.copy(download, archive, overwrite = TRUE)
    if (!copied) {
      unlink(archive, force = TRUE)
      stop(
        sprintf(
          "R could not preserve the format of the archive downloaded from %s.",
          redact_lock_url(url)
        ),
        call. = FALSE
      )
    }
  }

  archive
}

acquire_archive <- function(url) {
  cached <- cached_archive_artifact(url)
  if (!is.null(cached)) {
    return(cached)
  }
  if (zak_offline()) {
    stop(
      sprintf(
        "Offline mode is enabled and no valid cached archive is available for %s.",
        redact_lock_url(url)
      ),
      call. = FALSE
    )
  }

  archive <- download_archive(url)
  cleanup <- TRUE
  on.exit(
    {
      if (cleanup) {
        unlink(archive, force = TRUE)
      }
    },
    add = TRUE
  )

  format <- archive_format(archive, source = url)
  info <- file.info(archive)
  if (!file.exists(archive) || is.na(info$size)) {
    stop(
      sprintf(
        "R could not inspect the archive acquired from %s.",
        redact_lock_url(url)
      ),
      call. = FALSE
    )
  }

  cached <- cache_archive_artifact(url, archive, format)
  if (!is.null(cached)) {
    return(cached)
  }

  artifact <- new_archive_artifact(
    url,
    archive,
    format,
    retrieved_at = Sys.time(),
    method = "download.file",
    owned = TRUE
  )
  cleanup <- FALSE
  artifact
}

new_archive_artifact <- function(
  url,
  path,
  format,
  retrieved_at,
  method,
  owned,
  sha256 = sha256_file(path)
) {
  info <- file.info(path)
  if (!file.exists(path) || is.na(info$size)) {
    stop("R could not inspect the acquired package archive.", call. = FALSE)
  }
  if (!is_sha256(sha256)) {
    stop("Package archive SHA-256 checksums must be valid.", call. = FALSE)
  }
  structure(
    list(
      url = url,
      path = normalizePath(path, mustWork = TRUE),
      format = format,
      size = unname(info$size),
      retrieved_at = retrieved_at,
      method = method,
      owned = owned,
      sha256 = tolower(sha256)
    ),
    class = c("zak_artifact", "list")
  )
}

release_acquired_artifact <- function(artifact) {
  if (!inherits(artifact, "zak_artifact")) {
    stop("`artifact` must be a zak acquired artifact.", call. = FALSE)
  }
  if (isTRUE(artifact$owned)) {
    unlink(
      artifact$path,
      recursive = isTRUE(artifact$directory),
      force = TRUE
    )
  }
  invisible()
}

# `expected` is deliberately required. Defaulting it to the artifact's own
# checksum made every caller that omitted it compare the artifact against a
# hash just computed from the same bytes, which always passes and, for a Git
# checkout, hashed the whole tree a second time. Verification is only
# meaningful against an independently recorded expectation, which is why it
# happens at install time against the plan or lockfile.
verify_acquired_artifact <- function(artifact, expected) {
  if (!inherits(artifact, "zak_artifact") || !is_sha256(expected)) {
    stop("Acquired package artifacts need a SHA-256 checksum.", call. = FALSE)
  }
  actual <- if (isTRUE(artifact$directory)) {
    git_checkout_sha256(artifact$path)
  } else {
    sha256_file(artifact$path)
  }
  if (!identical(actual, tolower(expected))) {
    stop(
      "Acquired package artifact failed SHA-256 verification.",
      call. = FALSE
    )
  }
  invisible()
}

acquire_local_archive <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  format <- archive_format(path, source = path)
  info <- file.info(path)
  if (!file.exists(path) || is.na(info$size)) {
    stop("R could not inspect the local package archive.", call. = FALSE)
  }
  structure(
    list(
      url = file_url(path),
      path = path,
      format = format,
      size = unname(info$size),
      retrieved_at = Sys.time(),
      method = "local",
      owned = FALSE,
      sha256 = sha256_file(path)
    ),
    class = c("zak_artifact", "list")
  )
}

archive_acquisition <- function(artifact) {
  if (!inherits(artifact, "zak_artifact")) {
    stop("`artifact` must be a zak acquired artifact.", call. = FALSE)
  }
  artifact[c("url", "format", "size", "retrieved_at", "method", "sha256")]
}

# `source` names where the bytes came from, so a failure says which URL or
# path was being read rather than only that something was unreadable.
archive_source_suffix <- function(source) {
  if (is.null(source) || !length(source) || !nzchar(source)) {
    return("")
  }
  sprintf(" (from %s)", redact_lock_url(source))
}

archive_format <- function(archive, source = NULL) {
  connection <- file(archive, open = "rb")
  on.exit(close(connection), add = TRUE)
  signature <- as.integer(readBin(connection, "raw", n = 4L))

  if (length(signature) >= 2L && identical(signature[1:2], c(31L, 139L))) {
    return("tar.gz")
  }

  zip_signatures <- list(
    c(80L, 75L, 3L, 4L),
    c(80L, 75L, 5L, 6L),
    c(80L, 75L, 7L, 8L)
  )
  if (
    length(signature) == 4L &&
      any(vapply(
        zip_signatures,
        identical,
        logical(1L),
        signature
      ))
  ) {
    return("zip")
  }

  stop(
    sprintf(
      "R could not identify the content as a tar.gz or ZIP archive%s.",
      archive_source_suffix(source)
    ),
    call. = FALSE
  )
}

archive_extension <- function(format) {
  switch(format, tar.gz = ".tar.gz", zip = ".zip")
}

local_archive_format <- function(path) {
  if (grepl("[.]tar[.]gz$", path, ignore.case = TRUE)) {
    return("tar.gz")
  }
  if (grepl("[.]zip$", path, ignore.case = TRUE)) {
    return("zip")
  }
  NULL
}

archive_members <- function(archive, format) {
  members <- switch(
    format,
    tar.gz = utils::untar(archive, list = TRUE),
    zip = utils::unzip(archive, list = TRUE)$Name
  )
  gsub("\\\\", "/", members)
}

inspect_archive <- function(archive, source = NULL, verbose = FALSE) {
  format <- archive_format(archive, source = source)
  members <- archive_members(archive, format)
  descriptions <- grep("^[^/]+/DESCRIPTION$", members, value = TRUE)
  if (!length(descriptions)) {
    stop(
      sprintf(
        "R could not find a top-level package DESCRIPTION file in the archive%s.",
        archive_source_suffix(source)
      ),
      call. = FALSE
    )
  }
  if (length(descriptions) > 1L) {
    stop(
      sprintf(
        "R found multiple top-level package DESCRIPTION files in the archive%s.",
        archive_source_suffix(source)
      ),
      call. = FALSE
    )
  }
  description_member <- descriptions[[1L]]

  root <- sub("/DESCRIPTION$", "", description_member)
  if (root %in% c(".", "..")) {
    stop(
      sprintf(
        "R could not identify the package directory in the archive%s.",
        archive_source_suffix(source)
      ),
      call. = FALSE
    )
  }

  description <- read_archive_description(
    archive,
    format,
    description_member
  )
  fields <- description[1L, , drop = TRUE]
  package <- required_description_field(fields, "Package")
  version <- required_description_field(fields, "Version")
  type <- archive_package_type(fields)

  report_step(
    verbose,
    sprintf("Package: %s %s (%s)", sQuote(package), version, type)
  )
  report_system_requirements(fields, verbose)

  list(
    package = package,
    version = version,
    type = type,
    format = format,
    fields = fields,
    root = root
  )
}

read_archive_description <- function(archive, format, member) {
  if (identical(format, "zip")) {
    connection <- unz(archive, member, open = "rb")
    on.exit(close(connection), add = TRUE)
    return(read.dcf(connection))
  }

  extract_dir <- tempfile("zak-description-")
  dir.create(extract_dir)
  on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)

  utils::untar(archive, files = member, exdir = extract_dir)
  description_path <- file.path(extract_dir, member)
  if (!file.exists(description_path)) {
    stop("R could not extract the package DESCRIPTION file.", call. = FALSE)
  }
  read.dcf(description_path)
}

extract_source_archive <- function(archive, metadata) {
  members <- archive_members(archive, metadata$format)
  package_members <- members[
    members == metadata$root |
      startsWith(members, paste0(metadata$root, "/"))
  ]
  unsafe <- grepl("(^|/)\\.\\.(/|$)", package_members) |
    grepl("^/|^[[:alpha:]]:/", package_members)
  if (any(unsafe)) {
    stop(
      "R refused to extract unsafe paths from the package archive.",
      call. = FALSE
    )
  }

  extract_dir <- tempfile("zak-source-")
  dir.create(extract_dir)
  extracted <- FALSE
  on.exit(
    {
      if (!extracted) {
        unlink(extract_dir, recursive = TRUE, force = TRUE)
      }
    },
    add = TRUE
  )
  switch(
    metadata$format,
    tar.gz = utils::untar(
      archive,
      files = package_members,
      exdir = extract_dir
    ),
    zip = utils::unzip(
      archive,
      files = package_members,
      exdir = extract_dir
    )
  )

  package_dir <- file.path(extract_dir, metadata$root)
  if (!file.exists(file.path(package_dir, "DESCRIPTION"))) {
    stop(
      "R could not extract the source package from the archive.",
      call. = FALSE
    )
  }

  extracted <- TRUE
  list(root = extract_dir, package = package_dir)
}

required_description_field <- function(fields, name) {
  value <- unname(fields[name])
  if (!length(value) || is.na(value) || !nzchar(trimws(value))) {
    stop(
      sprintf("The package DESCRIPTION is missing the %s field.", name),
      call. = FALSE
    )
  }
  trimws(value)
}

archive_package_type <- function(fields) {
  built <- unname(fields["Built"])
  if (length(built) && !is.na(built) && nzchar(trimws(built))) {
    "binary"
  } else {
    "source"
  }
}

report_system_requirements <- function(fields, verbose = FALSE) {
  requirements <- unname(fields["SystemRequirements"])
  if (
    !length(requirements) ||
      is.na(requirements) ||
      !nzchar(trimws(requirements))
  ) {
    return(invisible())
  }

  requirements <- gsub("[\r\n]+[[:space:]]*", " ", trimws(requirements))
  report_step(verbose, paste0("System requirements: ", requirements))
  invisible()
}
