#' Inspect the zak artifact cache
#'
#' `cache_info()` returns one row for each cached HTTP(S) archive or
#' commit-resolved Git source tree. The `valid` column checks the SHA-256
#' sidecar and the cached payload. Existing cache entries without metadata are
#' still listed, with a missing `source` value.
#'
#' @param path `NULL`, or a cache directory to inspect. The default uses the
#'   directory configured by `options(zak.cache)` or R's user cache directory.
#'
#' @return A data frame with cache type, key, redacted source, path, size,
#'   modification time, SHA-256 checksum, and validity.
#' @export
#'
#' @examples
#' \dontrun{
#' zak::cache_info()
#' }
cache_info <- function(path = NULL) {
  cache_entry_table(cache_inspection_root(path))
}

# Builds the reported table. `entries` may be supplied by a caller that has
# already selected a subset, so only those are checksummed: validating an entry
# reads the whole payload, and `cache_clean()` selects on timestamps alone.
cache_entry_table <- function(root, entries = NULL) {
  if (is.null(entries)) {
    entries <- cache_entries(root)
  }
  if (!length(entries)) {
    return(empty_cache_info())
  }

  rows <- lapply(entries, function(entry) {
    info <- file.info(entry$path)
    checksum <- cache_file_checksum(entry$path)
    metadata <- cache_file_metadata(entry$path)
    actual <- if (is.null(checksum)) {
      NULL
    } else {
      tryCatch(sha256_file(entry$path), error = function(error) NULL)
    }
    data.frame(
      type = entry$type,
      key = entry$key,
      source = if (is.null(metadata$source)) NA_character_ else metadata$source,
      path = normalizePath(entry$path, mustWork = TRUE),
      size = unname(info$size),
      modified = info$mtime,
      sha256 = if (is.null(checksum)) NA_character_ else checksum,
      valid = !is.null(actual) && identical(actual, checksum),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Remove entries from the zak artifact cache
#'
#' `cache_clean()` removes cached HTTP(S) archives and commit-resolved Git
#' source trees. By default it removes all entries in the selected cache. Set
#' `max_age` to retain entries modified within the given number of seconds.
#' Cache reads do not update that timestamp, so it reflects when an entry was
#' stored rather than when it was last used. The returned data frame describes
#' entries selected for removal; use `dry_run = TRUE` to preview the selection
#' without changing the cache.
#'
#' @param path `NULL`, or a cache directory to clean. The default uses the
#'   directory configured by `options(zak.cache)` or R's user cache directory.
#' @param max_age A non-negative number of seconds. Entries modified more
#'   recently than this age are retained. `Inf` removes all entries.
#' @param dry_run A single logical value. When `TRUE`, report selected entries
#'   without deleting them.
#'
#' @return A data frame describing the selected entries.
#' @export
#'
#' @examples
#' \dontrun{
#' zak::cache_clean(max_age = 30 * 24 * 60 * 60, dry_run = TRUE)
#' zak::cache_clean(max_age = 30 * 24 * 60 * 60)
#' }
cache_clean <- function(path = NULL, max_age = Inf, dry_run = FALSE) {
  root <- cache_inspection_root(path)
  validate_cache_max_age(max_age)
  validate_cache_logical(dry_run, "dry_run")
  entries <- cache_entries(root)
  if (!length(entries)) {
    return(empty_cache_info())
  }

  # Select on modification time before checksumming, so pruning by age does not
  # read every cached payload just to decide what to delete.
  modified <- as.POSIXct(vapply(
    entries,
    function(entry) as.numeric(file.info(entry$path)$mtime),
    numeric(1L)
  ), origin = "1970-01-01", tz = "")
  selected <- if (is.infinite(max_age)) {
    rep(TRUE, length(entries))
  } else {
    modified <= Sys.time() - max_age
  }
  selected_entries <- cache_entry_table(root, entries[selected])
  if (!isTRUE(dry_run) && nrow(selected_entries)) {
    for (entry in selected_entries$path) {
      unlink(
        c(entry, cache_checksum_file(entry), cache_metadata_file(entry)),
        force = TRUE
      )
    }
  }
  selected_entries
}

zak_cache_root <- function() {
  configured <- getOption("zak.cache")
  if (is.null(configured)) {
    return(tools::R_user_dir("zak", which = "cache"))
  }
  if (
    !is.character(configured) ||
      length(configured) != 1L ||
      is.na(configured) ||
      !nzchar(configured)
  ) {
    stop(
      "`options(zak.cache)` must be a single cache directory path.",
      call. = FALSE
    )
  }
  path.expand(configured)
}

cache_inspection_root <- function(path) {
  root <- if (is.null(path)) zak_cache_root() else path
  if (
    !is.character(root) ||
      length(root) != 1L ||
      is.na(root) ||
      !nzchar(root)
  ) {
    stop("`path` must be a single cache directory path.", call. = FALSE)
  }
  path.expand(root)
}

cache_entries <- function(root) {
  types <- c(archives = "archive", git = "git")
  entries <- list()
  for (directory in names(types)) {
    path <- file.path(root, directory)
    if (!dir.exists(path)) {
      next
    }
    pattern <- if (identical(directory, "archives")) {
      "[.]artifact$"
    } else {
      "[.]tar[.]gz$"
    }
    files <- list.files(path, pattern = pattern, full.names = TRUE)
    files <- files[file.info(files)$isdir %in% FALSE]
    if (!length(files)) {
      next
    }
    entries <- c(
      entries,
      lapply(files, function(file) {
        extension <- if (identical(directory, "archives")) {
          "[.]artifact$"
        } else {
          "[.]tar[.]gz$"
        }
        list(
          type = unname(types[[directory]]),
          key = sub(extension, "", basename(file)),
          path = file
        )
      })
    )
  }
  entries
}

empty_cache_info <- function() {
  data.frame(
    type = character(),
    key = character(),
    source = character(),
    path = character(),
    size = numeric(),
    modified = as.POSIXct(character()),
    sha256 = character(),
    valid = logical(),
    stringsAsFactors = FALSE
  )
}

validate_cache_max_age <- function(max_age) {
  valid <- is.numeric(max_age) &&
    length(max_age) == 1L &&
    !is.na(max_age) &&
    max_age >= 0
  if (!valid) {
    stop("`max_age` must be one non-negative number or `Inf`.", call. = FALSE)
  }
  invisible()
}

validate_cache_logical <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop(sprintf("`%s` must be `TRUE` or `FALSE`.", name), call. = FALSE)
  }
  invisible()
}

zak_offline <- function() {
  value <- getOption("zak.offline", FALSE)
  validate_cache_logical(value, "options(zak.offline)")
  isTRUE(value)
}

zak_cache_directory <- function(type) {
  path <- file.path(zak_cache_root(), type)
  created <- tryCatch(
    {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      dir.exists(path)
    },
    warning = function(warning) FALSE,
    error = function(error) FALSE
  )
  if (!created) {
    return(NULL)
  }
  normalizePath(path, mustWork = TRUE)
}

zak_cache_key <- function(...) {
  key_file <- tempfile("zak-cache-key-")
  on.exit(unlink(key_file, force = TRUE), add = TRUE)
  writeLines(paste(..., collapse = "\n"), key_file, useBytes = TRUE)
  unname(tools::md5sum(key_file))
}

sha256_file <- function(path) {
  checksum <- digest::digest(path, algo = "sha256", file = TRUE)
  if (!is_sha256(checksum)) {
    stop("R could not calculate a SHA-256 checksum.", call. = FALSE)
  }
  tolower(checksum)
}

is_sha256 <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    grepl("^[0-9a-f]{64}$", value, ignore.case = TRUE)
}

git_checkout_sha256 <- function(path) {
  files <- list.files(
    path,
    recursive = TRUE,
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )
  files <- normalizePath(files, mustWork = TRUE)
  files <- files[!file.info(files)$isdir]
  files <- files[!grepl("[/\\\\][.]git([/\\\\]|$)", files)]
  relative <- substring(files, nchar(normalizePath(path, mustWork = TRUE)) + 2L)
  order <- order(relative)
  manifest <- tempfile("zak-git-manifest-")
  on.exit(unlink(manifest, force = TRUE), add = TRUE)
  writeLines(
    paste(
      relative[order],
      vapply(files[order], sha256_file, character(1L)),
      sep = "\t"
    ),
    manifest,
    useBytes = TRUE
  )
  sha256_file(manifest)
}

zak_cache_file <- function(type, key, extension) {
  directory <- zak_cache_directory(type)
  if (is.null(directory)) {
    return(NULL)
  }
  file.path(directory, paste0(key, extension))
}

cache_store_file <- function(source, destination, metadata = list()) {
  if (is.null(destination)) {
    return(FALSE)
  }
  temporary <- tempfile("zak-cache-", tmpdir = dirname(destination))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  if (!file.copy(source, temporary, overwrite = TRUE)) {
    return(FALSE)
  }
  checksum <- sha256_file(temporary)
  if (file.rename(temporary, destination)) {
    stored <- cache_store_checksum(destination, checksum)
    cache_store_metadata(destination, metadata)
    return(stored)
  }
  if (!file.copy(temporary, destination, overwrite = TRUE)) {
    return(FALSE)
  }
  stored <- cache_store_checksum(destination, checksum)
  cache_store_metadata(destination, metadata)
  stored
}

cache_checksum_file <- function(path) {
  paste0(path, ".sha256")
}

cache_metadata_file <- function(path) {
  paste0(path, ".meta")
}

cache_file_checksum <- function(path) {
  checksum_file <- cache_checksum_file(path)
  if (!file.exists(checksum_file)) {
    return(NULL)
  }
  checksum <- tryCatch(
    readLines(checksum_file, warn = FALSE, encoding = "UTF-8"),
    error = function(error) character()
  )
  if (length(checksum) != 1L || !is_sha256(checksum)) {
    return(NULL)
  }
  tolower(checksum)
}

cache_file_metadata <- function(path) {
  metadata_file <- cache_metadata_file(path)
  if (!file.exists(metadata_file)) {
    return(list())
  }
  metadata <- tryCatch(
    read.dcf(metadata_file),
    error = function(error) NULL
  )
  if (is.null(metadata) || !nrow(metadata)) {
    return(list())
  }
  as.list(metadata[1L, , drop = TRUE])
}

cache_store_metadata <- function(path, metadata) {
  if (!length(metadata)) {
    return(invisible(FALSE))
  }
  values <- lapply(metadata, as.character)
  document <- as.data.frame(values, stringsAsFactors = FALSE)
  temporary <- tempfile("zak-cache-metadata-", tmpdir = dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  stored <- tryCatch(
    {
      write.dcf(document, temporary)
      destination <- cache_metadata_file(path)
      if (file.rename(temporary, destination)) {
        TRUE
      } else {
        file.copy(temporary, destination, overwrite = TRUE)
      }
    },
    error = function(error) FALSE
  )
  invisible(stored)
}

cache_file_is_valid <- function(path) {
  checksum <- cache_file_checksum(path)
  !is.null(checksum) &&
    isTRUE(
      tryCatch(
        identical(sha256_file(path), checksum),
        error = function(error) FALSE
      )
    )
}

cache_store_checksum <- function(path, checksum) {
  temporary <- tempfile("zak-cache-checksum-", tmpdir = dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  tryCatch(
    {
      writeLines(checksum, temporary, useBytes = TRUE)
      destination <- cache_checksum_file(path)
      if (file.rename(temporary, destination)) {
        return(TRUE)
      }
      file.copy(temporary, destination, overwrite = TRUE)
    },
    error = function(error) FALSE
  )
}

cache_archive_file <- function(url) {
  if (!grepl("^https?://", url, ignore.case = TRUE)) {
    return(NULL)
  }
  zak_cache_file("archives", zak_cache_key("archive", url), ".artifact")
}

cached_archive_artifact <- function(url) {
  path <- cache_archive_file(url)
  if (is.null(path) || !file.exists(path) || !cache_file_is_valid(path)) {
    return(NULL)
  }
  format <- tryCatch(archive_format(path), error = function(error) NULL)
  if (is.null(format)) {
    return(NULL)
  }
  new_archive_artifact(
    url,
    path,
    format,
    retrieved_at = file.info(path)$mtime,
    method = "cache",
    owned = FALSE,
    sha256 = cache_file_checksum(path)
  )
}

cache_archive_artifact <- function(url, archive, format) {
  destination <- cache_archive_file(url)
  if (is.null(destination)) {
    return(NULL)
  }
  cached_format <- if (
    file.exists(destination) && cache_file_is_valid(destination)
  ) {
    tryCatch(archive_format(destination), error = function(error) NULL)
  } else {
    NULL
  }
  if (
    !identical(cached_format, format) &&
      !cache_store_file(
        archive,
        destination,
        metadata = list(source = redact_lock_url(url))
      )
  ) {
    return(NULL)
  }
  new_archive_artifact(
    url,
    destination,
    format,
    retrieved_at = Sys.time(),
    method = "download.file",
    owned = FALSE,
    sha256 = cache_file_checksum(destination)
  )
}

is_git_commit <- function(ref) {
  is.character(ref) &&
    length(ref) == 1L &&
    !is.na(ref) &&
    grepl("^[0-9a-f]{40}$", ref, ignore.case = TRUE)
}

cache_git_file <- function(url, commit) {
  zak_cache_file(
    "git",
    zak_cache_key("git", url, commit),
    ".tar.gz"
  )
}

cached_git_artifact <- function(url, ref) {
  if (!is_git_commit(ref)) {
    return(NULL)
  }
  archive <- cache_git_file(url, ref)
  if (
    is.null(archive) ||
      !file.exists(archive) ||
      !cache_file_is_valid(archive)
  ) {
    return(NULL)
  }
  checkout <- tempfile("zak-git-cache-")
  dir.create(checkout)
  extracted <- tryCatch(
    {
      utils::untar(archive, exdir = checkout)
      file.exists(file.path(checkout, "DESCRIPTION"))
    },
    error = function(error) FALSE
  )
  if (!extracted) {
    unlink(checkout, recursive = TRUE, force = TRUE)
    return(NULL)
  }
  new_git_artifact(
    url,
    checkout,
    ref,
    ref,
    retrieved_at = file.info(archive)$mtime,
    method = "cache",
    owned = TRUE,
    sha256 = git_checkout_sha256(checkout)
  )
}

cache_git_artifact <- function(url, commit, checkout) {
  destination <- cache_git_file(url, commit)
  if (is.null(destination)) {
    return(invisible(FALSE))
  }
  temporary <- tempfile(
    "zak-git-cache-",
    tmpdir = dirname(destination),
    fileext = ".tar.gz"
  )
  created <- FALSE
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  original <- setwd(checkout)
  on.exit(setwd(original), add = TRUE)
  files <- setdiff(list.files(all.files = TRUE, no.. = TRUE), ".git")
  tryCatch(
    {
      utils::tar(temporary, files = files, compression = "gzip")
      created <- file.exists(temporary)
    },
    error = function(error) NULL
  )
  if (!created) {
    return(invisible(FALSE))
  }
  invisible(
    cache_store_file(
      temporary,
      destination,
      metadata = list(
        source = paste0(
          "git::",
          redact_lock_url(url),
          "@",
          commit
        )
      )
    )
  )
}
