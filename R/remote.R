url_remote_metadata <- function(url) {
  list(RemoteType = "url", RemoteUrl = url, RemoteSubdir = NULL)
}

add_url_remote_metadata <- function(package, url) {
  metadata <- url_remote_metadata(url)
  description_path <- file.path(package, "DESCRIPTION")
  binary_description_path <- file.path(package, "Meta", "package.rds")
  updated <- FALSE

  if (file.exists(description_path)) {
    description <- read.dcf(description_path)
    description <- as.data.frame(
      description,
      stringsAsFactors = FALSE,
      optional = TRUE
    )
    for (field in names(metadata)) {
      value <- metadata[[field]]
      if (is.null(value)) {
        description[[field]] <- NULL
      } else {
        description[[field]] <- value
      }
    }
    write.dcf(description, description_path)
    updated <- TRUE
  }

  if (file.exists(binary_description_path)) {
    package_description <- readRDS(binary_description_path)
    fields <- as.list(package_description$DESCRIPTION)
    fields <- utils::modifyList(fields, metadata)
    package_description$DESCRIPTION <- stats::setNames(
      vapply(fields, as.character, character(1L)),
      names(fields)
    )
    saveRDS(package_description, binary_description_path)
    updated <- TRUE
  }

  if (!updated) {
    stop(
      "R could not find package metadata to record the remote URL.",
      call. = FALSE
    )
  }

  clear_remote_metadata_md5(package)
  invisible()
}

clear_remote_metadata_md5 <- function(package) {
  path <- file.path(package, "MD5")
  if (!file.exists(path)) {
    return(invisible())
  }

  checksums <- readLines(path, warn = FALSE)
  metadata_files <- "\\*(DESCRIPTION|Meta/package[.]rds)$"
  writeLines(
    checksums[!grepl(metadata_files, checksums)],
    path,
    useBytes = TRUE
  )
  invisible()
}

record_installed_url_remote <- function(metadata, url, lib) {
  libraries <- if (is.null(lib)) .libPaths() else lib
  installed <- utils::installed.packages(
    lib.loc = libraries,
    noCache = TRUE
  )
  matching <- installed[, "Package"] == metadata$package &
    installed[, "Version"] == metadata$version
  if (!any(matching)) {
    return(invisible(FALSE))
  }

  package <- file.path(
    installed[which(matching)[[1L]], "LibPath"],
    metadata$package
  )
  add_url_remote_metadata(package, url)
  invisible(TRUE)
}
