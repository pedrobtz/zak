parse_remotes <- function(fields) {
  value <- unname(fields["Remotes"])
  if (!length(value) || is.na(value) || !nzchar(trimws(value))) {
    return(list())
  }
  values <- trimws(unlist(strsplit(value, ",", fixed = TRUE)))
  values <- values[nzchar(values)]
  lapply(values, function(value) {
    source <- parse_remote_reference(value)
    list(package = remote_reference_package(source), source = source)
  })
}

parse_remote_reference <- function(value) {
  if (!is.character(value) || length(value) != 1L || !nzchar(value)) {
    stop("Remote references must be non-empty text.", call. = FALSE)
  }
  if (startsWith(value, "github::")) {
    return(parse_github_reference(sub("^github::", "", value)))
  }
  if (startsWith(value, "git::")) {
    return(parse_git_reference(sub("^git::", "", value)))
  }
  if (startsWith(value, "bioc::")) {
    return(parse_bioconductor_reference(sub("^bioc::", "", value)))
  }
  if (startsWith(value, "url::")) {
    url <- sub("^url::", "", value)
    validate_install_url(url)
    return(list(type = "url", url = url))
  }
  if (grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(@.+)?$", value)) {
    return(parse_github_reference(value))
  }
  stop(
    sprintf("Unsupported Remotes reference: %s.", value),
    call. = FALSE
  )
}

parse_bioconductor_reference <- function(value) {
  if (!grepl("^[A-Za-z][A-Za-z0-9.]*$", value)) {
    stop(
      "Bioconductor references must use a package name.",
      call. = FALSE
    )
  }
  list(type = "bioconductor", package = value)
}

remote_reference_package <- function(source) {
  if (identical(source$type, "bioconductor")) {
    return(source$package)
  }
  if (identical(source$type, "git")) {
    value <- sub("[?#].*$", "", sub("/$", "", source$url))
    value <- sub("[.]git$", "", basename(value), ignore.case = TRUE)
    if (grepl("^[A-Za-z][A-Za-z0-9.]*$", value)) {
      return(value)
    }
  }
  if (identical(source$type, "url")) {
    value <- basename(sub("[?#].*$", "", source$url))
    value <- sub("[.]tar[.]gz$|[.]zip$", "", value, ignore.case = TRUE)
    value <- sub("_[0-9].*$", "", value)
    if (grepl("^[A-Za-z][A-Za-z0-9.]*$", value)) {
      return(value)
    }
  }
  NULL
}

remote_source_label <- function(source) {
  switch(
    source$type,
    git = paste0("git::", redact_lock_url(source$url), "@", source$ref),
    url = paste0("url::", redact_lock_url(source$url)),
    bioconductor = paste0("bioc::", source$package),
    sprintf("%s source", source$type)
  )
}
