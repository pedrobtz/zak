git_available <- function() {
  nzchar(Sys.which("git"))
}

validate_git_url <- function(url) {
  valid <- is.character(url) &&
    length(url) == 1L &&
    !is.na(url) &&
    nzchar(url) &&
    !grepl("[[:space:][:cntrl:]]", url) &&
    # Quoting stops the shell from reading a URL, but git still reads a
    # leading `-` as an option (for example `--upload-pack=`).
    !startsWith(url, "-") &&
    (grepl("^(https?|ssh|git|file)://", url, ignore.case = TRUE) ||
      grepl("^[^/@[:space:]]+@[^/:[:space:]]+:.+", url))
  if (!valid) {
    stop(
      "Git references must use an HTTP(S), SSH, Git, file, or SCP-style URL.",
      call. = FALSE
    )
  }
  invisible()
}

parse_git_reference <- function(value) {
  parsed <- split_git_reference(value)
  validate_git_url(parsed$url)
  list(type = "git", url = parsed$url, ref = parsed$ref)
}

parse_github_reference <- function(value) {
  parsed <- split_git_reference(value)
  if (!grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", parsed$url)) {
    stop(
      "GitHub references must use the `owner/repository` form.",
      call. = FALSE
    )
  }
  parts <- strsplit(parsed$url, "/", fixed = TRUE)[[1L]]
  list(
    type = "git",
    url = sprintf("https://github.com/%s/%s.git", parts[[1L]], parts[[2L]]),
    ref = parsed$ref
  )
}

split_git_reference <- function(value) {
  if (!is.character(value) || length(value) != 1L || !nzchar(value)) {
    stop("A Git reference must be non-empty text.", call. = FALSE)
  }
  at <- gregexpr("@", value, fixed = TRUE)[[1L]]
  last_at <- if (identical(at, -1L)) 0L else max(at)
  marker <- FALSE
  if (last_at > 0L && grepl("^[A-Za-z][A-Za-z0-9+.-]*://", value)) {
    scheme_end <- regexpr("://", value, fixed = TRUE)[[1L]] + 2L
    slash <- regexpr(
      "/",
      substr(value, scheme_end + 1L, nchar(value)),
      fixed = TRUE
    )[[1L]]
    authority_end <- if (identical(slash, -1L)) {
      nchar(value)
    } else {
      scheme_end + slash
    }
    marker <- last_at > authority_end
  } else if (
    last_at > 0L &&
      grepl("^[^/@[:space:]]+@[^/:[:space:]]+:", value)
  ) {
    marker <- last_at > regexpr(":", value, fixed = TRUE)[[1L]]
  } else {
    marker <- last_at > 0L
  }
  if (marker) {
    url <- substr(value, 1L, last_at - 1L)
    ref <- substr(value, last_at + 1L, nchar(value))
    if (!nzchar(ref)) {
      stop(
        "Git references must include a non-empty ref after `@`.",
        call. = FALSE
      )
    }
  } else {
    url <- value
    ref <- "HEAD"
  }
  if (!nzchar(url) || grepl("[[:space:][:cntrl:]]", ref)) {
    stop(
      "Git references must not contain whitespace or line breaks.",
      call. = FALSE
    )
  }
  # Quoting stops the shell from reading a ref, but git still reads a leading
  # `-` as an option. Git ref names cannot begin with `-` in any case.
  if (startsWith(ref, "-")) {
    stop("Git refs must not begin with `-`.", call. = FALSE)
  }
  list(url = url, ref = ref)
}

acquire_git_repository <- function(url, ref) {
  cached <- cached_git_artifact(url, ref)
  if (!is.null(cached)) {
    return(cached)
  }
  if (!git_available()) {
    stop(
      "The `git` executable is required for Git package references.",
      call. = FALSE
    )
  }
  if (zak_offline()) {
    stop(
      sprintf(
        paste0(
          "Offline mode is enabled and no valid cached Git source is available",
          " for %s at commit %s."
        ),
        redact_lock_url(url),
        ref
      ),
      call. = FALSE
    )
  }
  checkout <- tempfile("zak-git-")
  cleanup <- TRUE
  on.exit(
    {
      if (cleanup) {
        unlink(checkout, recursive = TRUE, force = TRUE)
      }
    },
    add = TRUE
  )

  run_git(c("clone", "--quiet", url, checkout), "clone the Git repository")
  run_git(
    c("-C", checkout, "checkout", "--quiet", ref),
    "check out the requested Git ref"
  )
  commit <- run_git(
    c("-C", checkout, "rev-parse", "HEAD"),
    "read the checked-out Git commit",
    require_output = TRUE
  )
  artifact <- new_git_artifact(
    url,
    checkout,
    ref,
    commit,
    retrieved_at = Sys.time(),
    method = "git clone",
    owned = TRUE
  )
  cache_git_artifact(url, commit, checkout)
  cleanup <- FALSE
  artifact
}

new_git_artifact <- function(
  url,
  path,
  ref,
  commit,
  retrieved_at,
  method,
  owned,
  sha256 = git_checkout_sha256(path)
) {
  info <- file.info(path)
  if (!dir.exists(path) || is.na(info$size)) {
    stop("R could not inspect the Git checkout.", call. = FALSE)
  }
  if (!is_sha256(sha256)) {
    stop("Git checkout SHA-256 checksums must be valid.", call. = FALSE)
  }
  structure(
    list(
      type = "git",
      url = url,
      path = normalizePath(path, mustWork = TRUE),
      ref = ref,
      commit = commit,
      format = "directory",
      size = git_checkout_size(path),
      retrieved_at = retrieved_at,
      method = method,
      owned = owned,
      directory = TRUE,
      sha256 = tolower(sha256)
    ),
    class = c("zak_git_artifact", "zak_artifact", "list")
  )
}

# Git's diagnostics are what make a failure actionable — authentication,
# an unknown ref, an unreachable host. Truncated so a verbose failure cannot
# bury the message.
git_failure_details <- function(diagnostics, limit = 10L) {
  diagnostics <- trimws(diagnostics)
  diagnostics <- diagnostics[nzchar(diagnostics)]
  if (!length(diagnostics)) {
    return("")
  }
  truncated <- length(diagnostics) > limit
  if (truncated) {
    diagnostics <- utils::tail(diagnostics, limit)
  }
  paste0(
    "\n",
    if (truncated) "  (last lines of git output)\n" else "",
    paste0("  ", redact_embedded_urls(diagnostics), collapse = "\n")
  )
}

run_git <- function(arguments, action, require_output = FALSE) {
  errors <- tempfile("zak-git-stderr-")
  on.exit(unlink(errors, force = TRUE), add = TRUE)

  # `system2()` shell-quotes the command but pastes `args` into the command
  # line unquoted, so arguments must be quoted here. Without this, a ref or
  # URL containing shell metacharacters is executed.
  #
  # stderr goes to a file rather than being merged into stdout: merging lets
  # git's chatter (detached-HEAD advice, config warnings) contaminate the value
  # of commands read for their output, such as `rev-parse HEAD`.
  output <- suppressWarnings(system2(
    Sys.which("git"),
    shQuote(arguments),
    stdout = TRUE,
    stderr = errors
  ))
  status <- attr(output, "status")
  diagnostics <- if (file.exists(errors)) {
    readLines(errors, warn = FALSE)
  } else {
    character()
  }

  if (!is.null(status) && status != 0L) {
    stop(
      sprintf(
        "Git could not %s.%s",
        action,
        git_failure_details(c(diagnostics, output))
      ),
      call. = FALSE
    )
  }
  value <- trimws(paste(output, collapse = "\n"))
  if (!nzchar(value) && isTRUE(require_output)) {
    stop(
      sprintf(
        "Git returned no result while trying to %s.%s",
        action,
        git_failure_details(diagnostics)
      ),
      call. = FALSE
    )
  }
  value
}

git_checkout_size <- function(path) {
  files <- list.files(
    path,
    recursive = TRUE,
    all.files = TRUE,
    full.names = TRUE
  )
  files <- files[!file.info(files)$isdir]
  files <- files[!grepl("[/\\\\][.]git([/\\\\]|$)", files)]
  sizes <- file.info(files)$size
  sum(sizes[!is.na(sizes)])
}

git_acquisition <- function(artifact) {
  if (!inherits(artifact, "zak_git_artifact")) {
    stop("`artifact` must be a Git acquired artifact.", call. = FALSE)
  }
  artifact[c(
    "type",
    "url",
    "ref",
    "commit",
    "format",
    "size",
    "retrieved_at",
    "method",
    "sha256"
  )]
}
