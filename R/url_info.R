# Inspecting a URL before downloading it --------------------------------------
#
# base::curlGetHeaders() (R >= 3.2, no packages required) performs a HEAD
# request and returns the response header lines, with the final status code in
# attr(, "status"). That is enough to check a file is there, and to learn its
# size and content type, without transferring the body.
#
# Two limitations shape the code below: curlGetHeaders() handles only
# http/https/ftp, so file:// URLs are answered from the filesystem instead; and
# it signals an error rather than returning a status when the host cannot be
# reached at all.

#' Parse response header lines into a named character vector.
#'
#' Names are lower-cased, since header names are case-insensitive. After a
#' redirect the response contains several header blocks; the last value for a
#' name wins, which is the one belonging to the final response.
#'
#' @param h Character vector of header lines, as returned by
#'   [base::curlGetHeaders()].
#' @return A named character vector, possibly empty.
#' @noRd
parse_headers <- function(h) {
    lines <- sub("\r?\n$", "", as.character(h))
    lines <- lines[grepl("^[^:[:space:]]+:", lines)]
    if (!length(lines)) {
        return(character(0))
    }
    nm <- tolower(sub(":.*$", "", lines))
    value <- trimws(sub("^[^:]*:[[:space:]]*", "", lines))
    last <- !duplicated(nm, fromLast = TRUE)
    out <- value[last]
    names(out) <- nm[last]
    out
}

#' Local path behind a file:// URL.
#'
#' @param url A single `file://` URL.
#' @return A single file path.
#' @noRd
file_url_path <- function(url) {
    path <- utils::URLdecode(sub("^file://(localhost)?", "", url))
    ## file:///C:/dir/pkg.tar.gz on Windows
    if (grepl("^/[A-Za-z]:", path)) path <- sub("^/", "", path)
    path
}

#' Does an HTTP status mean the file is there?
#'
#' @param status An integer HTTP status code.
#' @return `TRUE`, `FALSE`, or `NA` when the status settles nothing - 403
#'   (exists but forbidden), 405 (server refuses HEAD), 5xx (server trouble).
#' @noRd
status_exists <- function(status) {
    if (is.na(status)) {
        NA
    } else if (status >= 200L && status < 300L) {
        TRUE
    } else if (status == 404L || status == 410L) {
        FALSE
    } else {
        NA
    }
}

#' What a URL says about itself
#'
#' Issues a HEAD request with [base::curlGetHeaders()] and reports whether the
#' file is there, along with the size and content type the server advertises.
#' Nothing is downloaded. `file://` URLs are answered from the filesystem,
#' since `curlGetHeaders()` handles only http, https and ftp.
#'
#' @param url Character vector of URLs.
#' @param timeout Timeout in seconds for each request.
#' @return A data frame with one row per URL and the columns:
#'   \describe{
#'     \item{`url`}{The URL, as given.}
#'     \item{`exists`}{`TRUE`, `FALSE`, or `NA` when the answer is not
#'       conclusive - the host could not be reached, libcurl is unavailable,
#'       or the status settles nothing (403 forbidden, 405 HEAD refused, 5xx).}
#'     \item{`status`}{Final HTTP status after redirects, `NA` for `file://`.}
#'     \item{`type`}{Advertised `Content-Type`, or `NA`.}
#'     \item{`bytes`}{Advertised `Content-Length`, or `NA`.}
#'   }
#' @section Treating the result:
#'   `exists` is deliberately three-valued. A `FALSE` is worth acting on; an
#'   `NA` means the check could not settle the question and the download should
#'   be attempted anyway rather than refused.
#' @seealso [url_exists()]
#' @examples
#' ## file:// URLs are answered from the filesystem
#' f <- tempfile(fileext = ".tar.gz")
#' writeBin(as.raw(c(0x1f, 0x8b)), f)
#' url_info(paste0("file://", f))
#' unlink(f)
#'
#' \dontrun{
#' url_info("https://cloud.r-project.org/src/contrib/PACKAGES")
#' }
#' @export
url_info <- function(url, timeout = 10L) {
    if (!is.character(url)) {
        stop("'url' must be a character vector", call. = FALSE)
    }
    out <- data.frame(
        url = url,
        exists = rep(NA, length(url)),
        status = rep(NA_integer_, length(url)),
        type = rep(NA_character_, length(url)),
        bytes = rep(NA_real_, length(url)),
        stringsAsFactors = FALSE
    )
    if (!length(url)) {
        return(out)
    }

    has_libcurl <- isTRUE(unname(capabilities("libcurl")))
    warned_libcurl <- FALSE

    for (i in seq_along(url)) {
        u <- url[[i]]
        if (is.na(u)) {
            next
        }

        if (grepl("^file://", u)) {
            path <- file_url_path(u)
            out$exists[i] <- file.exists(path)
            if (isTRUE(out$exists[i])) out$bytes[i] <- file.size(path)
            next
        }

        if (!has_libcurl) {
            if (!warned_libcurl) {
                warning("this R build has no libcurl support, so URLs cannot ",
                        "be checked", call. = FALSE)
                warned_libcurl <- TRUE
            }
            next
        }

        ## An unreachable host is an error, not a status: leave the row NA.
        h <- tryCatch(
            curlGetHeaders(u, redirect = TRUE, timeout = timeout),
            error = function(e) NULL
        )
        if (is.null(h)) {
            next
        }

        status <- as.integer(attr(h, "status"))
        headers <- parse_headers(h)
        out$status[i] <- status
        out$exists[i] <- status_exists(status)
        if ("content-type" %in% names(headers)) {
            out$type[i] <- headers[["content-type"]]
        }
        if ("content-length" %in% names(headers)) {
            out$bytes[i] <- suppressWarnings(
                as.numeric(headers[["content-length"]])
            )
        }
    }
    out
}

#' Does a URL point at something that exists?
#'
#' A convenience wrapper around [url_info()] for when only the answer matters.
#'
#' @param url Character vector of URLs.
#' @param timeout Timeout in seconds for each request.
#' @return A logical vector, named by `url`. `NA` where the check could not
#'   settle the question; see [url_info()].
#' @seealso [url_info()]
#' @examples
#' f <- tempfile(fileext = ".tar.gz")
#' file.create(f)
#' url_exists(paste0("file://", f))
#' unlink(f)
#' @export
url_exists <- function(url, timeout = 10L) {
    info <- url_info(url, timeout = timeout)
    out <- info$exists
    names(out) <- url
    out
}
