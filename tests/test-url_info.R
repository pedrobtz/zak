library(pax)

## header parsing ------------------------------------------------------------

h <- c("HTTP/1.1 200 OK\r\n",
       "Content-Type: application/x-gzip\r\n",
       "Content-Length: 1234\r\n",
       "\r\n")
parsed <- pax:::parse_headers(h)
stopifnot(
    identical(parsed[["content-type"]], "application/x-gzip"),
    identical(parsed[["content-length"]], "1234"),
    ## names are lower-cased, values trimmed
    all(names(parsed) == tolower(names(parsed)))
)

## after a redirect the final block wins
redirected <- c("HTTP/1.1 302 Found\r\n",
                "Location: https://elsewhere.example.org/pkg.tar.gz\r\n",
                "Content-Length: 0\r\n",
                "\r\n",
                "HTTP/1.1 200 OK\r\n",
                "Content-Length: 4096\r\n",
                "\r\n")
stopifnot(identical(pax:::parse_headers(redirected)[["content-length"]], "4096"))

## a body-less response parses to an empty vector, not an error
stopifnot(length(pax:::parse_headers(c("HTTP/1.1 204 No Content\r\n", "\r\n"))) == 0L)

## status interpretation -----------------------------------------------------

stopifnot(
    isTRUE(pax:::status_exists(200L)),
    isTRUE(pax:::status_exists(206L)),
    isFALSE(pax:::status_exists(404L)),
    isFALSE(pax:::status_exists(410L)),
    ## exists, but we are not allowed to see it
    is.na(pax:::status_exists(403L)),
    ## the server refuses HEAD; that is not an answer about the file
    is.na(pax:::status_exists(405L)),
    is.na(pax:::status_exists(500L)),
    is.na(pax:::status_exists(NA_integer_))
)

## file:// URLs --------------------------------------------------------------

f <- tempfile(fileext = ".tar.gz")
writeBin(as.raw(c(0x1f, 0x8b, 0x08, 0x00)), f)
on.exit(unlink(f), add = TRUE)

info <- url_info(paste0("file://", f))
stopifnot(
    is.data.frame(info),
    nrow(info) == 1L,
    identical(names(info), c("url", "exists", "status", "type", "bytes")),
    isTRUE(info$exists),
    identical(info$bytes, 4),
    ## no HTTP status for a local file
    is.na(info$status)
)

missing_info <- url_info(paste0("file://", tempfile(fileext = ".tar.gz")))
stopifnot(isFALSE(missing_info$exists), is.na(missing_info$bytes))

## the wrapper answers the same question, named by url
u <- paste0("file://", f)
ex <- url_exists(c(u, paste0("file://", tempfile())))
stopifnot(
    is.logical(ex),
    identical(names(ex)[1L], u),
    isTRUE(ex[[1L]]),
    isFALSE(ex[[2L]])
)

## paths needing percent-decoding round-trip
spaced <- file.path(tempdir(), "a file.tar.gz")
file.create(spaced)
on.exit(unlink(spaced), add = TRUE)
stopifnot(isTRUE(url_exists(paste0("file://", utils::URLencode(spaced)))[[1L]]))

## edges ---------------------------------------------------------------------

empty <- url_info(character(0))
stopifnot(is.data.frame(empty), nrow(empty) == 0L,
          identical(names(empty), c("url", "exists", "status", "type", "bytes")))

## NA urls are passed through, not checked
na_info <- url_info(NA_character_)
stopifnot(nrow(na_info) == 1L, is.na(na_info$exists))

stopifnot(inherits(try(url_info(1L), silent = TRUE), "try-error"))
