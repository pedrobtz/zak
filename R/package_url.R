# Locating package files in a CRAN-like repository ----------------------------
#
# A repository serves each package at `<Repository>/<file>`, where the
# `Repository` column of available.packages() is the contrib URL for the
# requested type and the file name is normally `<Package>_<Version><ext>`.
# Repositories may override the file name through an optional `File` field,
# which is what utils::download.packages() honours; we do the same.

## Friendly OS names accepted in addition to the package types R itself uses.
pkg_type_aliases <- c(
    windows = "win.binary",
    win     = "win.binary",
    macos   = "mac.binary",
    mac     = "mac.binary",
    osx     = "mac.binary",
    linux   = "source",
    unix    = "source"
)

#' Resolve a package type to a single concrete type.
#'
#' `"binary"` and `"both"` describe a preference rather than a file, so they
#' resolve to the current platform's own type - which is `"source"` on
#' platforms CRAN does not build binaries for.
#'
#' @param type A single string: a type known to [utils::available.packages()]
#'   or one of the OS aliases in `pkg_type_aliases`.
#' @return A single string, one of `"source"` or a `win.binary` / `mac.binary`
#'   type (possibly with a platform suffix, e.g. `"mac.binary.big-sur-arm64"`).
#' @noRd
resolve_pkg_type <- function(type) {
    if (!is.character(type) || length(type) != 1L || is.na(type)) {
        stop("'type' must be a single, non-missing string", call. = FALSE)
    }
    lower <- tolower(type)
    if (lower %in% names(pkg_type_aliases)) {
        return(pkg_type_aliases[[lower]])
    }
    if (type %in% c("binary", "both")) .Platform$pkgType else type
}

#' File extension used by a resolved package type.
#'
#' @param type A type as returned by `resolve_pkg_type()`.
#' @return A single string, including the leading dot.
#' @noRd
pkg_type_ext <- function(type) {
    if (type == "source") {
        ".tar.gz"
    } else if (startsWith(type, "mac.binary")) {
        ".tgz"
    } else if (startsWith(type, "win.binary")) {
        ".zip"
    } else {
        stop("cannot determine a file extension for package type ",
             sQuote(type), call. = FALSE)
    }
}

#' Full URL of a package in a CRAN-like repository
#'
#' Builds the download URL a repository serves a package at, without
#' downloading anything. This is the address [utils::download.packages()] would
#' fetch from, and is suitable input for `install_url()`.
#'
#' @param pkgs Character vector of package names.
#' @param type Package type to build the URL for. Accepts the types known to
#'   [utils::available.packages()] (`"source"`, `"win.binary"`,
#'   `"mac.binary"`, ...) and the OS aliases `"windows"`, `"macos"` and
#'   `"linux"`. `"binary"` and `"both"` describe a preference rather than a
#'   file and resolve to the current platform's own type, which is `"source"`
#'   on platforms CRAN publishes no binaries for.
#' @param repos Character vector of repository URLs, as in
#'   [utils::available.packages()]. Ignored if `available` is supplied.
#' @param available Optionally, a data frame as returned by
#'   [available_packages()], to avoid contacting the repositories repeatedly.
#'   It must describe the same `type`, since the file extension follows from
#'   it.
#' @return A character vector of URLs, the same length as `pkgs` and named by
#'   it. Packages the repositories do not offer yield `NA` with a warning. If a
#'   package is offered by more than one repository, the first one wins, in the
#'   order [available_packages()] returns.
#' @seealso [available_packages()]
#' @examples
#' \dontrun{
#' package_url("jsonlite")
#' package_url("jsonlite", type = "windows")
#'
#' ## reuse one snapshot for many lookups
#' avail <- available_packages(type = "source")
#' package_url(c("jsonlite", "curl"), type = "source", available = avail)
#' }
#' @export
package_url <- function(pkgs,
                        type = getOption("pkgType"),
                        repos = getOption("repos"),
                        available = NULL) {
    if (!is.character(pkgs)) {
        stop("'pkgs' must be a character vector of package names", call. = FALSE)
    }
    type <- resolve_pkg_type(type)
    ext <- pkg_type_ext(type)

    if (is.null(available)) {
        available <- available_packages(repos = repos, type = type)
    } else if (!is.data.frame(available)) {
        stop("'available' must be a data frame, as returned by ",
             "available_packages()", call. = FALSE)
    }
    required <- c("Package", "Version", "Repository")
    missing_cols <- required[!required %in% names(available)]
    if (length(missing_cols)) {
        stop("'available' is missing the column(s): ",
             paste(missing_cols, collapse = ", "), call. = FALSE)
    }

    i <- match(pkgs, available$Package)

    files <- paste0(available$Package[i], "_", available$Version[i], ext)
    ## Repositories may serve a package under a different file name.
    if ("File" %in% names(available)) {
        named <- available$File[i]
        files[!is.na(named)] <- named[!is.na(named)]
    }
    urls <- paste0(available$Repository[i], "/", files)
    ## paste0() turns NA into the string "NA", so blank the misses out again.
    urls[is.na(i)] <- NA_character_
    names(urls) <- pkgs

    if (anyNA(i)) {
        warning("package(s) not available for type ", sQuote(type), ": ",
                paste(pkgs[is.na(i)], collapse = ", "), call. = FALSE)
    }
    urls
}
