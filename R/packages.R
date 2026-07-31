# Package databases as data frames -------------------------------------------
#
# `available.packages()` and `installed.packages()` both return character
# matrices, which are awkward to subset and filter. These helpers return the
# same information as data frames with character columns, leaving the column
# names untouched so the base R documentation still applies.

#' Convert a package matrix to a data frame.
#'
#' Internal helper shared by [available_packages()] and [installed_packages()].
#' Row names are dropped: a package can legitimately appear more than once
#' (installed in several libraries, or offered by several repositories), and
#' duplicate row names are an error for data frames.
#'
#' @param x A character matrix as returned by [utils::available.packages()] or
#'   [utils::installed.packages()].
#' @return A data frame with the columns of `x`, all of type character.
#' @noRd
as_package_df <- function(x) {
    if (!is.matrix(x)) {
        stop("'x' must be a matrix, as returned by available.packages() or ",
             "installed.packages()", call. = FALSE)
    }
    cols <- colnames(x)
    dimnames(x) <- list(NULL, cols)
    ## optional = TRUE keeps column names verbatim; make.names() would mangle
    ## the ones base R uses, such as "License_is_FOSS" or "NeedsCompilation".
    df <- as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
    names(df) <- cols
    df
}

#' Available packages as a data frame
#'
#' A thin wrapper around [utils::available.packages()] that returns a data
#' frame instead of a character matrix.
#'
#' @param repos Character vector of repository URLs, as in
#'   [utils::available.packages()].
#' @param ... Further arguments passed to [utils::available.packages()], for
#'   example `type` or `filters`.
#' @return A data frame with one row per available package and the columns
#'   documented in [utils::available.packages()] (`Package`, `Version`,
#'   `Depends`, `Imports`, `Repository`, ...), all of type character. Missing
#'   fields are `NA`. The result has zero rows if no repository is reachable
#'   or configured.
#' @seealso [installed_packages()]
#' @export
available_packages <- function(repos = getOption("repos"), ...) {
    as_package_df(utils::available.packages(repos = repos, ...))
}

#' Installed packages as a data frame
#'
#' A thin wrapper around [utils::installed.packages()] that returns a data
#' frame instead of a character matrix.
#'
#' Note that a package installed in more than one of `lib.loc` yields one row
#' per library, distinguished by the `LibPath` column.
#'
#' @param lib.loc Character vector of library trees to search, or `NULL` (the
#'   default) for [.libPaths()].
#' @param ... Further arguments passed to [utils::installed.packages()], for
#'   example `priority` or `fields`.
#' @return A data frame with one row per installed package and the columns
#'   documented in [utils::installed.packages()] (`Package`, `LibPath`,
#'   `Version`, `Priority`, `Depends`, `Built`, ...), all of type character.
#'   Missing fields are `NA`.
#' @seealso [available_packages()]
#' @export
installed_packages <- function(lib.loc = NULL, ...) {
    as_package_df(utils::installed.packages(lib.loc = lib.loc, ...))
}
