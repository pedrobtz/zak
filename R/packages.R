as_package_data_frame <- function(packages) {
  if (!is.matrix(packages)) {
    stop(
      "`packages` must be a matrix returned by an R package explorer.",
      call. = FALSE
    )
  }
  if (is.null(colnames(packages))) {
    stop("`packages` must have column names.", call. = FALSE)
  }

  columns <- colnames(packages)
  rownames(packages) <- NULL
  result <- as.data.frame(
    packages,
    stringsAsFactors = FALSE,
    optional = TRUE
  )
  names(result) <- columns
  result
}

#' Explore available packages
#'
#' `available_packages()` wraps [utils::available.packages()] and returns its
#' package database as a data frame instead of a character matrix. Warnings and
#' errors from repository access are preserved.
#'
#' @param repos A character vector of repository URLs. Defaults to
#'   `getOption("repos")`.
#' @param ... Additional arguments passed to [utils::available.packages()],
#'   such as `type` or `filters`.
#'
#' @return A data frame with one row per available package and the columns
#'   returned by [utils::available.packages()]. Column names are preserved,
#'   every column is character, and automatic row names allow duplicate package
#'   names. A zero-row matrix returned by R becomes a zero-row data frame with
#'   the same columns.
#' @seealso [installed_packages()]
#' @export
#'
#' @examples
#' \dontrun{
#' packages <- available_packages()
#' packages[packages$Package == "jsonlite", c("Package", "Version")]
#' }
available_packages <- function(repos = getOption("repos"), ...) {
  packages <- utils::available.packages(repos = repos, ...)
  as_package_data_frame(packages)
}

#' Explore installed packages
#'
#' `installed_packages()` wraps [utils::installed.packages()] and returns its
#' package database as a data frame instead of a character matrix. A package
#' found in multiple libraries has one row per library, distinguished by
#' `LibPath`.
#'
#' @param lib.loc A character vector of library trees to search, or `NULL` to
#'   use all known libraries.
#' @param ... Additional arguments passed to [utils::installed.packages()],
#'   such as `priority` or `fields`.
#'
#' @return A data frame with one row per installed package and the columns
#'   returned by [utils::installed.packages()]. Column names are preserved and
#'   every column is character. Automatic row names allow the same package to
#'   appear in multiple libraries.
#' @seealso [available_packages()]
#' @export
#'
#' @examples
#' packages <- installed_packages(priority = "base")
#' packages[, c("Package", "Version")]
installed_packages <- function(lib.loc = NULL, ...) {
  packages <- utils::installed.packages(lib.loc = lib.loc, ...)
  as_package_data_frame(packages)
}
