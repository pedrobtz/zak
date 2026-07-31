library(pax)

## as_package_df() -----------------------------------------------------------

m <- matrix(
    c("A", "1.0", NA,
      "B", "2.0", "A (>= 1.0)"),
    nrow = 2L, byrow = TRUE,
    dimnames = list(c("A", "B"), c("Package", "Version", "Depends"))
)

df <- pax:::as_package_df(m)
stopifnot(
    is.data.frame(df),
    nrow(df) == 2L,
    identical(names(df), c("Package", "Version", "Depends")),
    identical(df$Package, c("A", "B")),
    is.character(df$Version),
    is.na(df$Depends[1L])
)

## row names are dropped, so duplicated package names are not an error
dup <- m[c(1L, 1L), , drop = FALSE]
dup_df <- pax:::as_package_df(dup)
stopifnot(nrow(dup_df) == 2L, identical(dup_df$Package, c("A", "A")))

## a zero-row matrix keeps its columns
empty_df <- pax:::as_package_df(m[integer(0), , drop = FALSE])
stopifnot(
    is.data.frame(empty_df),
    nrow(empty_df) == 0L,
    identical(names(empty_df), c("Package", "Version", "Depends"))
)

## non-matrix input is rejected
stopifnot(inherits(try(pax:::as_package_df("nope"), silent = TRUE), "try-error"))

## installed_packages() ------------------------------------------------------

inst <- installed_packages()
stopifnot(
    is.data.frame(inst),
    all(c("Package", "LibPath", "Version") %in% names(inst)),
    is.character(inst$Package),
    "base" %in% inst$Package
)

base_pkgs <- installed_packages(priority = "base")$Package
stopifnot("base" %in% base_pkgs, "utils" %in% base_pkgs)

## available_packages() ------------------------------------------------------
## No network in R CMD check: point at a repository that does not exist and
## check the empty-result contract rather than the contents.

avail <- suppressWarnings(
    available_packages(repos = c(CRAN = "file:///nonexistent-pax-test-repo"))
)
stopifnot(
    is.data.frame(avail),
    nrow(avail) == 0L,
    all(c("Package", "Version", "Depends") %in% names(avail))
)
