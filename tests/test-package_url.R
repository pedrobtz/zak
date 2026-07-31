library(pax)

## A stand-in for available_packages(), so these tests need no network.
avail <- data.frame(
    Package = c("A", "B", "C"),
    Version = c("1.0", "2.0-1", "0.9"),
    Repository = c("https://cran.example.org/src/contrib",
                   "https://cran.example.org/src/contrib",
                   "https://other.example.org/src/contrib"),
    stringsAsFactors = FALSE
)

## type resolution -----------------------------------------------------------

stopifnot(
    identical(pax:::resolve_pkg_type("source"), "source"),
    identical(pax:::resolve_pkg_type("windows"), "win.binary"),
    identical(pax:::resolve_pkg_type("MacOS"), "mac.binary"),
    identical(pax:::resolve_pkg_type("linux"), "source"),
    identical(pax:::resolve_pkg_type("win.binary"), "win.binary"),
    ## a preference, not a file: resolves to whatever this platform uses
    identical(pax:::resolve_pkg_type("binary"), .Platform$pkgType),
    identical(pax:::resolve_pkg_type("both"), .Platform$pkgType)
)

stopifnot(
    identical(pax:::pkg_type_ext("source"), ".tar.gz"),
    identical(pax:::pkg_type_ext("win.binary"), ".zip"),
    identical(pax:::pkg_type_ext("mac.binary"), ".tgz"),
    ## platform-suffixed macOS types still resolve
    identical(pax:::pkg_type_ext("mac.binary.big-sur-arm64"), ".tgz"),
    inherits(try(pax:::pkg_type_ext("nonesuch"), silent = TRUE), "try-error")
)

## URL construction ----------------------------------------------------------

u <- package_url("A", type = "source", available = avail)
stopifnot(
    identical(names(u), "A"),
    identical(unname(u), "https://cran.example.org/src/contrib/A_1.0.tar.gz")
)

## the extension follows the requested type
stopifnot(
    endsWith(package_url("A", type = "windows", available = avail), "A_1.0.zip"),
    endsWith(package_url("A", type = "macos", available = avail), "A_1.0.tgz"),
    endsWith(package_url("A", type = "linux", available = avail), "A_1.0.tar.gz")
)

## vectorised, order preserved, per-package repository respected
u2 <- package_url(c("C", "A"), type = "source", available = avail)
stopifnot(
    identical(names(u2), c("C", "A")),
    identical(
        unname(u2),
        c("https://other.example.org/src/contrib/C_0.9.tar.gz",
          "https://cran.example.org/src/contrib/A_1.0.tar.gz")
    )
)

## an explicit File field wins over <Package>_<Version><ext>
avail_file <- avail
avail_file$File <- c(NA, "B_2.0-1_special.tar.gz", NA)
uf <- package_url(c("A", "B"), type = "source", available = avail_file)
stopifnot(
    endsWith(uf[["A"]], "/A_1.0.tar.gz"),
    identical(uf[["B"]],
              "https://cran.example.org/src/contrib/B_2.0-1_special.tar.gz")
)

## offered by several repositories: the first wins
dupes <- rbind(
    avail,
    data.frame(Package = "A", Version = "9.9",
               Repository = "https://mirror.example.org/src/contrib",
               stringsAsFactors = FALSE)
)
stopifnot(endsWith(package_url("A", type = "source", available = dupes),
                   "A_1.0.tar.gz"))

## unknown packages give NA plus a warning
warned <- FALSE
res <- withCallingHandlers(
    package_url(c("A", "nonesuch"), type = "source", available = avail),
    warning = function(w) {
        warned <<- TRUE
        invokeRestart("muffleWarning")
    }
)
stopifnot(warned, is.na(res[["nonesuch"]]), !is.na(res[["A"]]))

## zero-length input is not an error
z <- package_url(character(0), type = "source", available = avail)
stopifnot(is.character(z), length(z) == 0L)

## invalid input -------------------------------------------------------------

stopifnot(
    inherits(try(package_url(1L, available = avail), silent = TRUE),
             "try-error"),
    inherits(try(package_url("A", type = c("source", "windows"),
                             available = avail), silent = TRUE),
             "try-error"),
    inherits(try(package_url("A", type = NA_character_, available = avail),
                 silent = TRUE), "try-error"),
    inherits(try(package_url("A", type = "source", available = "nope"),
                 silent = TRUE), "try-error"),
    ## 'available' without the columns we need
    inherits(try(package_url("A", type = "source",
                             available = avail[, c("Package", "Version")]),
                 silent = TRUE), "try-error")
)
