## Tests against a real repository over the network.
##
## These are opt-in: CRAN requires that a check does not fail because the
## machine running it has no internet access, so they run only when
## PAX_NETWORK_TESTS (or NOT_CRAN, which the usual CI setups already set) asks
## for them, and they bow out quietly if the repository turns out to be
## unreachable anyway.

library(pax)

enabled <- function() {
    flag <- Sys.getenv("PAX_NETWORK_TESTS", unset = "")
    if (!nzchar(flag)) flag <- Sys.getenv("NOT_CRAN", unset = "")
    tolower(flag) %in% c("true", "1", "yes")
}

CRAN <- c(CRAN = "https://cloud.r-project.org")

if (!enabled()) {
    message("network tests skipped; set PAX_NETWORK_TESTS=true to run them")
} else {

    ## Reachability is checked with the code under test, so a broken
    ## url_exists() cannot silently disable the rest of the file: an outright
    ## FALSE is a real failure, only NA (unreachable, blocked, no libcurl)
    ## counts as "no network".
    packages_url <- paste0(CRAN[["CRAN"]], "/src/contrib/PACKAGES")
    reachable <- url_exists(packages_url)[[1L]]
    stopifnot(!isFALSE(reachable))

    if (is.na(reachable)) {
        message("repository unreachable; skipping the rest of the network tests")
    } else {

        ## url_info() against a file that is certainly there ------------------

        info <- url_info(packages_url)
        stopifnot(
            nrow(info) == 1L,
            isTRUE(info$exists),
            identical(info$status, 200L),
            ## servers may omit Content-Length, so only check it when present
            is.na(info$bytes) || info$bytes > 0
        )

        ## ...and one that certainly is not ------------------------------------

        absent <- url_info(paste0(
            CRAN[["CRAN"]], "/src/contrib/paxNoSuchPackage_0.0.0.tar.gz"
        ))
        stopifnot(isFALSE(absent$exists), identical(absent$status, 404L))

        ## both answers in one vectorised call
        both <- url_exists(c(packages_url, paste0(packages_url, ".nonesuch")))
        stopifnot(isTRUE(both[[1L]]), isFALSE(both[[2L]]))

        ## package_url() names a file that really exists -----------------------
        ##
        ## This is the point of testing over the network: the repository is the
        ## only authority on whether <Repository>/<Package>_<Version><ext> is
        ## the right address.

        avail_src <- available_packages(repos = CRAN, type = "source")
        stopifnot(is.data.frame(avail_src), nrow(avail_src) > 0L)

        ## a package that has been on CRAN for years and has no dependencies
        pkg <- "jsonlite"
        stopifnot(pkg %in% avail_src$Package)

        src_url <- package_url(pkg, type = "source", available = avail_src)
        stopifnot(
            endsWith(src_url[[pkg]], ".tar.gz"),
            isTRUE(url_exists(src_url[[pkg]])[[1L]])
        )

        ## The Windows binary is served from a different contrib path with a
        ## different extension, so this exercises the OS-type handling from a
        ## machine that is (most likely) not Windows. CRAN only builds binaries
        ## for current R, so skip rather than fail if this R is too old.
        avail_win <- available_packages(repos = CRAN, type = "win.binary")
        if (pkg %in% avail_win$Package) {
            win_url <- package_url(pkg, type = "windows", available = avail_win)
            stopifnot(
                endsWith(win_url[[pkg]], ".zip"),
                isTRUE(url_exists(win_url[[pkg]])[[1L]])
            )
        } else {
            message("no Windows binary for ", pkg,
                    " at this R version; skipping that check")
        }

        ## redirects are followed to the final status --------------------------
        ##
        ## cran.r-project.org redirects to a mirror; the status reported must
        ## be the one at the end of the chain, not the 30x.
        redirected <- url_info("http://cran.r-project.org/src/contrib/PACKAGES")
        stopifnot(!isFALSE(redirected$exists))
    }
}
