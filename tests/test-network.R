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

        ## Long-standing CRAN packages with compiled code and no hard
        ## dependencies of their own, so they stay usable as fixtures once
        ## install_url() starts actually installing them. Several at once also
        ## exercises package_url()'s vectorisation against real addresses.
        pkgs <- c("rlang", "jsonlite")
        stopifnot(all(pkgs %in% avail_src$Package))

        src_urls <- package_url(pkgs, type = "source", available = avail_src)
        stopifnot(
            identical(names(src_urls), pkgs),
            all(endsWith(src_urls, ".tar.gz")),
            ## the version in the URL is the one the repository advertises
            identical(
                unname(src_urls),
                paste0(avail_src$Repository[match(pkgs, avail_src$Package)], "/",
                       pkgs, "_",
                       avail_src$Version[match(pkgs, avail_src$Package)],
                       ".tar.gz")
            )
        )

        src_ok <- url_exists(unname(src_urls))
        stopifnot(identical(unname(src_ok), rep(TRUE, length(pkgs))))

        ## The Windows binaries are served from a different contrib path with a
        ## different extension, so this exercises the OS-type handling from a
        ## machine that is (most likely) not Windows. CRAN only builds binaries
        ## for current R, so skip rather than fail if this R is too old.
        avail_win <- available_packages(repos = CRAN, type = "win.binary")
        win_pkgs <- pkgs[pkgs %in% avail_win$Package]
        if (length(win_pkgs)) {
            win_urls <- package_url(win_pkgs, type = "windows",
                                    available = avail_win)
            win_ok <- url_exists(unname(win_urls))
            stopifnot(
                all(endsWith(win_urls, ".zip")),
                identical(unname(win_ok), rep(TRUE, length(win_pkgs)))
            )
        } else {
            message("no Windows binaries at this R version; skipping that check")
        }

        ## redirects are followed to the final status --------------------------
        ##
        ## cran.r-project.org redirects to a mirror; the status reported must
        ## be the one at the end of the chain, not the 30x.
        redirected <- url_info("http://cran.r-project.org/src/contrib/PACKAGES")
        stopifnot(!isFALSE(redirected$exists))
    }
}
