test_that("restore installs a locked URL from the cache offline", {
  testthat::skip_if_not_installed("jsonlite")
  fixture <- make_fixture_package("zakfixturerestore")
  cache <- tempfile("zak-cache-")
  library <- tempfile("zak-library-")
  dir.create(library)
  old <- options(zak.cache = cache, zak.offline = FALSE)
  on.exit(
    {
      options(old)
      unlink(c(fixture$root, cache, library), recursive = TRUE)
    },
    add = TRUE
  )
  url <- "https://example.test/zakfixturerestore_1.0.0.tar.gz"
  lockfile <- tempfile("zak-restore-", fileext = ".json")
  on.exit(unlink(lockfile), add = TRUE)
  downloads <- 0L
  testthat::local_mocked_bindings(
    download_archive = function(url) {
      downloads <<- downloads + 1L
      fixture$archive
    },
    .package = "zak"
  )

  package_plan <- suppressMessages(zak::plan(url, lib = library))
  zak::lock(package_plan, lockfile)
  previous <- options(zak.offline = TRUE)
  on.exit(options(previous), add = TRUE)

  result <- suppressMessages(zak::restore(lockfile, lib = library))

  expect_null(result)
  expect_identical(downloads, 1L)
  expect_true(dir.exists(file.path(library, fixture$package)))
})

test_that("restore blocks a lockfile with changed artifact provenance", {
  testthat::skip_if_not_installed("jsonlite")
  fixture <- make_fixture_package("zakfixturerestoredrift")
  cache <- tempfile("zak-cache-")
  library <- tempfile("zak-library-")
  dir.create(library)
  old <- options(zak.cache = cache, zak.offline = FALSE)
  on.exit(
    {
      options(old)
      unlink(c(fixture$root, cache, library), recursive = TRUE)
    },
    add = TRUE
  )
  lockfile <- tempfile("zak-restore-drift-", fileext = ".json")
  on.exit(unlink(lockfile), add = TRUE)
  testthat::local_mocked_bindings(
    download_archive = function(url) fixture$archive,
    .package = "zak"
  )

  package_plan <- suppressMessages(
    zak::plan(
      "https://example.test/zakfixturerestoredrift_1.0.0.tar.gz",
      lib = library
    )
  )
  zak::lock(package_plan, lockfile)
  document <- jsonlite::read_json(lockfile, simplifyVector = FALSE)
  document$acquisition$sha256 <- strrep("0", 64L)
  writeLines(
    jsonlite::toJSON(document, auto_unbox = TRUE, pretty = TRUE, na = "null"),
    lockfile,
    useBytes = TRUE
  )

  expect_snapshot(error = TRUE, zak::restore(lockfile, lib = library))
  expect_false(dir.exists(file.path(library, fixture$package)))
})

test_that("restore rejects lockfiles without acquired source checksums", {
  testthat::skip_if_not_installed("jsonlite")
  fixture <- make_fixture_package("zakfixturerestorelegacy")
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library)), add = TRUE)
  lockfile <- tempfile("zak-restore-legacy-", fileext = ".json")
  on.exit(unlink(lockfile), add = TRUE)
  testthat::local_mocked_bindings(
    download_archive = function(url) fixture$archive,
    .package = "zak"
  )

  package_plan <- suppressMessages(
    zak::plan(
      "https://example.test/zakfixturerestorelegacy_1.0.0.tar.gz",
      lib = library
    )
  )
  zak::lock(package_plan, lockfile)
  document <- jsonlite::read_json(lockfile, simplifyVector = FALSE)
  document$acquisition$sha256 <- NULL
  writeLines(
    jsonlite::toJSON(document, auto_unbox = TRUE, pretty = TRUE, na = "null"),
    lockfile,
    useBytes = TRUE
  )

  expect_snapshot(error = TRUE, zak::restore(lockfile, lib = library))
})

test_that("restore pins Git sources to the locked commit", {
  testthat::skip_if_not_installed("jsonlite")
  fixture <- make_fixture_package("zakfixturerestoregit")
  repository <- make_fixture_git_repository(fixture)
  cache <- tempfile("zak-cache-")
  library <- tempfile("zak-library-")
  dir.create(library)
  old <- options(zak.cache = cache, zak.offline = FALSE)
  on.exit(
    {
      options(old)
      unlink(c(fixture$root, cache, library), recursive = TRUE)
    },
    add = TRUE
  )
  lockfile <- tempfile("zak-restore-git-", fileext = ".json")
  on.exit(unlink(lockfile), add = TRUE)
  reference <- paste0("git::", repository$url, "@", repository$commit)

  package_plan <- suppressMessages(zak::plan(reference, lib = library))
  zak::lock(package_plan, lockfile)
  previous <- options(zak.offline = TRUE)
  on.exit(options(previous), add = TRUE)
  testthat::local_mocked_bindings(
    run_git = function(...) stop("unexpected Git command"),
    .package = "zak"
  )

  result <- suppressMessages(zak::restore(lockfile, lib = library))

  expect_null(result)
  expect_true(dir.exists(file.path(library, fixture$package)))
})
