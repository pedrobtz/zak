test_that("HTTP archive acquisition reuses the configured cache", {
  fixture <- make_fixture_package("zakfixturecachearchive")
  cache <- tempfile("zak-cache-")
  old <- options(zak.cache = cache, zak.offline = FALSE)
  on.exit(
    {
      options(old)
      unlink(c(fixture$root, cache), recursive = TRUE)
    },
    add = TRUE
  )
  downloads <- 0L
  testthat::local_mocked_bindings(
    download_archive = function(url) {
      downloads <<- downloads + 1L
      archive <- tempfile(fileext = ".tar.gz")
      file.copy(fixture$archive, archive)
      archive
    },
    .package = "zak"
  )

  first <- zak:::acquire_archive(
    "https://example.test/zakfixturecachearchive_1.0.0.tar.gz"
  )
  second <- zak:::acquire_archive(
    "https://example.test/zakfixturecachearchive_1.0.0.tar.gz"
  )

  expect_identical(downloads, 1L)
  expect_identical(first$method, "download.file")
  expect_identical(second$method, "cache")
  expect_identical(first$path, second$path)
  expect_identical(first$owned, FALSE)
  expect_identical(second$owned, FALSE)
  expect_identical(first$sha256, zak:::sha256_file(first$path))
  expect_identical(second$sha256, first$sha256)
  expect_identical(
    readLines(zak:::cache_checksum_file(first$path), warn = FALSE),
    first$sha256
  )
})

test_that("commit-resolved Git checkouts reuse the configured cache", {
  fixture <- make_fixture_package("zakfixturecachegit")
  repository <- make_fixture_git_repository(fixture)
  cache <- tempfile("zak-cache-")
  old <- options(zak.cache = cache)
  on.exit(
    {
      options(old)
      unlink(c(fixture$root, cache), recursive = TRUE)
    },
    add = TRUE
  )

  first <- zak:::acquire_git_repository(repository$url, repository$commit)
  on.exit(zak:::release_acquired_artifact(first), add = TRUE)
  testthat::local_mocked_bindings(
    git_available = function() FALSE,
    .package = "zak"
  )
  second <- zak:::acquire_git_repository(repository$url, repository$commit)
  on.exit(zak:::release_acquired_artifact(second), add = TRUE)

  expect_identical(first$method, "git clone")
  expect_identical(second$method, "cache")
  expect_identical(second$commit, repository$commit)
  expect_identical(file.exists(file.path(second$path, "DESCRIPTION")), TRUE)
  expect_identical(second$sha256, zak:::git_checkout_sha256(second$path))
})

test_that("cache checksums reject and repair tampered Git entries", {
  fixture <- make_fixture_package("zakfixturecachegitchecksum")
  repository <- make_fixture_git_repository(fixture)
  cache <- tempfile("zak-cache-")
  old <- options(zak.cache = cache)
  on.exit(
    {
      options(old)
      unlink(c(fixture$root, cache), recursive = TRUE)
    },
    add = TRUE
  )

  first <- zak:::acquire_git_repository(repository$url, repository$commit)
  on.exit(zak:::release_acquired_artifact(first), add = TRUE)
  archive <- zak:::cache_git_file(repository$url, repository$commit)
  writeLines(strrep("0", 64L), zak:::cache_checksum_file(archive))
  repaired <- zak:::acquire_git_repository(repository$url, repository$commit)
  on.exit(zak:::release_acquired_artifact(repaired), add = TRUE)
  reused <- zak:::acquire_git_repository(repository$url, repository$commit)
  on.exit(zak:::release_acquired_artifact(reused), add = TRUE)

  expect_identical(first$method, "git clone")
  expect_identical(repaired$method, "git clone")
  expect_identical(reused$method, "cache")
  expect_identical(reused$sha256, repaired$sha256)
})

test_that("cache failures preserve temporary archive acquisition", {
  fixture <- make_fixture_package("zakfixturecachefallback")
  cache_file <- tempfile("zak-cache-file-")
  file.create(cache_file)
  old <- options(zak.cache = cache_file)
  on.exit(
    {
      options(old)
      unlink(c(fixture$root, cache_file), recursive = TRUE)
    },
    add = TRUE
  )
  testthat::local_mocked_bindings(
    download_archive = function(url) {
      archive <- tempfile(fileext = ".tar.gz")
      file.copy(fixture$archive, archive)
      archive
    },
    .package = "zak"
  )

  artifact <- zak:::acquire_archive(
    "https://example.test/zakfixturecachefallback_1.0.0.tar.gz"
  )
  on.exit(zak:::release_acquired_artifact(artifact), add = TRUE)

  expect_identical(artifact$method, "download.file")
  expect_identical(artifact$owned, TRUE)
})

test_that("fresh archive acquisition repairs an invalid cache entry", {
  fixture <- make_fixture_package("zakfixturecacherepair")
  cache <- tempfile("zak-cache-")
  old <- options(zak.cache = cache)
  on.exit(
    {
      options(old)
      unlink(c(fixture$root, cache), recursive = TRUE)
    },
    add = TRUE
  )
  url <- "https://example.test/zakfixturecacherepair_1.0.0.tar.gz"
  cache_path <- zak:::cache_archive_file(url)
  writeLines("not an archive", cache_path)
  downloads <- 0L
  testthat::local_mocked_bindings(
    download_archive = function(url) {
      downloads <<- downloads + 1L
      archive <- tempfile(fileext = ".tar.gz")
      file.copy(fixture$archive, archive)
      archive
    },
    .package = "zak"
  )

  repaired <- zak:::acquire_archive(url)
  reused <- zak:::acquire_archive(url)

  expect_identical(downloads, 1L)
  expect_identical(repaired$method, "download.file")
  expect_identical(reused$method, "cache")
  expect_identical(reused$format, "tar.gz")
})

test_that("cache checksums reject and repair tampered archive entries", {
  fixture <- make_fixture_package("zakfixturecachechecksum")
  cache <- tempfile("zak-cache-")
  old <- options(zak.cache = cache)
  on.exit(
    {
      options(old)
      unlink(c(fixture$root, cache), recursive = TRUE)
    },
    add = TRUE
  )
  url <- "https://example.test/zakfixturecachechecksum_1.0.0.tar.gz"
  downloads <- 0L
  testthat::local_mocked_bindings(
    download_archive = function(url) {
      downloads <<- downloads + 1L
      archive <- tempfile(fileext = ".tar.gz")
      file.copy(fixture$archive, archive)
      archive
    },
    .package = "zak"
  )

  first <- zak:::acquire_archive(url)
  writeLines(strrep("0", 64L), zak:::cache_checksum_file(first$path))
  repaired <- zak:::acquire_archive(url)
  reused <- zak:::acquire_archive(url)

  expect_identical(downloads, 2L)
  expect_identical(repaired$method, "download.file")
  expect_identical(reused$method, "cache")
  expect_identical(reused$sha256, zak:::sha256_file(reused$path))
})

test_that("cached Git remotes install without another Git command", {
  remote <- make_fixture_package("zakfixturecachedependency")
  remote_git <- make_fixture_git_repository(remote)
  target <- make_fixture_package(
    "zakfixturecachetarget",
    fields = list(
      Imports = remote$package,
      Remotes = paste0("git::", remote_git$url, "@", remote_git$commit)
    )
  )
  cache <- tempfile("zak-cache-")
  library <- tempfile("zak-library-")
  dir.create(library)
  old <- options(zak.cache = cache)
  on.exit(
    {
      options(old)
      unlink(c(remote$root, target$root, cache, library), recursive = TRUE)
    },
    add = TRUE
  )

  suppressMessages(zak::plan(target$source, lib = library))
  testthat::local_mocked_bindings(
    run_git = function(...) stop("unexpected Git command"),
    .package = "zak"
  )

  expect_null(suppressMessages(zak::install(target$source, lib = library)))
  installed <- utils::installed.packages(lib.loc = library)
  expect_identical(remote$package %in% installed[, "Package"], TRUE)
})

test_that("cache_info reports integrity and redacted source metadata", {
  fixture <- make_fixture_package("zakfixturecacheinfo")
  cache <- tempfile("zak-cache-")
  old <- options(zak.cache = cache)
  on.exit(
    {
      options(old)
      unlink(c(fixture$root, cache), recursive = TRUE)
    },
    add = TRUE
  )
  url <- paste0(
    "https://example.test/zakfixturecacheinfo_1.0.0.tar.gz",
    "?token=secret&channel=stable"
  )
  testthat::local_mocked_bindings(
    download_archive = function(url) fixture$archive,
    .package = "zak"
  )

  artifact <- zak:::acquire_archive(url)
  info <- zak::cache_info(cache)

  expect_identical(nrow(info), 1L)
  expect_identical(info$type, "archive")
  expect_identical(
    info$source,
    paste0(
      "https://example.test/zakfixturecacheinfo_1.0.0.tar.gz",
      "?token=<redacted>&channel=stable"
    )
  )
  expect_identical(info$sha256, artifact$sha256)
  expect_identical(info$valid, TRUE)
})

test_that("cache_clean previews and removes cache entries", {
  fixture <- make_fixture_package("zakfixturecacheclean")
  cache <- tempfile("zak-cache-")
  old <- options(zak.cache = cache)
  on.exit(
    {
      options(old)
      unlink(c(fixture$root, cache), recursive = TRUE)
    },
    add = TRUE
  )
  url <- "https://example.test/zakfixturecacheclean_1.0.0.tar.gz"
  testthat::local_mocked_bindings(
    download_archive = function(url) fixture$archive,
    .package = "zak"
  )

  artifact <- zak:::acquire_archive(url)
  preview <- zak::cache_clean(cache, dry_run = TRUE)

  expect_identical(nrow(preview), 1L)
  expect_identical(preview$path, artifact$path)
  expect_true(file.exists(artifact$path))

  removed <- zak::cache_clean(cache)

  expect_identical(nrow(removed), 1L)
  expect_false(file.exists(artifact$path))
  expect_identical(nrow(zak::cache_info(cache)), 0L)
})

test_that("cache_clean retains entries within the requested age", {
  old_fixture <- make_fixture_package("zakfixturecacheold")
  new_fixture <- make_fixture_package("zakfixturecachenew")
  cache <- tempfile("zak-cache-")
  old <- options(zak.cache = cache)
  on.exit(
    {
      options(old)
      unlink(c(old_fixture$root, new_fixture$root, cache), recursive = TRUE)
    },
    add = TRUE
  )
  old_url <- "https://example.test/zakfixturecacheold_1.0.0.tar.gz"
  new_url <- "https://example.test/zakfixturecachenew_1.0.0.tar.gz"
  testthat::local_mocked_bindings(
    download_archive = function(url) {
      if (identical(url, old_url)) old_fixture$archive else new_fixture$archive
    },
    .package = "zak"
  )

  old_artifact <- zak:::acquire_archive(old_url)
  zak:::acquire_archive(new_url)
  Sys.setFileTime(old_artifact$path, Sys.time() - 3600)

  selected <- zak::cache_clean(cache, max_age = 60, dry_run = TRUE)

  expect_identical(nrow(selected), 1L)
  expect_identical(selected$source, old_url)
  expect_true(file.exists(old_artifact$path))
  expect_identical(nrow(zak::cache_info(cache)), 2L)
})

test_that("offline mode reuses a valid archive cache without downloading", {
  fixture <- make_fixture_package("zakfixturecacheoffline")
  cache <- tempfile("zak-cache-")
  old <- options(zak.cache = cache, zak.offline = FALSE)
  on.exit(
    {
      options(old)
      unlink(c(fixture$root, cache), recursive = TRUE)
    },
    add = TRUE
  )
  url <- "https://example.test/zakfixturecacheoffline_1.0.0.tar.gz"
  downloads <- 0L
  testthat::local_mocked_bindings(
    download_archive = function(url) {
      downloads <<- downloads + 1L
      fixture$archive
    },
    .package = "zak"
  )
  zak:::release_acquired_artifact(zak:::acquire_archive(url))

  previous <- options(zak.offline = TRUE)
  on.exit(options(previous), add = TRUE)
  artifact <- zak:::acquire_archive(url)
  on.exit(zak:::release_acquired_artifact(artifact), add = TRUE)

  expect_identical(artifact$method, "cache")
  expect_identical(downloads, 1L)
})

test_that("offline mode reports an uncached archive", {
  cache <- tempfile("zak-cache-")
  old <- options(zak.cache = cache, zak.offline = TRUE)
  on.exit(
    {
      options(old)
      unlink(cache, recursive = TRUE)
    },
    add = TRUE
  )

  expect_snapshot(
    error = TRUE,
    zak:::acquire_archive(
      "https://example.test/zakfixtureuncachedoffline_1.0.0.tar.gz"
    )
  )
})

test_that("offline mode reports an uncached Git source", {
  cache <- tempfile("zak-cache-")
  old <- options(zak.cache = cache, zak.offline = TRUE)
  on.exit(
    {
      options(old)
      unlink(cache, recursive = TRUE)
    },
    add = TRUE
  )

  expect_snapshot(
    error = TRUE,
    zak:::acquire_git_repository(
      "https://example.test/zakfixtureuncachedoffline.git",
      strrep("0", 40L)
    )
  )
})
