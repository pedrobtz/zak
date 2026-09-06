test_that("archive inspection reports source package metadata", {
  fixture <- make_fixture_package("zakfixturearchive")
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  expect_message(
    metadata <- zak:::inspect_archive(fixture$archive, verbose = TRUE),
    "Package: 'zakfixturearchive' 1.0.0 \\(source\\)"
  )
  expect_identical(metadata$package, fixture$package)
  expect_identical(metadata$version, fixture$version)
  expect_identical(metadata$type, "source")
  expect_identical(metadata$format, "tar.gz")
  expect_identical(metadata$root, fixture$package)
})

test_that("ZIP inspection reports source package metadata", {
  fixture <- make_fixture_package("zakfixtureziparchive")
  archive <- make_fixture_zip(fixture)
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  expect_message(
    metadata <- zak:::inspect_archive(archive, verbose = TRUE),
    "Package: 'zakfixtureziparchive' 1.0.0 \\(source\\)"
  )
  expect_identical(metadata$package, fixture$package)
  expect_identical(metadata$version, fixture$version)
  expect_identical(metadata$type, "source")
  expect_identical(metadata$format, "zip")
  expect_identical(metadata$root, fixture$package)
})

test_that("a Built field identifies binary package metadata", {
  expect_identical(zak:::archive_package_type(c(Package = "example")), "source")
  expect_identical(
    zak:::archive_package_type(c(
      Package = "example",
      Built = "R 4.5.0; platform"
    )),
    "binary"
  )
})

test_that("SystemRequirements are reported only when present", {
  expect_message(
    zak:::report_system_requirements(
      c(SystemRequirements = "libcurl,\n  OpenSSL"),
      verbose = TRUE
    ),
    "System requirements: libcurl, OpenSSL"
  )
  expect_silent(
    zak:::report_system_requirements(c(Package = "example"), verbose = TRUE)
  )
  expect_silent(
    zak:::report_system_requirements(c(SystemRequirements = "  "))
  )
})

test_that("the download helper detects content and preserves its format", {
  fixture <- make_fixture_package("zakfixturedownload")
  archive <- tempfile(fileext = ".bin")
  file.copy(fixture$archive, archive)
  on.exit(unlink(c(fixture$root, archive), recursive = TRUE), add = TRUE)

  downloaded <- zak:::download_archive(zak:::file_url(archive))
  on.exit(unlink(downloaded), add = TRUE)
  expect_match(downloaded, "[.]tar[.]gz$")
  expect_identical(
    unname(tools::md5sum(downloaded)),
    unname(tools::md5sum(fixture$archive))
  )
})

test_that("the download helper preserves ZIP content", {
  fixture <- make_fixture_package("zakfixturezipdownload")
  source <- make_fixture_zip(fixture)
  archive <- tempfile(fileext = ".data")
  file.copy(source, archive)
  on.exit(unlink(c(fixture$root, archive), recursive = TRUE), add = TRUE)

  downloaded <- zak:::download_archive(zak:::file_url(archive))
  on.exit(unlink(downloaded), add = TRUE)
  expect_match(downloaded, "[.]zip$")
  expect_identical(
    unname(tools::md5sum(downloaded)),
    unname(tools::md5sum(source))
  )
})

test_that("archive acquisition returns a reusable artifact", {
  fixture <- make_fixture_package("zakfixtureacquired")
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  artifact <- zak:::acquire_archive(zak:::file_url(fixture$archive))
  on.exit(zak:::release_acquired_artifact(artifact), add = TRUE)

  expect_s3_class(artifact, "zak_artifact")
  expect_identical(artifact$url, zak:::file_url(fixture$archive))
  expect_identical(artifact$format, "tar.gz")
  expect_identical(artifact$size, unname(file.info(artifact$path)$size))
  expect_identical(file.exists(artifact$path), TRUE)
  expect_identical(
    names(zak:::archive_acquisition(artifact)),
    c("url", "format", "size", "retrieved_at", "method", "sha256")
  )
  expect_identical(artifact$sha256, zak:::sha256_file(artifact$path))

  metadata <- zak:::inspect_archive(artifact$path)
  expect_identical(metadata$package, fixture$package)

  zak:::release_acquired_artifact(artifact)
  expect_identical(file.exists(artifact$path), FALSE)
})

test_that("unsupported downloads fail with a format error", {
  archive <- tempfile()
  writeBin(as.raw(1:10), archive)
  on.exit(unlink(archive), add = TRUE)

  expect_error(
    zak:::download_archive(zak:::file_url(archive)),
    "tar.gz or ZIP"
  )
})

test_that("download failures are reported with the URL", {
  # `download.file()` signals failure differently per method: some raise a
  # condition, others only return a non-zero status. Ignoring the status let an
  # error page be treated as a package archive.
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("<html>404</html>", destfile)
      1L
    },
    .package = "utils"
  )
  expect_error(
    zak:::download_archive("https://example.test/missing_1.0.0.tar.gz"),
    "could not download https://example.test/missing_1.0.0.tar.gz"
  )
  expect_error(
    zak:::download_archive("https://example.test/missing_1.0.0.tar.gz"),
    "status 1"
  )
})

test_that("a download that raises a condition names the URL", {
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      stop("cannot open URL")
    },
    .package = "utils"
  )
  expect_error(
    zak:::download_archive("https://example.test/broken_1.0.0.tar.gz"),
    "could not download https://example.test/broken_1.0.0.tar.gz: cannot open URL"
  )
})

test_that("an empty download is rejected rather than inspected", {
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      file.create(destfile)
      0L
    },
    .package = "utils"
  )
  expect_error(
    zak:::download_archive("https://example.test/empty_1.0.0.tar.gz"),
    "downloaded no content from https://example.test/empty_1.0.0.tar.gz"
  )
})

test_that("a successful download of a non-archive names the URL", {
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("<html>not an archive</html>", destfile)
      0L
    },
    .package = "utils"
  )
  expect_error(
    zak:::download_archive("https://example.test/page_1.0.0.tar.gz"),
    "could not identify the content as a tar.gz or ZIP archive"
  )
  expect_error(
    zak:::download_archive("https://example.test/page_1.0.0.tar.gz"),
    "from https://example.test/page_1.0.0.tar.gz",
    fixed = TRUE
  )
})

test_that("download errors redact credentials in the URL", {
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) stop("refused"),
    .package = "utils"
  )
  message <- tryCatch(
    zak:::download_archive("https://ci:ghp_secret@example.test/p_1.0.0.tar.gz"),
    error = function(error) conditionMessage(error)
  )
  expect_false(grepl("ghp_secret", message, fixed = TRUE))
  expect_match(message, "https://<redacted>@example.test", fixed = TRUE)
})

test_that("artifact verification requires an independent expectation", {
  fixture <- make_fixture_package("zakverifyexpectation")
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  artifact <- zak:::acquire_local_archive(fixture$archive)

  # `expected` used to default to the artifact's own checksum, so callers that
  # omitted it compared the bytes against a hash just taken from those bytes.
  expect_error(
    zak:::verify_acquired_artifact(artifact),
    'argument "expected" is missing'
  )
  expect_silent(zak:::verify_acquired_artifact(artifact, artifact$sha256))
})

test_that("verification detects an artifact changed after acquisition", {
  fixture <- make_fixture_package("zakverifychanged")
  replacement <- make_fixture_package("zakverifychanged", version = "2.0.0")
  copy <- tempfile("zak-verify-", fileext = ".tar.gz")
  file.copy(fixture$archive, copy)
  on.exit(
    unlink(c(fixture$root, replacement$root, copy), recursive = TRUE),
    add = TRUE
  )

  artifact <- zak:::acquire_local_archive(copy)
  recorded <- artifact$sha256

  # The window this guards: the file backing a plan changes before install.
  file.copy(replacement$archive, copy, overwrite = TRUE)
  expect_error(
    zak:::verify_acquired_artifact(artifact, recorded),
    "failed SHA-256 verification"
  )
})

test_that("preparing a Git source hashes the checkout once", {
  testthat::skip_if(!nzchar(Sys.which("git")), "A git executable is required.")
  fixture <- make_fixture_package("zakverifygithash")
  repository <- make_fixture_git_repository(fixture)
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  calls <- 0L
  original <- zak:::git_checkout_sha256
  testthat::local_mocked_bindings(
    git_checkout_sha256 = function(path) {
      calls <<- calls + 1L
      original(path)
    },
    .package = "zak"
  )
  prepared <- suppressMessages(zak:::prepare_package_source(
    list(type = "git", url = repository$url, ref = repository$commit),
    NULL,
    FALSE,
    zak:::current_platform_facts()
  ))
  on.exit(zak:::cleanup_plan_artifact(prepared$artifact), add = TRUE)

  # Hashing a whole checkout is expensive; the redundant verification did it
  # a second time on every prepare.
  expect_identical(calls, 1L)
})
