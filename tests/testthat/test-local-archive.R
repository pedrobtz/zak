test_that("local package archives are parsed with their declared format", {
  fixture <- make_fixture_package("zakfixturelocalarchiveparse")
  zip <- make_fixture_zip(fixture)
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  expect_identical(
    zak:::parse_package_reference(fixture$archive),
    list(
      type = "local_archive",
      path = normalizePath(fixture$archive, mustWork = TRUE),
      format = "tar.gz"
    )
  )
  expect_identical(
    zak:::parse_package_reference(zip),
    list(
      type = "local_archive",
      path = normalizePath(zip, mustWork = TRUE),
      format = "zip"
    )
  )
})

test_that("local tar.gz archives use the common plan and remain untouched", {
  fixture <- make_fixture_package("zakfixturelocalarchivetar")
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  result <- suppressMessages(zak::plan(fixture$archive, lib = library))

  expect_identical(result$source$type, "local_archive")
  expect_identical(result$source$format, "tar.gz")
  expect_identical(result$target$package, fixture$package)
  expect_identical(result$target$format, "tar.gz")
  expect_identical(result$acquisition$method, "local")
  expect_match(result$acquisition$url, "^file://")
  expect_identical(result$candidates$target$source$type, "local_archive")
  expect_identical(result$candidates$target$source$format, "tar.gz")
  expect_identical(result$candidates$target$size, result$acquisition$size)
  expect_true(file.exists(fixture$archive))
})

test_that("local source ZIP archives can be installed", {
  fixture <- make_fixture_package("zakfixturelocalarchivezip")
  zip <- make_fixture_zip(fixture)
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  expect_null(suppressMessages(zak::install(zip, lib = library)))
  installed <- utils::installed.packages(lib.loc = library)
  expect_true(fixture$package %in% installed[, "Package"])
  expect_true(file.exists(zip))
  description <- read.dcf(
    file.path(library, fixture$package, "DESCRIPTION")
  )
  expect_false("RemoteType" %in% colnames(description))
})

test_that("local archive references reject unsupported and malformed files", {
  fixture <- make_fixture_package("zakfixturelocalarchivemismatch")
  unsupported <- tempfile("zak-local-reference-", fileext = ".txt")
  malformed <- tempfile("zak-local-reference-", fileext = ".tar.gz")
  mismatch <- tempfile("zak-local-reference-", fileext = ".zip")
  writeLines("not an R package archive", unsupported)
  writeBin(as.raw(seq_len(10L)), malformed)
  file.copy(fixture$archive, mismatch)
  on.exit(
    unlink(c(fixture$root, unsupported, malformed, mismatch)),
    add = TRUE
  )

  expect_error(
    zak:::parse_package_reference(unsupported),
    "Local files must be `.tar.gz` or `.zip` package archives"
  )
  expect_error(
    zak::plan(malformed),
    "could not identify the content as a tar.gz or ZIP archive"
  )
  expect_error(
    zak::plan(mismatch),
    "Local archive extension indicates `zip`, but its content is `tar.gz`"
  )
  expect_true(file.exists(malformed))
})

test_that("local archive artifacts are not released as downloaded files", {
  fixture <- make_fixture_package("zakfixturelocalartifact")
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  artifact <- zak:::acquire_local_archive(fixture$archive)

  zak:::release_acquired_artifact(artifact)

  expect_true(file.exists(fixture$archive))
})
