test_that("package matrices become character data frames", {
  packages <- matrix(
    c(
      "example",
      "1.0.0",
      NA,
      "example",
      "2.0.0",
      "dependency"
    ),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(
      c("example", "example"),
      c("Package", "Version", "Imports")
    )
  )

  result <- zak:::as_package_data_frame(packages)

  expect_s3_class(result, "data.frame")
  expect_identical(names(result), colnames(packages))
  expect_identical(result$Package, c("example", "example"))
  expect_true(all(vapply(result, is.character, logical(1L))))
  expect_identical(rownames(result), c("1", "2"))
  expect_true(is.na(result$Imports[[1L]]))
})

test_that("empty package matrices retain their columns", {
  packages <- matrix(
    character(),
    nrow = 0L,
    ncol = 3L,
    dimnames = list(NULL, c("Package", "Version", "Repository"))
  )

  result <- zak:::as_package_data_frame(packages)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
  expect_identical(names(result), colnames(packages))
  expect_true(all(vapply(result, is.character, logical(1L))))
})

test_that("package data-frame conversion rejects invalid inputs", {
  expect_error(
    zak:::as_package_data_frame(list(Package = "example")),
    "must be a matrix"
  )
  expect_error(
    zak:::as_package_data_frame(matrix("example", nrow = 1L)),
    "must have column names"
  )
})

test_that("installed_packages returns an explorable data frame", {
  result <- zak::installed_packages(priority = "base")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("Package", "LibPath", "Version") %in% names(result)))
  expect_true(all(vapply(result, is.character, logical(1L))))
  expect_true(all(c("base", "utils") %in% result$Package))
})

test_that("installed_packages preserves an empty library schema", {
  library <- tempfile("zak-empty-library-")
  dir.create(library)
  on.exit(unlink(library, recursive = TRUE), add = TRUE)

  result <- zak::installed_packages(lib.loc = library)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
  expect_true(all(c("Package", "LibPath", "Version") %in% names(result)))
  expect_true(all(vapply(result, is.character, logical(1L))))
})

test_that("available_packages returns repository rows as a data frame", {
  fixture <- make_fixture_package("zakfixtureavailable")
  repository <- make_fixture_repository(list(fixture))
  on.exit(
    unlink(c(fixture$root, repository$root), recursive = TRUE),
    add = TRUE
  )

  result <- zak::available_packages(
    repos = repository$url,
    type = "source",
    filters = list()
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("Package", "Version", "Repository") %in% names(result)))
  expect_true(all(vapply(result, is.character, logical(1L))))
  expect_true(fixture$package %in% result$Package)
})

test_that("available_packages preserves an empty repository schema", {
  result <- zak::available_packages(repos = character(), type = "source")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
  expect_true(all(c("Package", "Version", "Depends") %in% names(result)))
  expect_true(all(vapply(result, is.character, logical(1L))))
})
