test_that("plan describes a URL target without installing it", {
  fixture <- make_fixture_package("zakfixtureplantarget")
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download_archive = function(url) fixture$archive,
    .package = "zak"
  )

  result <- suppressMessages(
    zak::plan(
      "https://example.test/zakfixtureplantarget_1.0.0.tar.gz",
      lib = library
    )
  )

  expect_s3_class(result, "zak_plan")
  expect_identical(
    result$source,
    list(
      type = "url",
      url = "https://example.test/zakfixtureplantarget_1.0.0.tar.gz"
    )
  )
  expect_identical(
    result$target,
    list(
      package = fixture$package,
      version = fixture$version,
      type = "source",
      format = "tar.gz"
    )
  )
  expect_identical(result$status, "ready")
  expect_identical(nrow(result$dependencies), 0L)
  expect_identical(result$acquisition$url, result$source$url)
  expect_identical(result$acquisition$format, "tar.gz")
  expect_identical("path" %in% names(result$acquisition), FALSE)
  expect_identical(dir.exists(file.path(library, fixture$package)), FALSE)
})

test_that("plan records dependency order, versions, and constraints", {
  leaf <- make_fixture_package("zakfixtureplanleaf")
  direct <- make_fixture_package(
    "zakfixtureplandirect",
    fields = list(Imports = "zakfixtureplanleaf")
  )
  target <- make_fixture_package(
    "zakfixtureplantargetdeps",
    fields = list(Imports = "zakfixtureplandirect (>= 1.0.0)")
  )
  repository <- make_fixture_repository(list(leaf, direct))
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(
      c(leaf$root, direct$root, target$root, repository$root, library),
      recursive = TRUE
    ),
    add = TRUE
  )

  testthat::local_mocked_bindings(
    download_archive = function(url) target$archive,
    .package = "zak"
  )

  with_fixture_options(repository$url, {
    result <- suppressMessages(
      zak::plan(
        "https://example.test/zakfixtureplantargetdeps_1.0.0.tar.gz",
        lib = library
      )
    )
  })

  expect_identical(result$status, "ready")
  expect_identical(
    result$dependencies[, "Package"],
    c("zakfixtureplanleaf", "zakfixtureplandirect")
  )
  expect_identical(result$dependencies[, "Version"], c("1.0.0", "1.0.0"))
  expect_identical(result$dependencies[, "Status"], c("install", "install"))
  expect_identical(result$dependencies[, "Direct"], c(FALSE, TRUE))
  expect_identical(
    result$dependencies[, "Constraint"],
    c("", ">= 1.0.0")
  )
  expect_identical(dir.exists(file.path(library, target$package)), FALSE)
})

test_that("plan reports unavailable dependencies without installing", {
  target <- make_fixture_package(
    "zakfixtureplanunavailable",
    fields = list(Imports = "zakfixtureplanmissing")
  )
  unrelated <- make_fixture_package("zakfixtureplanunrelated")
  repository <- make_fixture_repository(list(unrelated))
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(
      c(target$root, unrelated$root, repository$root, library),
      recursive = TRUE
    ),
    add = TRUE
  )

  testthat::local_mocked_bindings(
    download_archive = function(url) target$archive,
    .package = "zak"
  )

  with_fixture_options(repository$url, {
    expect_warning(
      result <- suppressMessages(
        zak::plan(
          "https://example.test/zakfixtureplanunavailable_1.0.0.tar.gz",
          lib = library
        )
      ),
      "not available"
    )
  })

  expect_identical(result$status, "blocked")
  expect_identical(result$dependencies[, "Package"], "zakfixtureplanmissing")
  expect_identical(result$dependencies[, "Status"], "unavailable")
  expect_identical(is.na(result$dependencies[1L, "Version"]), TRUE)
  expect_identical(dir.exists(file.path(library, target$package)), FALSE)
})

test_that("plan rejects malformed archives", {
  archive <- tempfile(fileext = ".bin")
  writeBin(as.raw(1:10), archive)
  on.exit(unlink(archive), add = TRUE)

  testthat::local_mocked_bindings(
    download_archive = function(url) archive,
    .package = "zak"
  )

  expect_snapshot(error = TRUE, {
    zak::plan("https://example.test/malformed.tar.gz")
  })
  expect_identical(file.exists(archive), FALSE)
})

test_that("zak plans print a concise summary", {
  fixture <- make_fixture_package("zakfixtureplanprint")
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download_archive = function(url) fixture$archive,
    .package = "zak"
  )

  result <- suppressMessages(
    zak::plan("https://example.test/zakfixtureplanprint_1.0.0.tar.gz")
  )

  expect_output(
    print(result),
    "zak install plan: zakfixtureplanprint 1[.]0[.]0 \\(source\\)"
  )
  expect_output(print(result), "Status: ready")
})
