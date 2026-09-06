test_that("source adapters dispatch parsed references", {
  references <- list(
    list(type = "url", url = "https://example.test/package.tar.gz"),
    list(type = "local", path = tempdir()),
    list(type = "local_archive", path = tempfile(), format = "tar.gz"),
    list(type = "git", url = "https://example.test/repo.git", ref = "main"),
    list(type = "bioconductor", package = "example"),
    list(type = "repository", package = "example")
  )

  adapters <- lapply(references, zak:::source_adapter)
  expect_identical(
    vapply(adapters, `[[`, character(1L), "type"),
    c("url", "local", "local_archive", "git", "bioconductor", "repository")
  )
  expect_identical(
    vapply(
      adapters,
      function(adapter) is.function(adapter$prepare),
      logical(1L)
    ),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
  expect_identical(
    lapply(adapters, class),
    list(
      c("zak_source_adapter", "list"),
      c("zak_source_adapter", "list"),
      c("zak_source_adapter", "list"),
      c("zak_source_adapter", "list"),
      c("zak_source_adapter", "list"),
      c("zak_source_adapter", "list")
    )
  )
})

test_that("local source adapters return a common preparation result", {
  fixture <- make_fixture_package("zakfixturesourceadapter")
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  reference <- zak:::parse_package_reference(fixture$source)
  result <- zak:::prepare_package_source(
    reference,
    lib = NULL,
    verbose = FALSE,
    platform = zak:::current_platform_facts()
  )

  expect_s3_class(result, "zak_source_result")
  expect_identical(
    names(result),
    c(
      "source",
      "metadata",
      "target",
      "acquisition",
      "artifact",
      "repositories",
      "configured"
    )
  )
  expect_identical(result$source, reference)
  expect_identical(result$metadata$package, fixture$package)
  expect_identical(result$metadata$format, "directory")
  expect_identical(result$target[1L, "Package"], fixture$package)
  expect_null(result$acquisition)
  expect_null(result$artifact)
})

test_that("URL source adapters keep acquisition separate from metadata", {
  fixture <- make_fixture_package("zakfixturesourceurl")
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  reference <- list(
    type = "url",
    url = "https://example.test/zakfixturesourceurl_1.0.0.tar.gz"
  )
  testthat::local_mocked_bindings(
    download_archive = function(url) fixture$archive,
    .package = "zak"
  )

  result <- zak:::prepare_package_source(
    reference,
    lib = NULL,
    verbose = FALSE,
    platform = zak:::current_platform_facts()
  )
  on.exit(zak:::release_acquired_artifact(result$artifact), add = TRUE)

  expect_identical(result$source, reference)
  expect_identical(result$metadata$package, fixture$package)
  expect_identical(result$acquisition$url, reference$url)
  expect_identical("path" %in% names(result$acquisition), FALSE)
  expect_s3_class(result$artifact, "zak_artifact")
})

test_that("Git source adapters return checkout and commit metadata", {
  fixture <- make_fixture_package("zakfixturesourcegit")
  repository <- make_fixture_git_repository(fixture)
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  reference <- list(
    type = "git",
    url = repository$url,
    ref = repository$commit
  )

  result <- zak:::prepare_package_source(
    reference,
    lib = NULL,
    verbose = FALSE,
    platform = zak:::current_platform_facts()
  )
  on.exit(zak:::release_acquired_artifact(result$artifact), add = TRUE)

  expect_s3_class(result, "zak_source_result")
  expect_identical(result$metadata$package, fixture$package)
  expect_identical(result$metadata$format, "directory")
  expect_identical(result$acquisition$type, "git")
  expect_identical(result$acquisition$commit, repository$commit)
  expect_s3_class(result$artifact, "zak_git_artifact")
})

test_that("unsupported source adapters fail explicitly", {
  expect_snapshot(error = TRUE, {
    zak:::source_adapter(list(type = "unknown", reference = "example"))
  })
})

test_that("a local directory without a DESCRIPTION is rejected clearly", {
  # This used to surface as a raw `read.dcf()` file-connection warning followed
  # by a subscript error. The Git adapter already guarded this; both now share
  # `read_source_description()`.
  directory <- tempfile("zak-source-nodescription-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)

  expect_error(
    zak::plan(directory),
    "must contain a top-level package DESCRIPTION file"
  )
})

test_that("an unreadable DESCRIPTION is rejected clearly", {
  directory <- tempfile("zak-source-emptydescription-")
  dir.create(directory)
  file.create(file.path(directory, "DESCRIPTION"))
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)

  expect_error(zak::plan(directory), "unreadable DESCRIPTION file")
})

test_that("a Git checkout without a DESCRIPTION is rejected clearly", {
  checkout <- tempfile("zak-source-gitnodescription-")
  dir.create(checkout)
  on.exit(unlink(checkout, recursive = TRUE), add = TRUE)

  expect_error(
    zak:::read_source_description(checkout, "A Git checkout"),
    "A Git checkout must contain a top-level package DESCRIPTION file"
  )
})
