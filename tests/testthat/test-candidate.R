test_that("URL plans expose a normalized target candidate", {
  fixture <- make_fixture_package(
    "zakfixturecandidateurl",
    fields = list(
      Depends = "R (>= 4.0.0)",
      SystemRequirements = "libcurl"
    )
  )
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download_archive = function(url) fixture$archive,
    .package = "zak"
  )
  url <- "https://example.test/zakfixturecandidateurl_1.0.0.tar.gz"
  result <- suppressMessages(zak::plan(url, lib = library))
  candidate <- result$candidates$target

  expect_s3_class(candidate, "zak_candidate")
  expect_identical(
    names(candidate),
    c(
      "package",
      "version",
      "source",
      "repository",
      "platform",
      "r_compatibility",
      "type",
      "format",
      "hashes",
      "size",
      "system_requirements",
      "provenance"
    )
  )
  expect_identical(candidate$package, fixture$package)
  expect_identical(candidate$source, list(type = "url", reference = url))
  expect_null(candidate$repository)
  expect_identical(candidate$platform, result$platform)
  expect_identical(candidate$r_compatibility$constraint, ">= 4.0.0")
  expect_identical(
    candidate$hashes,
    list(sha256 = result$acquisition$sha256)
  )
  expect_identical(candidate$size, as.numeric(result$acquisition$size))
  expect_identical(candidate$system_requirements, "libcurl")
  expect_identical(candidate$provenance$source, "url")
  expect_identical(candidate$provenance$reference, url)
  expect_identical(candidate$provenance$acquired_from, url)
  expect_identical("path" %in% names(candidate$provenance), FALSE)
})

test_that("repository plans expose candidates with repository provenance", {
  leaf <- make_fixture_package("zakfixturecandidateleaf")
  target <- make_fixture_package(
    "zakfixturecandidatetarget",
    fields = list(Imports = "zakfixturecandidateleaf")
  )
  repository <- make_fixture_repository(list(leaf, target))
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(
      c(leaf$root, target$root, repository$root, library),
      recursive = TRUE
    ),
    add = TRUE
  )

  with_fixture_options(repository$url, {
    result <- suppressMessages(
      zak::plan(target$package, lib = library)
    )
  })
  target_candidate <- result$candidates$target
  dependency_candidate <- result$candidates$dependencies[[leaf$package]]

  expect_identical(
    target_candidate$source,
    list(type = "repository", reference = target$package)
  )
  expect_identical(
    dependency_candidate$source,
    list(type = "repository", reference = leaf$package)
  )
  expect_identical(dependency_candidate$package, leaf$package)
  expect_identical(dependency_candidate$version, leaf$version)
  expect_identical(dependency_candidate$platform, result$platform)
  expect_identical(dependency_candidate$provenance$source, "repository")
  expect_identical(
    dependency_candidate$provenance$reference,
    leaf$package
  )
  expect_null(dependency_candidate$provenance$acquired_from)
})

test_that("candidate normalization rejects incomplete platform facts", {
  platform <- zak:::current_platform_facts()
  platform$extra <- "unexpected"

  expect_snapshot(error = TRUE, {
    zak:::new_package_candidate(
      package = "example",
      version = "1.0.0",
      source = list(type = "repository", package = "example"),
      repository = NULL,
      platform = platform,
      type = "source",
      format = "repository",
      fields = c(Package = "example", Version = "1.0.0")
    )
  })
})
