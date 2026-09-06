test_that("Bioconductor and Remotes references normalize to source records", {
  expect_identical(
    zak:::parse_package_reference("bioc::GenomicRanges"),
    list(type = "bioconductor", package = "GenomicRanges")
  )
  expect_identical(
    zak:::parse_remotes(c(
      Remotes = paste(
        "github::owner/remoteone@main",
        "bioc::GenomicRanges",
        "url::https://example.test/remotetwo_1.0.0.tar.gz",
        sep = ", "
      )
    )),
    list(
      list(
        package = "remoteone",
        source = list(
          type = "git",
          url = "https://github.com/owner/remoteone.git",
          ref = "main"
        )
      ),
      list(
        package = "GenomicRanges",
        source = list(type = "bioconductor", package = "GenomicRanges")
      ),
      list(
        package = "remotetwo",
        source = list(
          type = "url",
          url = "https://example.test/remotetwo_1.0.0.tar.gz"
        )
      )
    )
  )
})

test_that("Remotes mappings are exposed on local package plans", {
  target <- make_fixture_package(
    "zakfixtureremotestarget",
    fields = list(
      Remotes = "github::owner/remoteone@main"
    )
  )
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(c(target$root, library), recursive = TRUE),
    add = TRUE
  )

  result <- suppressMessages(zak::plan(target$source, lib = library))

  expect_length(result$remotes, 1L)
  expect_identical(result$remotes[[1L]]$package, "remoteone")
  expect_identical(result$remotes[[1L]]$source$type, "git")
  expect_identical(result$remotes[[1L]]$source$ref, "main")
  expect_identical(result$target$package, target$package)
  expect_identical(result$status, "ready")
})

test_that("Bioconductor plans and locks use the Bioconductor repository set", {
  testthat::skip_if_not_installed("jsonlite")
  fixture <- make_fixture_package("zakfixturebioc")
  repository <- make_fixture_repository(list(fixture))
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(c(fixture$root, repository$root, library), recursive = TRUE),
    add = TRUE
  )
  testthat::local_mocked_bindings(
    bioc_repositories = function() c(BIOC = repository$url),
    .package = "zak"
  )

  with_fixture_options(repository$url, {
    result <- suppressMessages(
      zak::plan(paste0("bioc::", fixture$package), lib = library)
    )
  })

  expect_identical(
    result$source,
    list(type = "bioconductor", package = fixture$package)
  )
  expect_identical(result$target$package, fixture$package)
  expect_identical(result$target$format, "repository")
  expect_identical(unname(result$repositories[[1L]]), repository$url)
  expect_identical(
    result$candidates$target$source,
    list(type = "bioconductor", reference = fixture$package)
  )
  expect_match(result$candidates$target$repository, "/src/contrib$")
  file <- tempfile("zak-lock-bioc-", fileext = ".json")
  on.exit(unlink(file), add = TRUE)
  zak::lock(result, file)
  document <- jsonlite::read_json(file, simplifyVector = FALSE)
  expect_identical(document$source$type, "bioconductor")
  expect_identical(
    zak::compare_lock(zak::read_lock(file), result)$status,
    "current"
  )
})

test_that("Bioconductor package references install from their repository set", {
  fixture <- make_fixture_package("zakfixturebiocinstall")
  repository <- make_fixture_repository(list(fixture))
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(c(fixture$root, repository$root, library), recursive = TRUE),
    add = TRUE
  )
  testthat::local_mocked_bindings(
    bioc_repositories = function() c(BIOC = repository$url),
    .package = "zak"
  )

  with_fixture_options(repository$url, {
    expect_null(
      suppressMessages(
        zak::install(paste0("bioc::", fixture$package), lib = library)
      )
    )
  })

  installed <- utils::installed.packages(lib.loc = library)
  expect_true(fixture$package %in% installed[, "Package"])
})

test_that("unsupported Remotes syntax fails before resolution", {
  expect_error(
    zak:::parse_remotes(c(Remotes = "gitlab::owner/repository")),
    "Unsupported Remotes reference"
  )
  expect_error(
    zak:::parse_package_reference("bioc::not-a-package"),
    "package name"
  )
})
