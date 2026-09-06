test_that("package references identify repository names and local directories", {
  fixture <- make_fixture_package("zakfixtureparsedlocal")
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  expect_identical(
    zak:::parse_package_reference("example.package"),
    list(type = "repository", package = "example.package")
  )
  expect_identical(
    zak:::parse_package_reference(fixture$source),
    list(
      type = "local",
      path = normalizePath(fixture$source, mustWork = TRUE)
    )
  )
  expect_identical(
    zak:::parse_package_reference("https://example.test/package.tar.gz"),
    list(type = "url", url = "https://example.test/package.tar.gz")
  )
})

test_that("a local package directory can be planned and installed", {
  fixture <- make_fixture_package("zakfixturelocalinstall")
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  result <- suppressMessages(
    zak::plan(fixture$source, lib = library)
  )

  expect_identical(result$source$type, "local")
  expect_identical(result$source$path, normalizePath(fixture$source))
  expect_identical(result$target$package, fixture$package)
  expect_identical(result$target$version, fixture$version)
  expect_identical(result$target$format, "directory")
  expect_identical(result$status, "ready")
  expect_s3_class(result$platform, "zak_platform_facts")

  expect_null(
    suppressMessages(zak::install(fixture$source, lib = library))
  )
  installed <- utils::installed.packages(lib.loc = library)
  expect_identical(fixture$package %in% installed[, "Package"], TRUE)
})

test_that("a repository package can be planned and installed", {
  leaf <- make_fixture_package("zakfixturerepositoryleaf")
  target <- make_fixture_package(
    "zakfixturerepositorytarget",
    fields = list(Imports = "zakfixturerepositoryleaf")
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

  expect_identical(
    result$source,
    list(type = "repository", package = target$package)
  )
  expect_identical(result$target$package, target$package)
  expect_identical(result$target$version, target$version)
  expect_identical(result$target$format, "repository")
  expect_identical(result$dependencies[, "Package"], leaf$package)
  expect_identical(result$dependencies[, "Status"], "install")
  expect_s3_class(result$platform, "zak_platform_facts")

  with_fixture_options(repository$url, {
    expect_null(
      suppressMessages(zak::install(target$package, lib = library))
    )
  })
  installed <- utils::installed.packages(lib.loc = library)[, "Package"]
  expect_identical(all(c(leaf$package, target$package) %in% installed), TRUE)
})

test_that("unknown package references fail before installation", {
  unrelated <- make_fixture_package("zakfixtureunknownunrelated")
  repository <- make_fixture_repository(list(unrelated))
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(c(unrelated$root, repository$root, library), recursive = TRUE),
    add = TRUE
  )

  with_fixture_options(repository$url, {
    expect_snapshot(error = TRUE, {
      zak::plan("zakfixturemissingpackage", lib = library)
    })
  })
  expect_identical(list.files(library), character())
})
