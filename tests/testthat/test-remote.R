test_that("URL remote metadata matches the remotes convention", {
  expect_identical(
    zak:::url_remote_metadata("https://example.test/package.tar.gz"),
    list(
      RemoteType = "url",
      RemoteUrl = "https://example.test/package.tar.gz",
      RemoteSubdir = NULL
    )
  )
})

test_that("remote metadata updates source and binary descriptions", {
  fixture <- make_fixture_package(
    "zakfixtureremotemetadata",
    fields = list(
      RemoteType = "github",
      RemoteUrl = "https://old.example",
      RemoteSubdir = "legacy"
    )
  )
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  meta <- file.path(fixture$source, "Meta")
  dir.create(meta)
  package_description <- list(
    DESCRIPTION = c(
      Package = fixture$package,
      Version = fixture$version,
      RemoteType = "github",
      RemoteUrl = "https://old.example",
      RemoteSubdir = "legacy"
    )
  )
  saveRDS(package_description, file.path(meta, "package.rds"))
  writeLines(
    c(
      "first *DESCRIPTION",
      "second *Meta/package.rds",
      "third *R/fixture.R"
    ),
    file.path(fixture$source, "MD5")
  )

  url <- "https://example.test/zakfixtureremotemetadata_1.0.0.tar.gz"
  zak:::add_url_remote_metadata(fixture$source, url)

  description <- read.dcf(file.path(fixture$source, "DESCRIPTION"))
  expect_identical(unname(description[1L, "RemoteType"]), "url")
  expect_identical(unname(description[1L, "RemoteUrl"]), url)
  expect_false("RemoteSubdir" %in% colnames(description))

  binary <- readRDS(file.path(meta, "package.rds"))$DESCRIPTION
  expect_identical(unname(binary["RemoteType"]), "url")
  expect_identical(unname(binary["RemoteUrl"]), url)
  expect_false("RemoteSubdir" %in% names(binary))
  expect_identical(
    readLines(file.path(fixture$source, "MD5")),
    "third *R/fixture.R"
  )
})

test_that("installed binary metadata is located by package and version", {
  fixture <- make_fixture_package("zakfixtureinstalledremote")
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  utils::install.packages(
    fixture$archive,
    repos = NULL,
    type = "source",
    lib = library,
    quiet = TRUE
  )
  metadata <- list(package = fixture$package, version = fixture$version)
  url <- "https://example.test/zakfixtureinstalledremote_1.0.0.zip"

  expect_true(zak:::record_installed_url_remote(metadata, url, library))
  description <- read.dcf(file.path(library, fixture$package, "DESCRIPTION"))
  expect_identical(unname(description[1L, "RemoteType"]), "url")
  expect_identical(unname(description[1L, "RemoteUrl"]), url)
})
