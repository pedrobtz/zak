test_that("platform facts normalize operating systems and architectures", {
  macos <- zak:::current_platform_facts(
    os_type = "unix",
    r_os = "darwin23",
    r_arch = "aarch64",
    r_version = "4.6.1",
    package_type = "both"
  )
  expect_identical(
    macos,
    structure(
      list(
        os = "macos",
        architecture = "arm64",
        r_version = "4.6.1",
        package_type = "both"
      ),
      class = c("zak_platform_facts", "list")
    )
  )

  windows <- zak:::current_platform_facts(
    os_type = "windows",
    r_os = "mingw32",
    r_arch = "amd64",
    r_version = "4.5.0",
    package_type = "win.binary"
  )
  expect_identical(windows$os, "windows")
  expect_identical(windows$architecture, "x86_64")
  expect_identical(windows$package_type, "win.binary")

  linux <- zak:::current_platform_facts(
    os_type = "unix",
    r_os = "linux-gnu",
    r_arch = "x86_64",
    r_version = "4.4.0",
    package_type = "source"
  )
  expect_identical(linux$os, "linux")
  expect_identical(linux$architecture, "x86_64")
})

test_that("plans record the current platform facts", {
  fixture <- make_fixture_package("zakfixtureplatformplan")
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download_archive = function(url) fixture$archive,
    .package = "zak"
  )

  result <- suppressMessages(
    zak::plan("https://example.test/zakfixtureplatformplan_1.0.0.tar.gz")
  )

  expect_s3_class(result$platform, "zak_platform_facts")
  expect_identical(result$platform, zak:::current_platform_facts())
  expect_identical(
    names(result$platform),
    c("os", "architecture", "r_version", "package_type")
  )
})

test_that("archive compatibility distinguishes binary platform support", {
  source <- list(type = "source", format = "tar.gz")
  binary_zip <- list(type = "binary", format = "zip")
  windows <- zak:::current_platform_facts(
    os_type = "windows",
    r_os = "mingw32",
    r_arch = "x86_64",
    r_version = "4.5.0",
    package_type = "win.binary"
  )
  macos <- zak:::current_platform_facts(
    os_type = "unix",
    r_os = "darwin23",
    r_arch = "arm64",
    r_version = "4.6.1",
    package_type = "both"
  )

  expect_identical(
    zak:::archive_compatibility(source, macos),
    "compatible"
  )
  expect_identical(
    zak:::archive_compatibility(binary_zip, windows),
    "compatible"
  )
  expect_identical(
    zak:::archive_compatibility(binary_zip, macos),
    "incompatible"
  )
})

test_that("binary archives built for another OS or architecture are rejected", {
  linux <- zak:::current_platform_facts(
    os_type = "unix",
    r_os = "linux-gnu",
    r_arch = "x86_64",
    r_version = "4.6.1",
    package_type = "source"
  )
  macos_arm <- zak:::current_platform_facts(
    os_type = "unix",
    r_os = "darwin23",
    r_arch = "aarch64",
    r_version = "4.6.1",
    package_type = "both"
  )
  binary <- function(built) {
    list(
      package = "zakbinarycheck",
      type = "binary",
      format = "tar.gz",
      fields = c(Package = "zakbinarycheck", Built = built)
    )
  }
  macos_build <- "R 4.6.1; aarch64-apple-darwin23; 2026-01-01; unix"
  linux_build <- "R 4.6.1; x86_64-pc-linux-gnu; 2026-01-01; unix"

  # A macOS binary tar.gz used to report "compatible" anywhere, because only
  # the Windows-ZIP case was checked.
  expect_identical(
    zak:::archive_compatibility(binary(macos_build), linux),
    "incompatible"
  )
  expect_identical(
    zak:::archive_compatibility(binary(linux_build), macos_arm),
    "incompatible"
  )
  expect_identical(
    zak:::archive_compatibility(binary(macos_build), macos_arm),
    "compatible"
  )
  expect_identical(
    zak:::archive_compatibility(binary(linux_build), linux),
    "compatible"
  )

  # Same OS, wrong architecture.
  expect_identical(
    zak:::archive_compatibility(
      binary("R 4.6.1; x86_64-apple-darwin20; 2026-01-01; unix"),
      macos_arm
    ),
    "incompatible"
  )
})

test_that("architecture-independent binaries are judged on OS family alone", {
  linux <- zak:::current_platform_facts(
    os_type = "unix",
    r_os = "linux-gnu",
    r_arch = "x86_64",
    r_version = "4.6.1",
    package_type = "source"
  )
  windows <- zak:::current_platform_facts(
    os_type = "windows",
    r_os = "mingw32",
    r_arch = "x86_64",
    r_version = "4.6.1",
    package_type = "win.binary"
  )
  # `NeedsCompilation: no` packages record an empty platform triple.
  metadata <- list(
    package = "zaknoarch",
    type = "binary",
    format = "tar.gz",
    fields = c(Package = "zaknoarch", Built = "R 4.6.1; ; 2026-01-01; unix")
  )

  expect_identical(zak:::archive_compatibility(metadata, linux), "compatible")
  expect_identical(
    zak:::archive_compatibility(metadata, windows),
    "incompatible"
  )
})

test_that("compatibility errors name the archive and both platforms", {
  linux <- zak:::current_platform_facts(
    os_type = "unix",
    r_os = "linux-gnu",
    r_arch = "x86_64",
    r_version = "4.6.1",
    package_type = "source"
  )
  metadata <- list(
    package = "zakbinarymessage",
    type = "binary",
    format = "tar.gz",
    fields = c(
      Package = "zakbinarymessage",
      Built = "R 4.6.1; aarch64-apple-darwin23; 2026-01-01; unix"
    )
  )

  expect_error(
    zak:::validate_archive_compatibility(metadata, linux),
    "zakbinarymessage.*built for macos and this is linux"
  )
})

test_that("source archives and unknown Built fields stay installable", {
  linux <- zak:::current_platform_facts(
    os_type = "unix",
    r_os = "linux-gnu",
    r_arch = "x86_64",
    r_version = "4.6.1",
    package_type = "source"
  )
  source_archive <- list(
    package = "zaksourcearchive",
    type = "source",
    format = "tar.gz",
    fields = c(Package = "zaksourcearchive")
  )
  # An unrecognized platform triple must not block installation.
  unknown <- list(
    package = "zakunknownbuild",
    type = "binary",
    format = "tar.gz",
    fields = c(Package = "zakunknownbuild", Built = "R 4.6.1; wat; x; ")
  )

  expect_identical(
    zak:::archive_compatibility(source_archive, linux),
    "compatible"
  )
  expect_identical(zak:::archive_compatibility(unknown, linux), "compatible")
})
