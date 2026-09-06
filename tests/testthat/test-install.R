test_that("zak exports its public functions", {
  expect_setequal(
    getNamespaceExports("zak"),
    c(
      "available_packages",
      "activate",
      "cache_clean",
      "cache_info",
      "compare_lock",
      "deactivate",
      "dependencies",
      "init",
      "install",
      "installed_packages",
      "lock",
      "plan",
      "project_library",
      "read_lock",
      "restore",
      "snapshot",
      "status"
    )
  )
  expect_null(formals(zak::install)$lib)
  expect_identical(formals(zak::install)$verbose, FALSE)
})

test_that("install requires one HTTP or HTTPS URL", {
  expect_silent(zak:::validate_install_url("https://example.test/package.zip"))
  expect_silent(zak:::validate_install_url(
    "HTTP://example.test/package.tar.gz"
  ))
  expect_error(zak:::validate_install_url(character()), "single HTTP or HTTPS")
  expect_error(zak:::validate_install_url(c("https://a", "https://b")))
  expect_error(zak:::validate_install_url("file:///tmp/package.tar.gz"))
  expect_error(zak:::validate_install_url(
    "https://example.test/pkg.zip\nField: value"
  ))
})

test_that("install requires one logical verbose value", {
  expect_silent(zak:::validate_verbose(TRUE))
  expect_silent(zak:::validate_verbose(FALSE))
  expect_error(zak:::validate_verbose(NULL), "`TRUE` or `FALSE`")
  expect_error(zak:::validate_verbose(NA), "`TRUE` or `FALSE`")
  expect_error(zak:::validate_verbose(c(TRUE, FALSE)), "`TRUE` or `FALSE`")
  expect_error(zak:::validate_verbose("yes"), "`TRUE` or `FALSE`")
})

test_that("verbose step reporting has a stable prefix", {
  expect_message(
    zak:::report_step(TRUE, "Inspecting package archive."),
    "^\\[zak\\] Inspecting package archive[.]"
  )
  expect_silent(zak:::report_step(FALSE, "Inspecting package archive."))
})

test_that("NULL lib is omitted and an explicit library is forwarded", {
  arguments <- list(pkgs = "example")
  expect_false("lib" %in% names(zak:::with_library_argument(arguments, NULL)))
  expect_identical(
    zak:::with_library_argument(arguments, "/tmp/example")$lib,
    "/tmp/example"
  )
})

test_that("a package without dependencies installs end to end", {
  fixture <- make_fixture_package("zakfixturenodeps")
  cache <- tempfile("zak-cache-")
  library <- tempfile("zak-library-")
  dir.create(library)
  old <- options(zak.cache = cache)
  on.exit(
    {
      options(old)
      unlink(c(fixture$root, cache, library), recursive = TRUE)
    },
    add = TRUE
  )

  downloads <- 0L
  testthat::local_mocked_bindings(
    download_archive = function(url) {
      downloads <<- downloads + 1L
      fixture$archive
    },
    .package = "zak"
  )

  result <- zak::install(
    "https://example.test/zakfixturenodeps_1.0.0.tar.gz",
    lib = library
  )
  expect_null(result)
  expect_identical(downloads, 1L)
  installed <- utils::installed.packages(lib.loc = library)
  expect_true(fixture$package %in% installed[, "Package"])
  description <- read.dcf(file.path(library, fixture$package, "DESCRIPTION"))
  expect_identical(unname(description[1L, "RemoteType"]), "url")
  expect_identical(
    unname(description[1L, "RemoteUrl"]),
    "https://example.test/zakfixturenodeps_1.0.0.tar.gz"
  )
  package_description <- utils::packageDescription(
    fixture$package,
    lib.loc = library
  )
  expect_identical(package_description$RemoteType, "url")
  expect_identical(
    package_description$RemoteUrl,
    "https://example.test/zakfixturenodeps_1.0.0.tar.gz"
  )
})

test_that("install validates an acquired artifact against the plan hash", {
  fixture <- make_fixture_package("zakfixtureinstallhash")
  cache <- tempfile("zak-cache-")
  library <- tempfile("zak-library-")
  dir.create(library)
  old <- options(zak.cache = cache)
  on.exit(
    {
      options(old)
      unlink(c(fixture$root, cache, library), recursive = TRUE)
    },
    add = TRUE
  )
  testthat::local_mocked_bindings(
    download_archive = function(url) {
      archive <- tempfile(fileext = ".tar.gz")
      file.copy(fixture$archive, archive)
      archive
    },
    .package = "zak"
  )

  built <- zak:::build_install_plan(
    "https://example.test/zakfixtureinstallhash_1.0.0.tar.gz",
    lib = library,
    verbose = FALSE
  )
  on.exit(zak:::cleanup_plan_artifact(built$artifact), add = TRUE)
  connection <- file(built$artifact$path, open = "ab")
  writeBin(as.raw(0L), connection)
  close(connection)

  expect_error(
    zak:::install_from_plan(built$plan, built$artifact, verbose = FALSE),
    "failed SHA-256 verification"
  )
})

test_that("verbose install reports each major step", {
  fixture <- make_fixture_package("zakfixtureverbose")
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download_archive = function(url) fixture$archive,
    .package = "zak"
  )

  messages <- capture.output(
    result <- zak::install(
      "https://example.test/zakfixtureverbose_1.0.0.tar.gz",
      lib = library,
      verbose = TRUE
    ),
    type = "message"
  )

  expect_null(result)
  steps <- messages[startsWith(messages, "[zak]")]
  expect_identical(
    steps,
    c(
      "[zak] Validating package reference.",
      "[zak] Downloading package archive.",
      "[zak] Inspecting package archive.",
      "[zak] Package: 'zakfixtureverbose' 1.0.0 (source)",
      "[zak] Checking archive compatibility.",
      "[zak] Resolving required package dependencies.",
      "[zak] No dependency installation is required.",
      "[zak] Installing target package zakfixtureverbose 1.0.0.",
      "[zak] Extracting source package archive.",
      "[zak] Recording URL provenance in source metadata.",
      paste(
        "[zak] Finished target installation command for",
        "zakfixtureverbose 1.0.0."
      )
    )
  )
})

test_that("a source ZIP package installs end to end", {
  fixture <- make_fixture_package("zakfixturezip")
  archive <- make_fixture_zip(fixture)
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download_archive = function(url) archive,
    .package = "zak"
  )

  result <- zak::install(
    "https://example.test/zakfixturezip_1.0.0.zip",
    lib = library
  )
  expect_null(result)
  installed <- utils::installed.packages(lib.loc = library)
  expect_true(fixture$package %in% installed[, "Package"])
  description <- read.dcf(file.path(library, fixture$package, "DESCRIPTION"))
  expect_identical(unname(description[1L, "RemoteType"]), "url")
  expect_identical(
    unname(description[1L, "RemoteUrl"]),
    "https://example.test/zakfixturezip_1.0.0.zip"
  )
})

test_that("direct and transitive dependencies install before the target", {
  leaf <- make_fixture_package("zakfixtureleaf")
  direct <- make_fixture_package(
    "zakfixturedirect",
    fields = list(Imports = "zakfixtureleaf")
  )
  target <- make_fixture_package(
    "zakfixturetarget",
    fields = list(Imports = "zakfixturedirect (>= 1.0.0)")
  )
  target_archive <- make_fixture_zip(target)
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
    download_archive = function(url) target_archive,
    .package = "zak"
  )

  with_fixture_options(repository$url, {
    messages <- capture.output(
      result <- zak::install(
        "https://example.test/zakfixturetarget_1.0.0.zip",
        lib = library,
        verbose = TRUE
      ),
      type = "message"
    )
  })

  expect_null(result)
  expect_true(all(
    c(
      "[zak] Reading configured package repositories.",
      "[zak] Installing 2 required dependencies.",
      "[zak] Finished dependency installation."
    ) %in%
      messages
  ))
  installed <- utils::installed.packages(lib.loc = library)[, "Package"]
  expect_true(all(
    c(leaf$package, direct$package, target$package) %in% installed
  ))
})

test_that("binary ZIP packages are rejected outside Windows", {
  testthat::skip_on_os("windows")
  metadata <- list(format = "zip", type = "binary")

  expect_error(
    zak:::install_target_archive(
      "example.zip",
      metadata,
      "https://example.test/example.zip",
      NULL
    ),
    "only be installed on Windows"
  )
})

test_that("binary ZIP packages use the Windows binary installer", {
  expect_identical(
    zak:::binary_zip_install_type("windows"),
    "win.binary"
  )
  expect_error(
    zak:::binary_zip_install_type("unix"),
    "only be installed on Windows"
  )
})

test_that("binary ZIP compatibility is checked before dependency installation", {
  metadata <- list(package = "zakzipcheck", format = "zip", type = "binary")
  windows <- zak:::current_platform_facts(
    os_type = "windows",
    r_os = "mingw32",
    r_arch = "x86_64",
    r_version = "4.6.1",
    package_type = "win.binary"
  )
  linux <- zak:::current_platform_facts(
    os_type = "unix",
    r_os = "linux-gnu",
    r_arch = "x86_64",
    r_version = "4.6.1",
    package_type = "source"
  )

  expect_silent(zak:::validate_archive_compatibility(metadata, windows))
  expect_error(
    zak:::validate_archive_compatibility(metadata, linux),
    "Windows binary ZIP archive"
  )
})

test_that("an unavailable dependency prevents target installation", {
  target <- make_fixture_package(
    "zakfixtureunavailable",
    fields = list(Imports = "zakfixturedoesnotexist")
  )
  unrelated <- make_fixture_package("zakfixtureunrelated")
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
    expect_error(
      suppressWarnings(
        zak::install(
          "https://example.test/zakfixtureunavailable_1.0.0.tar.gz",
          lib = library
        )
      ),
      "'zakfixtureunavailable' was not installed"
    )
  })
  expect_false(dir.exists(file.path(library, target$package)))
})

test_that("a failed dependency build prevents target installation", {
  broken <- make_fixture_package("zakfixturebroken", load_failure = TRUE)
  target <- make_fixture_package(
    "zakfixtureblockedtarget",
    fields = list(Imports = "zakfixturebroken")
  )
  repository <- make_fixture_repository(list(broken))
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(
      c(broken$root, target$root, repository$root, library),
      recursive = TRUE
    ),
    add = TRUE
  )

  testthat::local_mocked_bindings(
    download_archive = function(url) target$archive,
    .package = "zak"
  )

  with_fixture_options(repository$url, {
    expect_error(
      suppressWarnings(
        zak::install(
          "https://example.test/zakfixtureblockedtarget_1.0.0.tar.gz",
          lib = library
        )
      ),
      "staged library was incomplete"
    )
  })
  expect_false(dir.exists(file.path(library, target$package)))
})

test_that("a failed target build retains install.packages warning behavior", {
  leaf <- make_fixture_package("zakfixturebadtargetleaf")
  target <- make_fixture_package(
    "zakfixturebadtarget",
    fields = list(Imports = "zakfixturebadtargetleaf"),
    load_failure = TRUE
  )
  repository <- make_fixture_repository(list(leaf))
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(
      c(leaf$root, target$root, repository$root, library),
      recursive = TRUE
    ),
    add = TRUE
  )

  testthat::local_mocked_bindings(
    download_archive = function(url) target$archive,
    .package = "zak"
  )

  with_fixture_options(repository$url, {
    expect_error(
      suppressWarnings(
        zak::install(
          "https://example.test/zakfixturebadtarget_1.0.0.tar.gz",
          lib = library
        )
      ),
      "staged library was incomplete"
    )
  })
  installed <- utils::installed.packages(lib.loc = library)[, "Package"]
  expect_identical(leaf$package %in% installed, FALSE)
  expect_false(dir.exists(file.path(library, target$package)))
})

test_that("optional dependencies do not require a configured repository", {
  target <- make_fixture_package(
    "zakfixtureoptional",
    fields = list(
      Suggests = "zakfixturesuggested",
      Enhances = "zakfixtureenhanced"
    )
  )
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(target$root, library), recursive = TRUE), add = TRUE)

  old <- options(repos = character())
  on.exit(options(old), add = TRUE)
  testthat::local_mocked_bindings(
    download_archive = function(url) target$archive,
    .package = "zak"
  )

  expect_null(
    zak::install(
      "https://example.test/zakfixtureoptional_1.0.0.tar.gz",
      lib = library
    )
  )
})

test_that("install() errors instead of silently committing nothing", {
  broken <- make_fixture_package("zakfixturesilentfail", load_failure = TRUE)
  repository <- make_fixture_repository(list(broken))
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(c(broken$root, repository$root, library), recursive = TRUE),
    add = TRUE
  )

  with_fixture_options(repository$url, {
    expect_error(
      suppressWarnings(zak::install("zakfixturesilentfail", lib = library)),
      "'zakfixturesilentfail' was not installed"
    )
  })
  expect_identical(list.files(library), character())
})

test_that("staged verification sees a binary package with no Meta/package.rds", {
  # `utils::installed.packages()` reports nothing for such a package, so an
  # install that genuinely succeeded used to be discarded silently.
  fixture <- make_fixture_package(
    "zakfixturebinarystaged",
    fields = list(Built = current_built_field())
  )
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  suppressMessages(suppressWarnings(
    zak::install(fixture$archive, lib = library)
  ))

  staged <- file.path(library, fixture$package)
  expect_true(dir.exists(staged))
  expect_false(file.exists(file.path(staged, "Meta", "package.rds")))
  expect_identical(
    zak:::staged_package_version(library, fixture$package),
    "1.0.0"
  )
})
