test_that("lock writes deterministic JSON without temporary paths or secrets", {
  testthat::skip_if_not_installed("jsonlite")
  fixture <- make_fixture_package(
    "zakfixturelock",
    fields = list(SystemRequirements = "libcurl,\n  OpenSSL")
  )
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download_archive = function(url) fixture$archive,
    .package = "zak"
  )
  package_plan <- suppressMessages(
    zak::plan(
      paste0(
        "https://example.test/zakfixturelock_1.0.0.tar.gz",
        "?token=secret-value&build=1"
      ),
      lib = library
    )
  )
  first <- tempfile("zak-lock-first-", fileext = ".json")
  second <- tempfile("zak-lock-second-", fileext = ".json")
  on.exit(unlink(c(first, second)), add = TRUE)

  expect_identical(
    zak::lock(package_plan, first),
    normalizePath(first, mustWork = FALSE)
  )
  zak::lock(package_plan, second)

  expect_identical(readLines(first), readLines(second))
  contents <- paste(readLines(first), collapse = "\n")
  expect_identical(grepl("secret-value", contents, fixed = TRUE), FALSE)
  expect_identical(grepl("zak-download-", contents, fixed = TRUE), FALSE)

  document <- jsonlite::read_json(first, simplifyVector = FALSE)
  expect_identical(document$lockfile$name, "zak")
  expect_identical(document$lockfile$version, 1L)
  expect_identical(document$source$type, "url")
  expect_identical(
    document$source$url,
    paste0(
      "https://example.test/zakfixturelock_1.0.0.tar.gz",
      "?token=<redacted>&build=1"
    )
  )
  expect_identical(document$target$system_requirements, "libcurl, OpenSSL")
  expect_identical(is.null(document$acquisition$path), TRUE)
  expect_identical(
    document$acquisition$sha256,
    package_plan$acquisition$sha256
  )
})

test_that("lock records repository and dependency decisions", {
  testthat::skip_if_not_installed("jsonlite")
  leaf <- make_fixture_package("zakfixturelockleaf")
  target <- make_fixture_package(
    "zakfixturelocktarget",
    fields = list(Imports = "zakfixturelockleaf")
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
    package_plan <- suppressMessages(
      zak::plan(target$package, lib = library)
    )
  })
  file <- tempfile("zak-lock-", fileext = ".json")
  on.exit(unlink(file), add = TRUE)
  zak::lock(package_plan, file)
  document <- jsonlite::read_json(file, simplifyVector = FALSE)

  expect_identical(document$source$type, "repository")
  expect_identical(document$source$package, target$package)
  expect_identical(document$dependencies[[1L]]$Package, leaf$package)
  expect_identical(document$dependencies[[1L]]$Version, leaf$version)
  expect_identical(document$dependencies[[1L]]$Status, "install")
  expect_identical(length(document$repositories), 1L)
})

test_that("lock round trips local archive provenance", {
  testthat::skip_if_not_installed("jsonlite")
  fixture <- make_fixture_package("zakfixturelocklocalarchive")
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  package_plan <- suppressMessages(
    zak::plan(fixture$archive, lib = library)
  )
  file <- tempfile("zak-lock-local-archive-", fileext = ".json")
  on.exit(unlink(file), add = TRUE)
  zak::lock(package_plan, file)

  document <- jsonlite::read_json(file, simplifyVector = FALSE)
  expect_identical(document$source$type, "local_archive")
  expect_identical(document$source$format, "tar.gz")
  expect_identical(document$acquisition$method, "local")
  expect_match(document$acquisition$url, "^file://")
  expect_identical(
    zak::compare_lock(zak::read_lock(file), package_plan)$status,
    "current"
  )
})

test_that("lock round trips Git refs and resolved commits", {
  testthat::skip_if_not_installed("jsonlite")
  fixture <- make_fixture_package("zakfixturelockgit")
  repository <- make_fixture_git_repository(fixture)
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  reference <- paste0("git::", repository$url, "@", repository$commit)
  package_plan <- suppressMessages(zak::plan(reference, lib = library))
  file <- tempfile("zak-lock-git-", fileext = ".json")
  on.exit(unlink(file), add = TRUE)
  zak::lock(package_plan, file)

  document <- jsonlite::read_json(file, simplifyVector = FALSE)
  expect_identical(document$source$type, "git")
  expect_identical(document$source$ref, repository$commit)
  expect_identical(document$acquisition$type, "git")
  expect_identical(document$acquisition$commit, repository$commit)
  expect_identical(document$acquisition$sha256, package_plan$acquisition$sha256)
  expect_identical(
    zak::compare_lock(zak::read_lock(file), package_plan)$status,
    "current"
  )
})

test_that("lock records remote dependency provenance and detects drift", {
  testthat::skip_if_not_installed("jsonlite")
  remote <- make_fixture_package("zakfixturelockremotedep")
  remote_git <- make_fixture_git_repository(remote)
  target <- make_fixture_package(
    "zakfixturelockremotetarget",
    fields = list(
      Imports = remote$package,
      Remotes = paste0("git::", remote_git$url, "@", remote_git$commit)
    )
  )
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(
    unlink(c(remote$root, target$root, library), recursive = TRUE),
    add = TRUE
  )

  package_plan <- suppressMessages(
    zak::plan(target$source, lib = library)
  )
  file <- tempfile("zak-lock-remote-dependency-", fileext = ".json")
  on.exit(unlink(file), add = TRUE)
  zak::lock(package_plan, file)

  document <- jsonlite::read_json(file, simplifyVector = FALSE)
  source <- document$dependency_sources[[remote$package]]
  expect_identical(source$type, "git")
  expect_identical(source$ref, remote_git$commit)
  expect_identical(source$commit, remote_git$commit)
  expect_identical(
    source$sha256,
    package_plan$dependency_sources[[remote$package]]$acquisition$sha256
  )

  package_lock <- zak::read_lock(file)
  expect_identical(
    package_lock$dependency_sources[[remote$package]]$commit,
    remote_git$commit
  )
  expect_identical(
    zak::compare_lock(package_lock, package_plan)$status,
    "current"
  )

  changed_plan <- package_plan
  changed_plan$dependency_sources[[remote$package]]$source$ref <- "other-ref"
  comparison <- zak::compare_lock(package_lock, changed_plan)
  expect_identical(comparison$status, "drifted")
  expect_true("dependency_sources" %in% comparison$changes$Field)
})

test_that("read_lock keeps compatibility with locks without remote sources", {
  testthat::skip_if_not_installed("jsonlite")
  fixture <- make_fixture_package("zakfixturelockcompat")
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  package_plan <- suppressMessages(
    zak::plan(fixture$source, lib = library)
  )
  file <- tempfile("zak-lock-compat-", fileext = ".json")
  compatible_file <- tempfile("zak-lock-compatible-", fileext = ".json")
  on.exit(unlink(c(file, compatible_file)), add = TRUE)
  zak::lock(package_plan, file)
  document <- jsonlite::read_json(file, simplifyVector = FALSE)
  document$dependency_sources <- NULL
  document$acquisition$sha256 <- NULL
  writeLines(
    jsonlite::toJSON(document, auto_unbox = TRUE, pretty = TRUE, na = "null"),
    compatible_file,
    useBytes = TRUE
  )

  package_lock <- zak::read_lock(compatible_file)
  expect_type(package_lock$dependency_sources, "list")
  expect_length(package_lock$dependency_sources, 0L)
})

test_that("blocked plans cannot be locked", {
  target <- make_fixture_package(
    "zakfixturelockblocked",
    fields = list(Imports = "zakfixturelockmissing")
  )
  unrelated <- make_fixture_package("zakfixturelockunrelated")
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

  with_fixture_options(repository$url, {
    expect_warning(
      package_plan <- suppressMessages(
        zak::plan(target$source, lib = library)
      ),
      "not available"
    )
  })
  file <- tempfile("zak-lock-blocked-", fileext = ".json")
  on.exit(unlink(file), add = TRUE)
  expect_snapshot(error = TRUE, zak::lock(package_plan, file))
  expect_identical(file.exists(file), FALSE)
})

test_that("read_lock returns a validated typed lock object", {
  testthat::skip_if_not_installed("jsonlite")
  fixture <- make_fixture_package("zakfixturereadlock")
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download_archive = function(url) fixture$archive,
    .package = "zak"
  )
  package_plan <- suppressMessages(
    zak::plan(
      "https://example.test/zakfixturereadlock_1.0.0.tar.gz",
      lib = library
    )
  )
  file <- tempfile("zak-read-lock-", fileext = ".json")
  on.exit(unlink(file), add = TRUE)
  zak::lock(package_plan, file)

  package_lock <- zak::read_lock(file)
  expect_s3_class(package_lock, "zak_lock")
  expect_identical(package_lock$file, normalizePath(file, mustWork = FALSE))
  expect_s3_class(package_lock$platform, "zak_platform_facts")
  expect_identical(package_lock$target$package, fixture$package)
  expect_identical(nrow(package_lock$dependencies), 0L)
})

test_that("read_lock rejects unsupported schema versions", {
  testthat::skip_if_not_installed("jsonlite")
  file <- tempfile("zak-invalid-lock-", fileext = ".json")
  on.exit(unlink(file), add = TRUE)
  writeLines(
    '{"lockfile":{"name":"zak","version":2}}',
    file,
    useBytes = TRUE
  )

  expect_snapshot(error = TRUE, zak::read_lock(file))
})

test_that("compare_lock reports current plans without volatile drift", {
  testthat::skip_if_not_installed("jsonlite")
  fixture <- make_fixture_package("zakfixturecomparelock")
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download_archive = function(url) fixture$archive,
    .package = "zak"
  )
  package_plan <- suppressMessages(
    zak::plan(
      "https://example.test/zakfixturecomparelock_1.0.0.tar.gz",
      lib = library
    )
  )
  file <- tempfile("zak-compare-lock-", fileext = ".json")
  on.exit(unlink(file), add = TRUE)
  zak::lock(package_plan, file)

  result <- zak::compare_lock(zak::read_lock(file), package_plan)
  expect_identical(result$status, "current")
  expect_identical(nrow(result$changes), 0L)
  expect_output(print(result), "No drift detected")
})

test_that("compare_lock reports changed plan decisions", {
  testthat::skip_if_not_installed("jsonlite")
  first <- make_fixture_package("zakfixturedriftlock", version = "1.0.0")
  second <- make_fixture_package("zakfixturedriftlock", version = "2.0.0")
  first_cache <- tempfile("zak-cache-")
  second_cache <- tempfile("zak-cache-")
  library <- tempfile("zak-library-")
  dir.create(library)
  old <- options(zak.cache = first_cache)
  on.exit(
    {
      options(old)
      unlink(
        c(first$root, second$root, first_cache, second_cache, library),
        recursive = TRUE
      )
    },
    add = TRUE
  )

  testthat::local_mocked_bindings(
    download_archive = function(url) first$archive,
    .package = "zak"
  )
  first_plan <- suppressMessages(
    zak::plan("https://example.test/zakfixturedriftlock.tar.gz", lib = library)
  )
  file <- tempfile("zak-drift-lock-", fileext = ".json")
  on.exit(unlink(file), add = TRUE)
  zak::lock(first_plan, file)

  previous <- options(zak.cache = second_cache)
  on.exit(options(previous), add = TRUE)
  testthat::local_mocked_bindings(
    download_archive = function(url) second$archive,
    .package = "zak"
  )
  second_plan <- suppressMessages(
    zak::plan("https://example.test/zakfixturedriftlock.tar.gz", lib = library)
  )

  result <- zak::compare_lock(file, second_plan)
  expect_identical(result$status, "drifted")
  expect_identical("target" %in% result$changes$Field, TRUE)
  expect_match(
    result$changes$Lock[result$changes$Field == "target"],
    "1[.]0[.]0"
  )
  expect_match(
    result$changes$Plan[result$changes$Field == "target"],
    "2[.]0[.]0"
  )
})

test_that("redaction covers credentials in any URL scheme", {
  # Only `https?://` userinfo used to be handled, so an ssh:// password was
  # written to the lockfile verbatim.
  expect_identical(
    zak:::redact_lock_url("ssh://user:s3cr3t@host/repo.git"),
    "ssh://<redacted>@host/repo.git"
  )
  expect_identical(
    zak:::redact_lock_url("https://user:tok@example.test/p.tar.gz"),
    "https://<redacted>@example.test/p.tar.gz"
  )
  # A token is often the user component on its own.
  expect_identical(
    zak:::redact_lock_url("https://ghp_secret@example.test/p.tar.gz"),
    "https://<redacted>@example.test/p.tar.gz"
  )
  # SCP-style references carry no secret and must keep their user.
  expect_identical(
    zak:::redact_lock_url("git@github.com:owner/repo.git"),
    "git@github.com:owner/repo.git"
  )
})

test_that("redaction covers presigned and SAS credential parameters", {
  expect_identical(
    zak:::redact_lock_url(
      "https://example.test/p.tar.gz?X-Amz-Signature=DEAD&X-Amz-Credential=AKIA"
    ),
    "https://example.test/p.tar.gz?X-Amz-Signature=<redacted>&X-Amz-Credential=<redacted>"
  )
  # Only the signature is secret; the rest of a SAS URL must survive.
  expect_identical(
    zak:::redact_lock_url(
      "https://example.test/p.tar.gz?sig=SECRET&sv=2021-08-06&sp=r"
    ),
    "https://example.test/p.tar.gz?sig=<redacted>&sv=2021-08-06&sp=r"
  )
  expect_identical(
    zak:::redact_lock_url("https://example.test/p.tar.gz#token=abc"),
    "https://example.test/p.tar.gz#token=<redacted>"
  )
  # Ordinary parameters are left alone.
  expect_identical(
    zak:::redact_lock_url("https://example.test/p.tar.gz?arch=x86_64&v=1.2.3"),
    "https://example.test/p.tar.gz?arch=x86_64&v=1.2.3"
  )
})

test_that("redaction never truncates the recorded URL", {
  # Splitting on every "?" used to drop everything after the second one, so the
  # lockfile recorded a URL that was never fetched.
  expect_identical(
    zak:::redact_lock_url("https://example.test/p.tar.gz?a=1?b=2"),
    "https://example.test/p.tar.gz?a=1?b=2"
  )
  # A later "?" belongs to whatever parameter it sits in and must survive
  # alongside redaction of a sensitive one.
  expect_identical(
    zak:::redact_lock_url("https://example.test/p.tar.gz?a=1?b=2&token=T"),
    "https://example.test/p.tar.gz?a=1?b=2&token=<redacted>"
  )
  # A "?" inside a sensitive value is part of that value and is redacted with
  # it, rather than being treated as a separator.
  expect_identical(
    zak:::redact_lock_url("https://example.test/p.tar.gz?token=T?b=2"),
    "https://example.test/p.tar.gz?token=<redacted>"
  )
})

test_that("restore refuses redacted acquisition and dependency URLs", {
  checksum <- strrep("a", 64L)
  base <- list(
    source = list(type = "repository", package = "zakredactcheck"),
    acquisition = NULL,
    dependency_sources = list()
  )

  redacted_acquisition <- base
  redacted_acquisition$acquisition <- list(
    url = "https://example.test/p.tar.gz?token=<redacted>",
    sha256 = checksum
  )
  expect_error(
    zak:::validate_restore_lock(redacted_acquisition),
    "redacted credentials"
  )

  redacted_dependency <- base
  redacted_dependency$dependency_sources <- list(
    zakredactdep = list(
      type = "url",
      url = "https://<redacted>@example.test/dep.tar.gz",
      sha256 = checksum
    )
  )
  expect_error(
    zak:::validate_restore_lock(redacted_dependency),
    "redacted credentials"
  )

  # A lockfile with nothing redacted still validates.
  clean <- base
  clean$acquisition <- list(
    url = "https://example.test/p.tar.gz",
    sha256 = checksum
  )
  expect_silent(zak:::validate_restore_lock(clean))
})
