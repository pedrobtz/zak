test_that("Git and GitHub references parse into one Git source", {
  expect_identical(
    zak:::parse_package_reference(
      "git::https://example.test/repository.git@feature/test"
    ),
    list(
      type = "git",
      url = "https://example.test/repository.git",
      ref = "feature/test"
    )
  )
  expect_identical(
    zak:::parse_package_reference("github::owner/repository@main"),
    list(
      type = "git",
      url = "https://github.com/owner/repository.git",
      ref = "main"
    )
  )
  expect_identical(
    zak:::parse_package_reference("git::git@example.test:owner/repository.git"),
    list(
      type = "git",
      url = "git@example.test:owner/repository.git",
      ref = "HEAD"
    )
  )
})

test_that("Git plans record the checked-out commit without retaining checkout paths", {
  fixture <- make_fixture_package("zakfixturegitplan")
  repository <- make_fixture_git_repository(fixture)
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  reference <- paste0("git::", repository$url, "@", repository$commit)
  result <- suppressMessages(zak::plan(reference, lib = library))

  expect_identical(result$source$type, "git")
  expect_identical(result$source$url, repository$url)
  expect_identical(result$source$ref, repository$commit)
  expect_identical(result$target$package, fixture$package)
  expect_identical(result$target$format, "directory")
  expect_identical(result$acquisition$type, "git")
  expect_identical(result$acquisition$ref, repository$commit)
  expect_identical(result$acquisition$commit, repository$commit)
  expect_identical(result$acquisition$format, "directory")
  expect_identical("path" %in% names(result$acquisition), FALSE)
  expect_identical(result$candidates$target$source$type, "git")
  expect_identical(result$candidates$target$source$ref, repository$commit)
  expect_identical(
    result$candidates$target$provenance$commit,
    repository$commit
  )
  expect_identical(dir.exists(file.path(library, fixture$package)), FALSE)
})

test_that("Git package references can be installed transactionally", {
  fixture <- make_fixture_package("zakfixturegitinstall")
  repository <- make_fixture_git_repository(fixture)
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(c(fixture$root, library), recursive = TRUE), add = TRUE)

  reference <- paste0("git::", repository$url, "@", repository$commit)
  expect_null(suppressMessages(zak::install(reference, lib = library)))

  installed <- utils::installed.packages(lib.loc = library)
  expect_true(fixture$package %in% installed[, "Package"])
})

test_that("Git references reject invalid syntax before acquisition", {
  expect_error(
    zak:::parse_package_reference("git::not-a-url@main"),
    "Git references must use"
  )
  expect_error(
    zak:::parse_package_reference("github::owner-only"),
    "owner/repository"
  )
  expect_error(
    zak:::parse_package_reference("github::owner/repository@"),
    "non-empty ref"
  )
})

test_that("run_git quotes arguments so shell metacharacters are not executed", {
  testthat::skip_if(!nzchar(Sys.which("git")), "A git executable is required.")
  marker <- tempfile("zak-injection-", fileext = ".txt")
  on.exit(unlink(marker, force = TRUE), add = TRUE)

  # `rev-parse` fails on the payload, which is the point: it must be passed to
  # git as one opaque argument rather than interpreted by the shell.
  suppressWarnings(try(
    zak:::run_git(
      c("rev-parse", paste0("HEAD;touch$IFS", marker)),
      "run an injected command"
    ),
    silent = TRUE
  ))

  expect_false(file.exists(marker))
})

test_that("git references reject refs and URLs that git would read as options", {
  expect_error(
    zak:::parse_git_reference("https://example.test/repo.git@--upload-pack=evil"),
    "must not begin with"
  )
  expect_error(
    zak:::validate_git_url("--upload-pack=evil"),
    "HTTP\\(S\\), SSH, Git, file, or SCP-style URL"
  )
})

test_that("a Git ref containing shell metacharacters does not execute", {
  testthat::skip_if(!nzchar(Sys.which("git")), "A git executable is required.")
  fixture <- make_fixture_package("zakfixtureinjection")
  repository <- make_fixture_git_repository(fixture)
  marker <- tempfile("zak-injection-clone-", fileext = ".txt")
  on.exit(unlink(marker, force = TRUE), add = TRUE)

  reference <- zak:::parse_package_reference(
    sprintf("git::%s@%s", repository$url, paste0("HEAD;touch$IFS", marker))
  )
  suppressWarnings(try(
    zak:::acquire_git_repository(reference$url, reference$ref),
    silent = TRUE
  ))

  expect_false(file.exists(marker))
})

test_that("git failures report git's own diagnostics", {
  testthat::skip_if(!nzchar(Sys.which("git")), "A git executable is required.")
  repository <- tempfile("zak-git-diagnostics-")
  dir.create(repository)
  on.exit(unlink(repository, recursive = TRUE), add = TRUE)
  zak:::run_git(c("-C", repository, "init", "--quiet"), "initialize")

  # The failure used to be reported as a bare "Git could not read a ref.",
  # discarding everything git said about why.
  expect_error(
    zak:::run_git(c("-C", repository, "rev-parse", "zaknosuchref"), "read a ref"),
    "unknown revision or path not in the working tree"
  )
  expect_error(
    zak:::run_git(c("-C", repository, "rev-parse", "zaknosuchref"), "read a ref"),
    "Git could not read a ref"
  )
})

test_that("git diagnostics redact credentials and truncate long output", {
  expect_match(
    zak:::git_failure_details(
      "fatal: Authentication failed for 'https://ci:ghp_secret@example.test/x.git/'"
    ),
    "https://<redacted>@example.test",
    fixed = TRUE
  )
  expect_false(
    grepl("ghp_secret", zak:::git_failure_details(
      "fatal: could not read Password for 'https://ci:ghp_secret@example.test'"
    ), fixed = TRUE)
  )

  many <- sprintf("line %d", seq_len(30))
  details <- zak:::git_failure_details(many, limit = 10L)
  expect_match(details, "last lines of git output", fixed = TRUE)
  expect_match(details, "line 30", fixed = TRUE)
  expect_false(grepl("line 1\n", details, fixed = TRUE))

  expect_identical(zak:::git_failure_details(character()), "")
  expect_identical(zak:::git_failure_details(c("", "   ")), "")
})

test_that("command output is not contaminated by git's stderr", {
  testthat::skip_if(!nzchar(Sys.which("git")), "A git executable is required.")
  repository <- tempfile("zak-git-streams-")
  dir.create(repository)
  on.exit(unlink(repository, recursive = TRUE), add = TRUE)

  zak:::run_git(c("-C", repository, "init", "--quiet"), "initialize")
  writeLines("x", file.path(repository, "file.txt"))
  zak:::run_git(c("-C", repository, "add", "-A"), "stage")
  zak:::run_git(
    c(
      "-C",
      repository,
      "-c",
      "user.email=zak@example.test",
      "-c",
      "user.name=Zak-Test",
      "commit",
      "--quiet",
      "-m",
      "one"
    ),
    "commit"
  )
  commit <- zak:::run_git(
    c("-C", repository, "rev-parse", "HEAD"),
    "read the commit",
    require_output = TRUE
  )

  # `git checkout` writes detached-HEAD advice to stderr and nothing to stdout.
  # With the streams merged, that advice became the command's return value, and
  # any command read for its output was open to the same contamination.
  advice <- zak:::run_git(
    c("-C", repository, "-c", "advice.detachedHead=true", "checkout", commit),
    "check out the commit"
  )
  expect_identical(advice, "")
  expect_false(grepl("detached", advice, fixed = TRUE))

  reread <- zak:::run_git(
    c("-C", repository, "rev-parse", "HEAD"),
    "read the commit",
    require_output = TRUE
  )
  expect_match(reread, "^[0-9a-f]{40}$")
  expect_identical(reread, commit)
})
