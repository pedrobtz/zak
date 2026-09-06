test_that("dependencies discovers DESCRIPTION and source evidence", {
  project <- tempfile("zak-project-")
  dir.create(file.path(project, "R"), recursive = TRUE)
  dir.create(file.path(project, "renv"), recursive = TRUE)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)

  write.dcf(
    data.frame(
      Package = "zakproject",
      Version = "1.0.0",
      Depends = "R (>= 4.1.0), cli",
      Imports = "dplyr (>= 1.0), jsonlite",
      LinkingTo = "Rcpp",
      Suggests = "testthat",
      Remotes = "github::owner/remote.package@main",
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    file.path(project, "DESCRIPTION")
  )
  writeLines(
    c(
      "library(cli)",
      "dplyr::mutate(data.frame())",
      "requireNamespace('jsonlite')",
      "loadNamespace(\"rlang\")"
    ),
    file.path(project, "R", "code.R")
  )
  writeLines(
    "ggplot2::ggplot(data.frame())",
    file.path(project, "analysis.qmd")
  )
  writeLines(
    "renv::restore()",
    file.path(project, "renv", "ignored.R")
  )

  result <- zak::dependencies(project)
  expect_identical(
    result[result$Source == "DESCRIPTION", c("Package", "Type")],
    data.frame(
      Package = c("R", "Rcpp", "cli", "dplyr", "jsonlite", "remote.package"),
      Type = c(
        "Depends",
        "LinkingTo",
        "Depends",
        "Imports",
        "Imports",
        "Remotes"
      ),
      stringsAsFactors = FALSE
    )
  )
  expect_identical(any(result$Package == "testthat"), FALSE)
  expect_identical(any(result$Package == "cli" & result$Source == "R"), TRUE)
  expect_identical(
    any(result$Package == "jsonlite" & result$Type == "namespace"),
    TRUE
  )
  expect_identical(
    any(result$Package == "ggplot2" & result$Source == "qmd"),
    TRUE
  )
  expect_identical(any(result$Package == "renv"), FALSE)
})

test_that("dependencies can include optional DESCRIPTION fields", {
  project <- tempfile("zak-project-optional-")
  dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  write.dcf(
    data.frame(
      Package = "zakprojectoptional",
      Version = "1.0.0",
      Suggests = "testthat",
      Enhances = "knitr",
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    file.path(project, "DESCRIPTION")
  )

  required <- zak::dependencies(project)
  optional <- zak::dependencies(
    project,
    include_suggests = TRUE,
    include_enhances = TRUE
  )
  expect_length(required$Package, 0L)
  expect_setequal(optional$Package, c("testthat", "knitr"))
  expect_snapshot(
    error = TRUE,
    zak::dependencies(project, include_suggests = NA)
  )
})

test_that("init creates a project lockfile without installing", {
  testthat::skip_if_not_installed("jsonlite")
  fixture <- make_fixture_package("zakfixtureproject")
  lockfile <- file.path(fixture$source, "zak.lock")
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  result <- zak::init(fixture$source)

  expect_s3_class(result, "zak_project")
  expect_identical(result$lockfile, normalizePath(lockfile, mustWork = TRUE))
  expect_identical(
    result$project_file,
    normalizePath(
      file.path(fixture$source, ".zak", "project.json"),
      mustWork = TRUE
    )
  )
  expect_identical(
    result$library,
    normalizePath(file.path(fixture$source, ".zak", "library"))
  )
  expect_identical(file.exists(lockfile), TRUE)
  expect_identical(file.exists(result$project_file), TRUE)
  expect_identical(dir.exists(result$library), TRUE)
  expect_identical(zak::read_lock(lockfile)$target$package, fixture$package)
  expect_identical(file.exists(file.path(fixture$source, "Rlibs")), FALSE)
  expect_snapshot(
    error = TRUE,
    tryCatch(
      zak::init(fixture$source),
      error = function(error) stop("Zak project metadata already exists.")
    )
  )
})

test_that("init records metadata for a non-package project", {
  testthat::skip_if_not_installed("jsonlite")
  fixture <- make_fixture_package("zakfixtureprojectdep")
  project <- tempfile("zak-script-project-")
  repository <- make_fixture_repository(list(fixture))
  dir.create(project)
  on.exit(
    unlink(c(project, fixture$root, repository$root), recursive = TRUE),
    add = TRUE
  )
  writeLines(
    "zakfixtureprojectdep::fixture_value()",
    file.path(project, "script.R")
  )

  result <- with_fixture_options(
    repository$url,
    suppressMessages(zak::init(project))
  )
  document <- jsonlite::read_json(
    result$project_file,
    simplifyVector = TRUE
  )
  project_lock <- zak::read_lock(result$lockfile)
  expect_s3_class(project_lock, "zak_project_lock")
  expect_identical(length(project_lock$references), 1L)
  expect_identical(
    project_lock$references[[1L]]$package,
    fixture$package
  )
  expect_identical(
    project_lock$references[[1L]]$reference,
    fixture$package
  )
  expect_identical(project_lock$project$library, ".zak/library")
  expect_identical(project_lock$plans[[1L]]$target$package, fixture$package)
  expect_identical(result$plans[[1L]]$target$package, fixture$package)
  expect_identical(
    result$lockfile,
    normalizePath(file.path(project, "zak.lock"))
  )
  expect_null(result$plan)
  expect_identical(document$project$name, basename(project))
  expect_identical(document$project$schema, 1L)
  expect_identical(document$project$library, ".zak/library")
  expect_setequal(result$dependencies$Package, fixture$package)
  expect_identical(document$dependencies$Package, fixture$package)
  expect_snapshot(
    error = TRUE,
    tryCatch(
      zak::init(project),
      error = function(error) stop("Zak project metadata already exists.")
    )
  )
})

test_that("restore installs a project lock in one staged transaction", {
  testthat::skip_if_not_installed("jsonlite")
  fixtures <- list(
    make_fixture_package("zakfixtureprojectrestorea"),
    make_fixture_package("zakfixtureprojectrestoreb")
  )
  project <- tempfile("zak-script-restore-project-")
  repository <- make_fixture_repository(fixtures)
  dir.create(project)
  on.exit(
    unlink(
      c(
        project,
        vapply(fixtures, `[[`, character(1L), "root"),
        repository$root
      ),
      recursive = TRUE
    ),
    add = TRUE
  )
  writeLines(
    c(
      "zakfixtureprojectrestorea::fixture_value()",
      "zakfixtureprojectrestoreb::fixture_value()"
    ),
    file.path(project, "script.R")
  )

  result <- with_fixture_options(
    repository$url,
    suppressMessages(zak::init(project))
  )
  expect_identical(
    any(vapply(
      fixtures,
      function(fixture) {
        dir.exists(file.path(result$library, fixture$package))
      },
      logical(1L)
    )),
    FALSE
  )

  staging_calls <- 0L
  original_create_staging_library <- zak:::create_staging_library
  testthat::local_mocked_bindings(
    create_staging_library = function(lib) {
      staging_calls <<- staging_calls + 1L
      original_create_staging_library(lib)
    },
    .package = "zak"
  )

  restored <- with_fixture_options(
    repository$url,
    suppressMessages(zak::restore(result$lockfile))
  )

  expect_null(restored)
  expect_identical(staging_calls, 1L)
  expect_identical(
    all(vapply(
      fixtures,
      function(fixture) {
        dir.exists(file.path(result$library, fixture$package))
      },
      logical(1L)
    )),
    TRUE
  )
  installed <- utils::installed.packages(lib.loc = result$library)
  expect_identical(
    unname(installed[
      vapply(fixtures, `[[`, character(1L), "package"),
      "Version"
    ]),
    vapply(fixtures, `[[`, character(1L), "version")
  )
})

test_that("projects can activate and deactivate their isolated library", {
  testthat::skip_if_not_installed("jsonlite")
  project <- tempfile("zak-project-activation-")
  dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  result <- zak::init(project)
  original <- .libPaths()
  on.exit(.libPaths(original), add = TRUE)
  on.exit(zak::deactivate(), add = TRUE)

  activation <- zak::activate(project)

  expect_s3_class(activation, "zak_activation")
  expect_identical(activation$path, result$path)
  expect_identical(activation$library, result$library)
  expect_identical(.libPaths()[[1L]], result$library)
  expect_identical(zak::project_library(project), result$library)
  expect_identical(zak::activate(project), activation)

  zak::deactivate()

  expect_identical(.libPaths(), original)
  expect_null(zak::deactivate())
})

test_that("activation prevents silently switching projects", {
  testthat::skip_if_not_installed("jsonlite")
  first <- tempfile("zak-project-first-")
  second <- tempfile("zak-project-second-")
  dir.create(first)
  dir.create(second)
  on.exit(unlink(c(first, second), recursive = TRUE), add = TRUE)
  zak::init(first)
  zak::init(second)
  original <- .libPaths()
  on.exit(.libPaths(original), add = TRUE)
  on.exit(zak::deactivate(), add = TRUE)

  zak::activate(first)

  expect_snapshot(
    error = TRUE,
    tryCatch(
      zak::activate(second),
      error = function(error) stop("Another zak project is already active.")
    )
  )
})

test_that("project initialization can opt out of an isolated library", {
  testthat::skip_if_not_installed("jsonlite")
  project <- tempfile("zak-project-no-isolation-")
  dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)

  result <- zak::init(project, isolated = FALSE)

  expect_null(result$library)
  expect_snapshot(error = TRUE, zak::project_library(project))
  expect_snapshot(error = TRUE, zak::activate(project))
})

test_that("a project lockfile with a Git dependency source yields a parsed reference", {
  testthat::skip_if_not_installed("jsonlite")

  # `init()` cannot currently emit such a lockfile (a project lock is only
  # written for a project with no DESCRIPTION, and `Remotes:` is not carried by
  # `available.packages()`), but `read_lock()` validates and accepts one from
  # anywhere else. It used to hand `install_source_dependency()` reference text
  # where a parsed reference was required, failing with
  # "$ operator is invalid for atomic vectors".
  fixture <- make_fixture_package("zakprojlockgit")
  repository <- make_fixture_repository(list(fixture))
  project <- tempfile("zak-project-locksource-")
  dir.create(project)
  on.exit(
    unlink(c(project, fixture$root, repository$root), recursive = TRUE),
    add = TRUE
  )
  writeLines("zakprojlockgit::fixture_value()", file.path(project, "use.R"))

  result <- with_fixture_options(
    repository$url,
    suppressMessages(zak::init(project, isolated = TRUE))
  )

  document <- jsonlite::read_json(result$lockfile, simplifyVector = FALSE)
  document$plans[[1L]]$dependency_sources <- list(
    zakprojlockgitdep = list(
      type = "git",
      url = "https://example.test/dep.git",
      ref = "main",
      commit = strrep("0", 40L),
      sha256 = strrep("a", 64L)
    )
  )
  jsonlite::write_json(
    document,
    result$lockfile,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )

  project_lock <- zak::read_lock(result$lockfile)
  entries <- zak:::project_lock_install_entries(project_lock)
  source <- entries$sources$zakprojlockgitdep$source

  expect_type(source, "list")
  expect_identical(source$type, "git")
  expect_identical(source$url, "https://example.test/dep.git")
  expect_identical(source$ref, strrep("0", 40L))
})

test_that("status() reports missing, ok, and extra packages", {
  testthat::skip_if_not_installed("jsonlite")
  fixture <- make_fixture_package("zakstatusfixture")
  extra <- make_fixture_package("zakstatusextra")
  repository <- make_fixture_repository(list(fixture, extra))
  project <- tempfile("zak-status-project-")
  dir.create(project)
  on.exit(
    unlink(
      c(project, fixture$root, extra$root, repository$root),
      recursive = TRUE
    ),
    add = TRUE
  )
  writeLines("zakstatusfixture::fixture_value()", file.path(project, "use.R"))

  result <- with_fixture_options(
    repository$url,
    suppressMessages(zak::init(project))
  )

  before <- zak::status(project)
  expect_s3_class(before, "zak_status")
  expect_identical(before$packages$Package, "zakstatusfixture")
  expect_identical(before$packages$Status, "missing")
  expect_identical(before$packages$Expected, "1.0.0")

  with_fixture_options(
    repository$url,
    suppressMessages(zak::restore(result$lockfile))
  )
  with_fixture_options(
    repository$url,
    suppressMessages(zak::install("zakstatusextra", lib = result$library))
  )

  after <- zak::status(project)
  rows <- after$packages
  expect_identical(rows$Status[rows$Package == "zakstatusfixture"], "ok")
  expect_identical(rows$Status[rows$Package == "zakstatusextra"], "extra")

  without_extra <- zak::status(project, include_extra = FALSE)
  expect_false("zakstatusextra" %in% without_extra$packages$Package)
})

test_that("status() reports a locked package installed at another version", {
  testthat::skip_if_not_installed("jsonlite")
  fixture <- make_fixture_package("zakstatuschanged")
  repository <- make_fixture_repository(list(fixture))
  project <- tempfile("zak-status-changed-")
  dir.create(project)
  on.exit(
    unlink(c(project, fixture$root, repository$root), recursive = TRUE),
    add = TRUE
  )
  writeLines("zakstatuschanged::fixture_value()", file.path(project, "use.R"))

  result <- with_fixture_options(
    repository$url,
    suppressMessages(zak::init(project))
  )
  with_fixture_options(
    repository$url,
    suppressMessages(zak::restore(result$lockfile))
  )

  description <- file.path(result$library, "zakstatuschanged", "DESCRIPTION")
  fields <- read.dcf(description)
  fields[1L, "Version"] <- "9.9.9"
  write.dcf(fields, description)

  rows <- zak::status(project)$packages
  expect_identical(rows$Status[rows$Package == "zakstatuschanged"], "changed")
  expect_identical(rows$Installed[rows$Package == "zakstatuschanged"], "9.9.9")
})

test_that("project_installed_versions() sees a package with no Meta/package.rds", {
  library <- tempfile("zak-status-library-")
  dir.create(file.path(library, "zakstatusbinary"), recursive = TRUE)
  on.exit(unlink(library, recursive = TRUE), add = TRUE)
  writeLines(
    c("Package: zakstatusbinary", "Version: 2.1.0"),
    file.path(library, "zakstatusbinary", "DESCRIPTION")
  )

  expect_identical(
    zak:::project_installed_versions(library),
    c(zakstatusbinary = "2.1.0")
  )
  expect_identical(
    zak:::project_installed_versions(tempfile("zak-absent-")),
    stats::setNames(character(), character())
  )
})

test_that("snapshot() rewrites the project lockfile without installing", {
  testthat::skip_if_not_installed("jsonlite")
  fixture <- make_fixture_package("zaksnapshotfixture")
  repository <- make_fixture_repository(list(fixture))
  project <- tempfile("zak-snapshot-project-")
  dir.create(project)
  on.exit(
    unlink(c(project, fixture$root, repository$root), recursive = TRUE),
    add = TRUE
  )
  writeLines("zaksnapshotfixture::fixture_value()", file.path(project, "a.R"))

  result <- with_fixture_options(
    repository$url,
    suppressMessages(zak::init(project))
  )
  unlink(result$lockfile)

  snapshotted <- with_fixture_options(
    repository$url,
    suppressMessages(zak::snapshot(project))
  )

  expect_true(file.exists(snapshotted$lockfile))
  expect_identical(
    zak::read_lock(snapshotted$lockfile)$references[[1L]]$package,
    "zaksnapshotfixture"
  )
  expect_identical(list.files(result$library), character())
})
