make_fixture_package <- function(
  package,
  version = "1.0.0",
  fields = list(),
  load_failure = FALSE
) {
  fixture_root <- tempfile("zak-fixture-")
  source_dir <- file.path(fixture_root, package)
  dir.create(file.path(source_dir, "R"), recursive = TRUE)

  description <- c(
    Package = package,
    Version = version,
    Title = paste("Fixture package", package),
    Description = "A small package used by the zak test suite.",
    `Authors@R` = "person('Test', 'Author', email = 'test@example.com', role = c('aut', 'cre'))",
    License = "MIT",
    Encoding = "UTF-8",
    unlist(fields, use.names = TRUE)
  )
  write.dcf(
    as.data.frame(as.list(description)),
    file.path(source_dir, "DESCRIPTION")
  )
  writeLines("export(fixture_value)", file.path(source_dir, "NAMESPACE"))
  writeLines(
    "fixture_value <- function() 1L",
    file.path(source_dir, "R", "fixture.R")
  )
  if (load_failure) {
    writeLines(
      ".onLoad <- function(libname, pkgname) stop('intentional fixture load failure')",
      file.path(source_dir, "R", "zzz.R")
    )
  }

  old <- setwd(fixture_root)
  on.exit(setwd(old), add = TRUE)
  output <- system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "build", shQuote(source_dir)),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop(paste(output, collapse = "\n"))
  }

  archive <- file.path(fixture_root, sprintf("%s_%s.tar.gz", package, version))
  if (!file.exists(archive)) {
    stop("Fixture archive was not built.")
  }

  list(
    root = fixture_root,
    source = source_dir,
    archive = archive,
    package = package,
    version = version
  )
}

make_fixture_zip <- function(fixture) {
  zip_command <- Sys.which(Sys.getenv("R_ZIPCMD", "zip"))
  testthat::skip_if(!nzchar(zip_command), "A zip command is required.")

  archive <- file.path(
    fixture$root,
    sprintf("%s_%s.zip", fixture$package, fixture$version)
  )
  old <- setwd(fixture$root)
  on.exit(setwd(old), add = TRUE)
  suppressWarnings(utils::zip(
    archive,
    files = fixture$package,
    flags = "-rq9X"
  ))
  if (!file.exists(archive)) {
    stop("Fixture ZIP archive was not built.")
  }
  archive
}

make_fixture_git_repository <- function(fixture) {
  git <- Sys.which("git")
  testthat::skip_if(!nzchar(git), "A git executable is required.")

  run <- function(arguments) {
    output <- system2(git, arguments, stdout = TRUE, stderr = TRUE)
    status <- attr(output, "status")
    if (!is.null(status) && status != 0L) {
      stop(paste(output, collapse = "\n"))
    }
    invisible(output)
  }
  run(c("-C", fixture$source, "init", "--quiet"))
  run(c(
    "-C",
    fixture$source,
    "-c",
    "user.name=Zak-Test",
    "-c",
    "user.email=zak@example.test",
    "add",
    "."
  ))
  run(c(
    "-C",
    fixture$source,
    "-c",
    "user.name=Zak-Test",
    "-c",
    "user.email=zak@example.test",
    "commit",
    "--quiet",
    "-m",
    "Initial-fixture-commit"
  ))
  commit <- system2(
    git,
    c("-C", fixture$source, "rev-parse", "HEAD"),
    stdout = TRUE,
    stderr = TRUE
  )
  list(
    path = fixture$source,
    url = zak:::file_url(fixture$source),
    commit = trimws(commit[[1L]])
  )
}

make_fixture_repository <- function(fixtures) {
  repository <- tempfile("zak-fixture-repository-")
  contrib <- file.path(repository, "src", "contrib")
  dir.create(contrib, recursive = TRUE)
  archives <- vapply(fixtures, `[[`, character(1L), "archive")
  copied <- file.copy(archives, contrib)
  if (!all(copied)) {
    stop("Failed to construct fixture repository.")
  }
  tools::write_PACKAGES(contrib, type = "source", addFiles = TRUE)
  list(root = repository, url = zak:::file_url(repository))
}

with_fixture_options <- function(repository, code) {
  old <- options(repos = c(FIXTURE = repository), pkgType = "source")
  on.exit(options(old), add = TRUE)
  force(code)
}

# A `Built:` field describing the machine running the tests, so binary fixtures
# stay installable wherever the suite runs.
current_built_field <- function() {
  sprintf(
    "R %s; %s; %s; %s",
    getRversion(),
    R.version$platform,
    format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
    .Platform$OS.type
  )
}
