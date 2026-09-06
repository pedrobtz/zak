#' Install an R package with its dependencies
#'
#' `install()` resolves an R package reference, installs missing required
#' dependencies from configured repositories or their declared `Remotes:`
#' sources, and then installs the requested package. References can be package
#' names, local package directories, local `.tar.gz` or `.zip` archives, or
#' HTTP(S) URLs to `.tar.gz` or `.zip` archives, Git/GitHub references, and
#' Bioconductor package references.
#' Source ZIP archives are extracted before installation. ZIP archives whose
#' `DESCRIPTION` contains a `Built` field are treated as Windows binary
#' packages and can be installed only on Windows.
#'
#' For URL archives, the installed package records `RemoteType: url` and the
#' original URL in `RemoteUrl`, following the metadata convention used by the
#' remotes package. This allows renv to record the package with `Source: URL`
#' and restore it from the same archive. Local archives do not receive remote
#' URL metadata. Git sources also omit remote URL metadata in this iteration.
#' Because the full URL is retained in the installed package metadata and
#' lockfiles, it should not contain embedded credentials or other secrets.
#'
#' Dependency handling follows `utils::install.packages(dependencies = NA)` for
#' repository-only plans: `Depends` (except `R`), `Imports`, and `LinkingTo`
#' are installed, while `Suggests` and `Enhances` are not. A matching
#' `Remotes:` declaration selects and installs the remote source for that
#' dependency; its transitive dependencies are resolved in the same graph.
#' Dependency package types follow R's normal platform configuration. When
#' `lib = NULL`, the `lib` argument is omitted from `install.packages()` so R
#' performs its normal library selection.
#' During metadata inspection, `install()` reports a binary package when the
#' archive's `DESCRIPTION` contains `Built`, and a source package otherwise.
#' A non-empty `SystemRequirements` field is also reported without attempting
#' to check or install those system dependencies.
#' It then lists dependencies needing installation with their repository
#' versions and declared version constraints.
#' Dependencies and the target are installed into a temporary staging library
#' and committed to the requested library only after the complete staged set
#' has been verified. A failed dependency or target install therefore does not
#' commit packages from that operation, and `install()` raises an error naming
#' each package that is missing or built at an unexpected version. Note that
#' `utils::install.packages()` itself only warns on a failed build, so this
#' error is what distinguishes a failed install from a successful one.
#' Set `options(zak.offline = TRUE)` to require valid cached HTTP(S) archives
#' and commit-resolved Git sources during installation.
#' Other package validity and compatibility checks are left to R itself.
#'
#' Apart from resolving the archive's dependencies, installation behavior,
#' conditions, validation, and return behavior are those of
#' `utils::install.packages()`.
#'
#' @param pkg A single package reference. This can be a package name, a local
#'   package directory, a local R package `.tar.gz` or `.zip` archive, or an
#'   HTTP(S) URL to an R package `.tar.gz` or `.zip` archive.
#'   Git references use `git::URL[@ref]`; GitHub references use
#'   `github::owner/repository[@ref]`.
#'   Bioconductor references use `bioc::package`.
#' @param lib `NULL`, or a single character path to the installation library.
#'   The default leaves library selection to `utils::install.packages()`.
#' @param verbose A single logical value. When `TRUE`, report each zak step,
#'   including download, archive inspection, dependency resolution,
#'   installation, and provenance recording. Output from R's package installer
#'   is unchanged.
#'
#' @return The return value of the target `utils::install.packages()` call,
#'   invisibly. That value is always `NULL`; a successful return, rather than
#'   the value, is what signals that the library was committed.
#' @seealso [plan()]
#' @export
#'
#' @examples
#' \dontrun{
#' zak::install("jsonlite")
#' zak::install("https://example.org/examplePackage_1.0.0.tar.gz")
#' zak::install("/path/to/local/package", verbose = TRUE)
#' }
install <- function(pkg, lib = NULL, verbose = FALSE) {
  built <- build_install_plan(pkg, lib, verbose)
  on.exit(cleanup_plan_artifact(built$artifact), add = TRUE)

  plan <- built$plan
  report_dependencies(
    plan$archive_metadata$package,
    plan$install_dependencies,
    plan$available
  )
  install_from_plan(plan, built$artifact, verbose)
}

install_from_plan <- function(plan, artifact, verbose) {
  metadata <- plan$archive_metadata
  dependencies <- plan$install_dependencies
  repositories <- plan$repositories
  lib <- installation_library(plan$library)
  staging <- create_staging_library(lib)
  on.exit(cleanup_staging_library(staging), add = TRUE)

  if (!is.null(artifact)) {
    verify_acquired_artifact(artifact, plan$acquisition$sha256)
  }

  if (length(dependencies)) {
    report_step(
      verbose,
      sprintf(
        "Installing %d required %s.",
        length(dependencies),
        if (length(dependencies) == 1L) "dependency" else "dependencies"
      )
    )
    install_dependency_packages(
      dependencies = dependencies,
      repositories = repositories,
      lib = staging,
      sources = plan$dependency_sources,
      platform = plan$platform,
      verbose = verbose
    )
    report_step(verbose, "Finished dependency installation.")
  } else {
    report_step(verbose, "No dependency installation is required.")
  }

  report_step(
    verbose,
    sprintf(
      "Installing target package %s %s.",
      metadata$package,
      metadata$version
    )
  )
  result <- switch(
    plan$source$type,
    url = install_target_archive(
      artifact$path,
      metadata,
      artifact$url,
      staging,
      verbose
    ),
    local = install_local_package(plan$source$path, staging),
    local_archive = install_target_archive(
      artifact$path,
      metadata,
      artifact$url,
      staging,
      verbose,
      record_url = FALSE
    ),
    git = install_local_package(artifact$path, staging),
    bioconductor = install_repository_package(
      metadata$package,
      plan$repositories,
      staging
    ),
    repository = install_repository_package(
      metadata$package,
      plan$repositories,
      staging
    )
  )
  report_step(
    verbose,
    sprintf(
      "Finished target installation command for %s %s.",
      metadata$package,
      metadata$version
    )
  )
  packages <- unique(c(dependencies, metadata$package))
  problems <- verify_staged_packages(plan, staging)
  if (length(problems)) {
    stop(staged_installation_error(problems), call. = FALSE)
  }
  commit_staged_packages(staging, lib, packages)
  invisible(result)
}

install_local_package <- function(path, lib) {
  arguments <- list(
    pkgs = path,
    repos = NULL,
    type = "source"
  )
  do.call(utils::install.packages, with_library_argument(arguments, lib))
}

install_repository_package <- function(
  package,
  repositories,
  lib,
  dependencies = NA
) {
  arguments <- list(
    pkgs = package,
    repos = repositories,
    dependencies = dependencies
  )
  do.call(utils::install.packages, with_library_argument(arguments, lib))
}

validate_verbose <- function(verbose) {
  valid <- is.logical(verbose) && length(verbose) == 1L && !is.na(verbose)
  if (!valid) {
    stop("`verbose` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  invisible()
}

validate_install_url <- function(url) {
  valid <- is.character(url) &&
    length(url) == 1L &&
    !is.na(url) &&
    nzchar(url) &&
    !grepl("[\r\n]", url) &&
    grepl("^https?://", url, ignore.case = TRUE)
  if (!valid) {
    stop("`url` must be a single HTTP or HTTPS URL.", call. = FALSE)
  }
  invisible()
}

validate_archive_compatibility <- function(
  metadata,
  platform = current_platform_facts()
) {
  reason <- archive_incompatibility_reason(metadata, platform)
  if (!is.null(reason)) {
    stop(
      sprintf(
        "Zak cannot install the %s %s package %s: %s.",
        metadata$format,
        metadata$type,
        sQuote(metadata$package),
        reason
      ),
      call. = FALSE
    )
  }
  invisible()
}

with_library_argument <- function(arguments, lib) {
  if (!is.null(lib)) {
    arguments$lib <- lib
  }
  arguments
}

install_dependency_packages <- function(
  dependencies,
  repositories,
  lib,
  sources = list(),
  platform = current_platform_facts(),
  verbose = FALSE
) {
  if (!length(sources)) {
    arguments <- list(
      pkgs = dependencies,
      repos = repositories,
      dependencies = NA
    )
    return(
      do.call(
        utils::install.packages,
        with_library_argument(arguments, lib)
      )
    )
  }

  result <- NULL
  for (package in dependencies) {
    source <- sources[[package]]
    if (is.null(source)) {
      result <- install_repository_package(
        package,
        repositories,
        lib,
        dependencies = FALSE
      )
    } else {
      result <- install_source_dependency(
        package,
        source,
        lib,
        platform,
        verbose
      )
    }
  }
  result
}

install_source_dependency <- function(
  package,
  source,
  lib,
  platform,
  verbose
) {
  reference <- source$source
  if (
    identical(reference$type, "git") &&
      !is.null(source$acquisition$commit)
  ) {
    reference$ref <- source$acquisition$commit
  }
  prepared <- prepare_package_source(reference, lib, verbose, platform)
  on.exit(cleanup_plan_artifact(prepared$artifact), add = TRUE)
  if (!is.null(prepared$artifact)) {
    verify_acquired_artifact(prepared$artifact, source$acquisition$sha256)
  }
  if (!identical(prepared$metadata$package, package)) {
    stop(
      sprintf(
        "Remote source was declared for '%s' but contains package '%s'.",
        package,
        prepared$metadata$package
      ),
      call. = FALSE
    )
  }

  switch(
    source$source$type,
    url = install_target_archive(
      prepared$artifact$path,
      prepared$metadata,
      prepared$artifact$url,
      lib,
      verbose,
      record_url = TRUE
    ),
    local_archive = install_target_archive(
      prepared$artifact$path,
      prepared$metadata,
      prepared$artifact$url,
      lib,
      verbose,
      record_url = FALSE
    ),
    git = install_local_package(prepared$artifact$path, lib),
    bioconductor = install_repository_package(
      package,
      prepared$repositories,
      lib,
      dependencies = FALSE
    ),
    repository = install_repository_package(
      package,
      prepared$repositories,
      lib,
      dependencies = FALSE
    ),
    stop(
      sprintf("Unsupported dependency source type: %s.", source$source$type),
      call. = FALSE
    )
  )
}
