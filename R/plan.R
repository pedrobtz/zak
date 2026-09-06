#' Create an installation plan for an R package
#'
#' `plan()` resolves an R package reference and returns the selected
#' installation plan. It does not install packages or mutate the target
#' library. The current references are package names from configured
#' repositories, local package directories, and HTTP(S) URLs to `.tar.gz` or
#' `.zip` archives, GitHub/Git references, and Bioconductor package
#' references.
#'
#' Local `.tar.gz` and `.zip` archives are inspected in place and are never
#' removed by planning.
#'
#' Git sources are cloned to a temporary checkout. Plans retain the requested
#' ref and resolved commit, but not the checkout path.
#'
#' Plans record the selected target, dependency order, versions, repositories,
#' constraints, availability, source, acquisition metadata, and the current
#' platform facts. A dependency listed in `Remotes:` is prepared from its
#' declared Git, URL, or Bioconductor source before resolution, so the plan
#' retains the source-specific candidate and provenance. URL archives are
#' downloaded to a temporary location so their metadata can be resolved. The
#' temporary path is not retained in the returned plan and is removed when
#' planning completes. Remote HTTP(S) archives and commit-resolved Git sources
#' reuse the cache configured by `options(zak.cache)` when available. Acquired
#' archives and Git trees record SHA-256 checksums; cache entries and target
#' installation are verified against those checksums.
#' Set `options(zak.offline = TRUE)` to require valid cached HTTP(S) archives
#' and commit-resolved Git sources during planning.
#'
#' `Remotes:` source identities must be inferable. Conflicting direct or
#' transitive declarations are rejected with their package, sources, and
#' declaring packages; failed remote preparation reports the dependency and
#' source context.
#'
#' @param pkg A single package reference. This can be a package name, a local
#'   package directory, a local R package `.tar.gz` or `.zip` archive, or an
#'   HTTP(S) URL to an R package `.tar.gz` or `.zip` archive. Git references
#'   use `git::URL[@ref]`; GitHub references use
#'   `github::owner/repository[@ref]`; Bioconductor references use
#'   `bioc::package`.
#' @param lib `NULL`, or a single character path to the installation library.
#'   Installed packages in this library are considered when resolving
#'   dependencies.
#' @param verbose A single logical value. When `TRUE`, report each planning
#'   step.
#'
#' @return An object of class `zak_plan` containing the source, acquisition,
#'   platform, normalized target and dependency candidates, target and
#'   dependency selections, constraints, repository metadata, and plan status.
#'   When package metadata contains `Remotes:`, normalized source mappings are
#'   available in `$remotes`.
#' @seealso [install()]
#' @export
#'
#' @examples
#' \dontrun{
#' package_plan <- zak::plan("jsonlite")
#' package_plan$dependencies
#' }
plan <- function(pkg, lib = NULL, verbose = FALSE) {
  built <- build_install_plan(pkg, lib, verbose)
  on.exit(cleanup_plan_artifact(built$artifact), add = TRUE)

  report_dependencies(
    built$plan$archive_metadata$package,
    built$plan$install_dependencies,
    built$plan$available
  )
  built$plan
}

build_install_plan <- function(pkg, lib, verbose) {
  validate_verbose(verbose)
  report_step(verbose, "Validating package reference.")
  reference <- parse_package_reference(pkg)
  platform <- current_platform_facts()
  prepared <- prepare_package_source(reference, lib, verbose, platform)
  cleanup <- TRUE
  on.exit(
    {
      if (cleanup) {
        cleanup_plan_artifact(prepared$artifact)
      }
    },
    add = TRUE
  )

  result <- build_resolved_install_plan(
    source = prepared$source,
    metadata = prepared$metadata,
    target = prepared$target,
    lib = lib,
    repositories = prepared$repositories,
    configured = prepared$configured,
    acquisition = prepared$acquisition,
    platform = platform,
    verbose = verbose
  )
  result$artifact <- prepared$artifact
  cleanup <- FALSE
  result
}

parse_package_reference <- function(pkg) {
  if (!is.character(pkg) || length(pkg) != 1L || is.na(pkg) || !nzchar(pkg)) {
    stop(
      "`pkg` must be a single package name, local directory, local archive, URL, or Git reference.",
      call. = FALSE
    )
  }
  if (grepl("[\r\n]", pkg)) {
    stop("`pkg` must not contain line breaks.", call. = FALSE)
  }

  if (startsWith(pkg, "git::")) {
    return(parse_git_reference(sub("^git::", "", pkg)))
  }
  if (startsWith(pkg, "github::")) {
    return(parse_github_reference(sub("^github::", "", pkg)))
  }
  if (startsWith(pkg, "bioc::")) {
    return(parse_bioconductor_reference(sub("^bioc::", "", pkg)))
  }

  if (grepl("^https?://", pkg, ignore.case = TRUE)) {
    validate_install_url(pkg)
    return(list(type = "url", url = pkg))
  }

  path <- path.expand(pkg)
  if (file.exists(path) && !dir.exists(path)) {
    format <- local_archive_format(path)
    if (is.null(format)) {
      stop(
        "Local files must be `.tar.gz` or `.zip` package archives.",
        call. = FALSE
      )
    }
    return(list(
      type = "local_archive",
      path = normalizePath(path, mustWork = TRUE),
      format = format
    ))
  }

  is_path <- startsWith(pkg, ".") ||
    startsWith(pkg, "/") ||
    startsWith(pkg, "~") ||
    dir.exists(pkg)
  if (is_path) {
    if (!dir.exists(path)) {
      stop(
        "Local package references must point to a directory or archive.",
        call. = FALSE
      )
    }
    path <- normalizePath(path, mustWork = TRUE)
    return(list(type = "local", path = path))
  }

  if (!grepl("^[A-Za-z][A-Za-z0-9.]*$", pkg)) {
    stop(
      "`pkg` must be a package name, local directory, local archive, HTTP(S) URL, or Git reference.",
      call. = FALSE
    )
  }
  list(type = "repository", package = pkg)
}

build_resolved_install_plan <- function(
  source,
  metadata,
  target,
  lib,
  repositories = NULL,
  configured = NULL,
  acquisition = NULL,
  platform = current_platform_facts(),
  verbose
) {
  direct <- direct_dependencies(metadata$package, target)
  remotes <- parse_remotes(metadata$fields)
  remote_map <- remote_source_map(remotes)
  remote_packages <- intersect(direct, names(remote_map))
  dependency_sources <- prepare_remote_sources(
    remotes,
    remote_packages,
    lib,
    platform,
    verbose
  )
  if (is.null(configured)) {
    configured <- empty_repository_index(target)
  }

  declared <- source_dependency_packages(metadata, dependency_sources)
  repository_dependencies <- setdiff(
    declared,
    c("R", names(dependency_sources))
  )
  if (length(repository_dependencies) && is.null(repositories)) {
    report_step(verbose, "Reading configured package repositories.")
    repositories <- configured_repositories()
    configured <- configured_source_index(repositories)
  }

  available <- combine_repository_indexes(
    target,
    configured,
    metadata$package
  )
  available <- append_source_indexes(available, dependency_sources)
  additional <- setdiff(
    reachable_remote_packages(metadata, target, available, remote_map),
    names(dependency_sources)
  )
  if (length(additional)) {
    more_sources <- prepare_remote_sources(
      remotes,
      additional,
      lib,
      platform,
      verbose
    )
    dependency_sources <- c(dependency_sources, more_sources)
    available <- append_source_indexes(available, more_sources)
  }

  report_step(verbose, "Resolving required package dependencies.")
  repeat {
    dependencies <- setdiff(
      resolve_dependencies(metadata$package, available, lib),
      metadata$package
    )
    additional <- setdiff(
      intersect(dependencies, names(remote_map)),
      names(dependency_sources)
    )
    if (!length(additional)) {
      break
    }
    more_sources <- prepare_remote_sources(
      remotes,
      additional,
      lib,
      platform,
      verbose
    )
    dependency_sources <- c(dependency_sources, more_sources)
    available <- append_source_indexes(available, more_sources)
  }

  list(
    plan = new_install_plan(
      source = source,
      metadata = metadata,
      direct = direct,
      available = available,
      repositories = repositories,
      dependencies = dependencies,
      lib = lib,
      acquisition = acquisition,
      platform = platform,
      dependency_sources = dependency_sources
    ),
    artifact = NULL
  )
}

new_install_plan <- function(
  source,
  metadata,
  direct,
  available,
  repositories,
  dependencies,
  lib,
  acquisition,
  platform,
  dependency_sources = list()
) {
  requirements <- dependency_requirements(
    metadata$package,
    dependencies,
    available
  )
  declared <- unique(vapply(requirements, `[[`, character(1L), "name"))
  unavailable <- setdiff(declared, c("R", available[, "Package"]))
  reported <- unique(c(dependencies, unavailable))
  constraints <- dependency_constraints(
    metadata$package,
    reported,
    available
  )

  structure(
    list(
      source = source,
      acquisition = acquisition,
      platform = platform,
      candidates = plan_candidates(
        source = source,
        metadata = metadata,
        dependencies = dependencies,
        available = available,
        platform = platform,
        acquisition = acquisition,
        dependency_sources = dependency_sources
      ),
      target = list(
        package = metadata$package,
        version = metadata$version,
        type = metadata$type,
        format = metadata$format
      ),
      dependencies = dependency_plan_table(
        reported,
        dependencies,
        direct,
        unavailable,
        available,
        constraints
      ),
      status = if (length(unavailable)) "blocked" else "ready",
      library = lib,
      archive_metadata = metadata,
      available = available,
      repositories = repositories,
      install_dependencies = dependencies,
      unavailable = unavailable,
      remotes = parse_remotes(metadata$fields),
      dependency_sources = dependency_sources
    ),
    class = "zak_plan"
  )
}

dependency_plan_table <- function(
  reported,
  dependencies,
  direct,
  unavailable,
  available,
  constraints
) {
  if (!length(reported)) {
    return(data.frame(
      Package = character(),
      Version = character(),
      Repository = character(),
      Status = character(),
      Direct = logical(),
      Constraint = character(),
      stringsAsFactors = FALSE
    ))
  }

  positions <- match(reported, available[, "Package"])
  present <- !is.na(positions)
  versions <- rep(NA_character_, length(reported))
  versions[present] <- available[positions[present], "Version"]
  repositories <- rep(NA_character_, length(reported))
  if ("Repository" %in% colnames(available)) {
    repositories[present] <- available[positions[present], "Repository"]
  }
  constraint_text <- vapply(
    reported,
    function(package) paste(constraints[[package]], collapse = ", "),
    character(1L)
  )

  data.frame(
    Package = reported,
    Version = versions,
    Repository = repositories,
    Status = ifelse(
      reported %in% unavailable,
      "unavailable",
      ifelse(reported %in% dependencies, "install", "available")
    ),
    Direct = reported %in% direct,
    Constraint = constraint_text,
    stringsAsFactors = FALSE
  )
}

cleanup_plan_artifact <- function(artifact) {
  if (!is.null(artifact)) {
    release_acquired_artifact(artifact)
  }
  invisible()
}

#' @export
print.zak_plan <- function(x, ...) {
  target <- x$target
  source <- x$source
  source_text <- switch(
    source$type,
    url = source$url,
    local = source$path,
    local_archive = source$path,
    git = paste0(source$url, "@", source$ref),
    bioconductor = source$package,
    repository = source$package
  )
  cat(
    sprintf(
      "zak install plan: %s %s (%s)\n",
      target$package,
      target$version,
      target$type
    )
  )
  cat(sprintf("Source: %s (%s)\n", source_text, source$type))
  cat(sprintf("Status: %s\n", x$status))
  if (nrow(x$dependencies)) {
    print(x$dependencies, row.names = FALSE)
  } else {
    cat("Dependencies: none\n")
  }
  invisible(x)
}
