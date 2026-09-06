remote_source_map <- function(remotes, declared_by = "the root package") {
  if (!length(remotes)) {
    return(list())
  }

  ambiguous <- Filter(function(remote) is.null(remote$package), remotes)
  if (length(ambiguous)) {
    stop(
      sprintf(
        "Remote source identity is ambiguous; zak cannot infer a package name from: %s.",
        paste(
          vapply(
            ambiguous,
            function(remote) {
              remote_source_label(remote$source)
            },
            character(1L)
          ),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  packages <- vapply(remotes, `[[`, character(1L), "package")
  duplicates <- unique(packages[duplicated(packages)])
  if (length(duplicates)) {
    details <- vapply(
      duplicates,
      function(package) {
        sources <- remotes[packages == package]
        sprintf(
          "package '%s' from %s",
          package,
          paste(
            vapply(
              sources,
              function(remote) {
                remote_source_label(remote$source)
              },
              character(1L)
            ),
            collapse = " and "
          )
        )
      },
      character(1L)
    )
    stop(
      sprintf(
        "Remote source conflict: %s defines multiple sources for %s.",
        declared_by,
        paste(details, collapse = "; ")
      ),
      call. = FALSE
    )
  }
  stats::setNames(
    lapply(remotes, `[[`, "source"),
    packages
  )
}

prepare_remote_sources <- function(
  remotes,
  packages,
  lib,
  platform,
  verbose
) {
  pending <- Filter(
    function(remote) {
      !is.null(remote$package) && remote$package %in% packages
    },
    remotes
  )
  pending <- lapply(pending, function(remote) {
    c(remote, list(declared_by = "the root package"))
  })
  records <- list()
  declarations <- list()

  while (length(pending)) {
    remote <- pending[[1L]]
    pending <- pending[-1L]
    package <- remote$package
    if (package %in% names(records)) {
      declared <- declarations[[package]]
      if (!identical(declared$source, remote$source)) {
        stop(
          sprintf(
            paste0(
              "Remote source conflict for package '%s': %s declared by %s ",
              "conflicts with %s declared by %s."
            ),
            package,
            remote_source_label(declared$source),
            declared$declared_by,
            remote_source_label(remote$source),
            remote$declared_by
          ),
          call. = FALSE
        )
      }
      next
    }

    declarations[[package]] <- list(
      source = remote$source,
      declared_by = remote$declared_by
    )

    record <- prepare_remote_source_record(
      package,
      remote$source,
      lib,
      platform,
      verbose
    )
    records[[package]] <- record

    nested <- parse_remotes(record$metadata$fields)
    remote_source_map(nested, declared_by = record$metadata$package)
    nested_direct <- direct_dependencies(
      record$metadata$package,
      record$target
    )
    nested <- Filter(
      function(candidate) candidate$package %in% nested_direct,
      nested
    )
    pending <- c(
      pending,
      lapply(nested, function(candidate) {
        c(candidate, list(declared_by = record$metadata$package))
      })
    )
  }

  records
}

prepare_remote_source_record <- function(
  package,
  source,
  lib,
  platform,
  verbose
) {
  prepared <- tryCatch(
    prepare_package_source(source, lib, verbose, platform),
    error = function(error) {
      stop(
        sprintf(
          "Could not prepare remote dependency '%s' from %s: %s",
          package,
          remote_source_label(source),
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
  on.exit(cleanup_plan_artifact(prepared$artifact), add = TRUE)

  if (!identical(prepared$metadata$package, package)) {
    stop(
      sprintf(
        paste0(
          "Remote source %s was declared for dependency '%s' but contains ",
          "package '%s'."
        ),
        remote_source_label(source),
        package,
        prepared$metadata$package
      ),
      call. = FALSE
    )
  }

  list(
    source = prepared$source,
    metadata = prepared$metadata,
    target = prepared$target,
    acquisition = prepared$acquisition,
    repositories = prepared$repositories
  )
}

append_source_indexes <- function(available, records) {
  if (!length(records)) {
    return(available)
  }
  for (record in records) {
    available <- append_repository_index(available, record$target)
  }
  available
}

source_dependency_packages <- function(metadata, records) {
  targets <- c(
    list(list(metadata = metadata, target = build_target_index(metadata))),
    unname(lapply(records, function(record) {
      list(metadata = record$metadata, target = record$target)
    }))
  )
  unique(unlist(lapply(targets, function(item) {
    direct_dependencies(item$metadata$package, item$target)
  })))
}

reachable_remote_packages <- function(metadata, target, available, remotes) {
  pending <- direct_dependencies(metadata$package, target)
  seen <- character()
  reachable <- character()

  while (length(pending)) {
    package <- pending[[1L]]
    pending <- pending[-1L]
    if (package %in% seen) {
      next
    }
    seen <- c(seen, package)
    if (package %in% names(remotes)) {
      reachable <- c(reachable, package)
    }
    if (package %in% rownames(available)) {
      package_index <- available[package, , drop = FALSE]
      pending <- c(
        pending,
        direct_dependencies(package, package_index)
      )
    }
  }

  unique(reachable)
}
