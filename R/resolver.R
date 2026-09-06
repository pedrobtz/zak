# The fields `install.packages(dependencies = NA)` installs.
installation_dependency_fields <- function() {
  c("Depends", "Imports", "LinkingTo")
}

# Selects the packages that must be installed for `package`, in installation
# order (dependencies before dependents). Reproduces the selection
# `utils::install.packages(dependencies = NA)` performs, so a zak plan and a
# plain install agree; see `R/dependencies.R`.
resolve_dependencies <- function(package, available, lib = NULL) {
  requested <- unique(package)
  present <- requested %in% rownames(available)
  base <- vapply(requested, is_base_package, logical(1L))

  if (any(base)) {
    warning(
      sprintf(
        "%s is a base package, and should not be updated.",
        paste(sQuote(requested[base]), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  unavailable_requests <- requested[!present & !base]
  if (length(unavailable_requests)) {
    warning(
      sprintf(
        "package %s is not available for this version of R.",
        paste(sQuote(unavailable_requests), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  selected <- requested[present]
  if (!length(selected)) {
    return(selected)
  }

  installed <- utils::installed.packages(
    lib.loc = dependency_resolution_libraries(lib),
    fields = c("Package", "Version")
  )
  fields <- installation_dependency_fields()
  packages <- requested
  pending <- selected
  unavailable <- character()

  repeat {
    declarations <- apply(
      available[pending, fields, drop = FALSE],
      1L,
      function(row) paste(row[!is.na(row)], collapse = ", ")
    )
    resolved <- required_dependencies(declarations, installed, available)
    unavailable <- c(unavailable, resolved$unavailable)
    needed <- setdiff(unique(resolved$required), c("R", packages))
    if (!length(needed)) {
      break
    }
    # Prepended so the result stays in installation order.
    packages <- c(needed, packages)
    pending <- needed
  }

  if (length(unavailable)) {
    warning(
      sprintf(
        "dependency %s is not available.",
        paste(sQuote(unique(unavailable)), collapse = ", ")
      ),
      call. = FALSE,
      immediate. = TRUE
    )
  }

  packages <- unique(packages)
  packages <- packages[packages %in% rownames(available)]
  added <- setdiff(packages, selected)
  if (length(added)) {
    message(
      sprintf(
        "also installing the %s %s",
        if (length(added) == 1L) "dependency" else "dependencies",
        paste(sQuote(added), collapse = ", ")
      )
    )
  }
  packages
}

install_target_archive <- function(
  archive,
  metadata,
  url,
  lib,
  verbose = FALSE,
  record_url = TRUE
) {
  package <- archive
  type <- "source"

  if (identical(metadata$type, "source")) {
    report_step(verbose, "Extracting source package archive.")
    extracted <- extract_source_archive(archive, metadata)
    on.exit(unlink(extracted$root, recursive = TRUE, force = TRUE), add = TRUE)
    package <- extracted$package
    if (isTRUE(record_url)) {
      report_step(verbose, "Recording URL provenance in source metadata.")
      add_url_remote_metadata(package, url)
    }
  }

  if (identical(metadata$format, "zip") && identical(metadata$type, "binary")) {
    type <- binary_zip_install_type()
  }

  arguments <- list(
    pkgs = package,
    repos = NULL,
    type = type
  )
  arguments <- with_library_argument(arguments, lib)
  result <- do.call(utils::install.packages, arguments)

  if (identical(metadata$type, "binary") && isTRUE(record_url)) {
    report_step(
      verbose,
      "Recording URL provenance in installed binary metadata."
    )
    record_installed_url_remote(metadata, url, lib)
  }

  result
}

binary_zip_install_type <- function(os_type = .Platform$OS.type) {
  if (os_type != "windows") {
    stop(
      "Windows binary ZIP packages can only be installed on Windows.",
      call. = FALSE
    )
  }
  "win.binary"
}
