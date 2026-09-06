report_step <- function(verbose, text) {
  if (isTRUE(verbose)) {
    message("[zak] ", text)
  }
  invisible()
}

report_dependencies <- function(package, dependencies, available) {
  requirements <- dependency_requirements(package, dependencies, available)
  declared <- unique(vapply(requirements, `[[`, character(1L), "name"))
  unavailable <- setdiff(declared, c("R", available[, "Package"]))
  reported <- unique(c(dependencies, unavailable))

  if (!length(reported)) {
    message("Dependencies: none need installation.")
    return(invisible())
  }

  constraints <- dependency_constraints(package, reported, available)
  lines <- vapply(
    reported,
    format_dependency,
    character(1L),
    available = available,
    constraints = constraints,
    unavailable = unavailable
  )

  message(
    sprintf(
      "Dependencies (%d to install, %d unavailable):\n%s",
      length(dependencies),
      length(unavailable),
      paste0("  - ", lines, collapse = "\n")
    )
  )
  invisible()
}

dependency_constraints <- function(package, dependencies, available) {
  constraints <- structure(
    vector("list", length(dependencies)),
    names = dependencies
  )
  requirements <- dependency_requirements(package, dependencies, available)

  for (requirement in requirements) {
    name <- requirement$name
    if (!name %in% dependencies || length(requirement) < 3L) {
      next
    }
    constraint <- paste(requirement$op, as.character(requirement$version))
    constraints[[name]] <- unique(c(constraints[[name]], constraint))
  }

  constraints
}

dependency_requirements <- function(package, dependencies, available) {
  parents <- intersect(c(package, dependencies), rownames(available))
  fields <- intersect(c("Depends", "Imports", "LinkingTo"), colnames(available))

  requirements <- list()
  for (parent in parents) {
    declarations <- available[parent, fields, drop = TRUE]
    declarations <- declarations[!is.na(declarations) & nzchar(declarations)]
    requirements <- c(requirements, split_dependency_field(declarations))
  }

  requirements
}

format_dependency <- function(package, available, constraints, unavailable) {
  position <- match(package, available[, "Package"])
  version <- if (package %in% unavailable || is.na(position)) {
    "unavailable"
  } else {
    available[position, "Version"]
  }
  requirements <- constraints[[package]]
  requirement <- if (length(requirements)) {
    paste("requires", paste(requirements, collapse = ", "))
  } else {
    "no version constraint"
  }

  sprintf("%s %s [%s]", package, version, requirement)
}
