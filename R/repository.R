build_target_index <- function(metadata) {
  dependency_fields <- c(
    "Package",
    "Version",
    "Depends",
    "Imports",
    "LinkingTo",
    "Suggests",
    "Enhances"
  )
  columns <- unique(c(dependency_fields, names(metadata$fields)))
  index <- matrix(
    NA_character_,
    nrow = 1L,
    ncol = length(columns),
    dimnames = list(metadata$package, columns)
  )
  index[1L, names(metadata$fields)] <- unname(metadata$fields)
  index
}

file_url <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (.Platform$OS.type == "windows") {
    paste0("file:///", path)
  } else {
    paste0("file://", path)
  }
}

direct_dependencies <- function(package, target_index) {
  dependencies <- tools::package_dependencies(
    package,
    db = target_index,
    which = "strong",
    recursive = FALSE
  )[[1L]]
  setdiff(dependencies, "R")
}

configured_repositories <- function() {
  getOption("repos")
}

bioc_repositories <- function() {
  namespace <- tryCatch(
    asNamespace("BiocManager"),
    error = function(error) NULL
  )
  if (is.null(namespace)) {
    stop(
      "The `BiocManager` package is required for Bioconductor references.",
      call. = FALSE
    )
  }
  getExportedValue(namespace, "repositories")()
}

configured_source_index <- function(repositories) {
  utils::available.packages(repos = repositories, type = "source")
}

empty_repository_index <- function(template) {
  template[FALSE, , drop = FALSE]
}

combine_repository_indexes <- function(target, configured, package) {
  if (nrow(configured)) {
    configured <- configured[configured[, "Package"] != package, , drop = FALSE]
  }

  columns <- union(colnames(target), colnames(configured))
  target <- add_repository_columns(target, columns)
  configured <- add_repository_columns(configured, columns)
  combined <- rbind(target, configured)
  rownames(combined) <- combined[, "Package"]
  combined
}

append_repository_index <- function(index, addition) {
  if (!nrow(addition)) {
    return(index)
  }

  columns <- union(colnames(index), colnames(addition))
  index <- add_repository_columns(index, columns)
  addition <- add_repository_columns(addition, columns)
  packages <- addition[, "Package"]
  if (nrow(index)) {
    index <- index[!rownames(index) %in% packages, , drop = FALSE]
  }
  combined <- rbind(index, addition)
  rownames(combined) <- combined[, "Package"]
  combined
}

add_repository_columns <- function(index, columns) {
  missing <- setdiff(columns, colnames(index))
  if (length(missing)) {
    extra <- matrix(
      NA_character_,
      nrow = nrow(index),
      ncol = length(missing),
      dimnames = list(rownames(index), missing)
    )
    index <- cbind(index, extra)
  }
  index[, columns, drop = FALSE]
}
