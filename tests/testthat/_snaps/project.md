# dependencies can include optional DESCRIPTION fields

    Code
      zak::dependencies(project, include_suggests = NA)
    Condition
      Error:
      ! `include_suggests` must be `TRUE` or `FALSE`.

# init creates a project lockfile without installing

    Code
      tryCatch(zak::init(fixture$source), error = function(error) stop(
        "Zak project metadata already exists."))
    Condition
      Error in `value[[3L]]()`:
      ! Zak project metadata already exists.

# init records metadata for a non-package project

    Code
      tryCatch(zak::init(project), error = function(error) stop(
        "Zak project metadata already exists."))
    Condition
      Error in `value[[3L]]()`:
      ! Zak project metadata already exists.

# activation prevents silently switching projects

    Code
      tryCatch(zak::activate(second), error = function(error) stop(
        "Another zak project is already active."))
    Condition
      Error in `value[[3L]]()`:
      ! Another zak project is already active.

# project initialization can opt out of an isolated library

    Code
      zak::project_library(project)
    Condition
      Error:
      ! This project was initialized without an isolated library.

---

    Code
      zak::activate(project)
    Condition
      Error:
      ! This project was initialized without an isolated library.

