test_that("staging libraries are created and cleaned up", {
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(library, recursive = TRUE), add = TRUE)

  staging <- zak:::create_staging_library(library)
  expect_identical(dir.exists(staging), TRUE)
  zak:::cleanup_staging_library(staging)
  expect_identical(file.exists(staging), FALSE)
})

test_that("verified staged packages are committed", {
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(library, recursive = TRUE), add = TRUE)

  staging <- zak:::create_staging_library(library)
  on.exit(zak:::cleanup_staging_library(staging), add = TRUE)
  package <- file.path(staging, "zakfixturecommitted")
  dir.create(package)
  writeLines("committed", file.path(package, "marker"))

  expect_silent(
    zak:::commit_staged_packages(
      staging,
      library,
      "zakfixturecommitted"
    )
  )
  expect_identical(
    readLines(file.path(library, "zakfixturecommitted", "marker")),
    "committed"
  )
})

test_that("a failed staged commit restores replaced packages", {
  library <- tempfile("zak-library-")
  dir.create(library)
  on.exit(unlink(library, recursive = TRUE), add = TRUE)

  existing <- file.path(library, "zakfixtureexisting")
  dir.create(existing)
  writeLines("old", file.path(existing, "marker"))

  staging <- zak:::create_staging_library(library)
  on.exit(zak:::cleanup_staging_library(staging), add = TRUE)
  replacement <- file.path(staging, "zakfixtureexisting")
  dir.create(replacement)
  writeLines("new", file.path(replacement, "marker"))

  expect_snapshot(error = TRUE, {
    zak:::commit_staged_packages(
      staging,
      library,
      c("zakfixtureexisting", "zakfixturemissing")
    )
  })
  expect_identical(
    readLines(file.path(existing, "marker")),
    "old"
  )
  expect_identical(
    file.exists(file.path(library, "zakfixturemissing")),
    FALSE
  )
})

test_that("rollback reports previous versions it could not restore", {
  # A rollback that cannot put a previous version back must say so, because
  # the caller keys the decision to delete the backup area off this result.
  # Deleting it after a failed restore destroys the only remaining copy.
  backup <- tempfile("zak-rollback-backup-")
  missing_library <- file.path(tempfile("zak-rollback-absent-"), "lib")
  dir.create(file.path(backup, "zakrollbackpkg"), recursive = TRUE)
  writeLines("old", file.path(backup, "zakrollbackpkg", "marker"))
  on.exit(unlink(backup, recursive = TRUE), add = TRUE)

  unrestored <- suppressWarnings(zak:::rollback_staged_packages(
    staging = tempfile("zak-rollback-staging-"),
    lib = missing_library,
    backup = backup,
    committed = "zakrollbackpkg",
    backups = "zakrollbackpkg"
  ))

  expect_identical(unrestored, "zakrollbackpkg")
  # The only surviving copy must still be in the backup area.
  expect_identical(
    readLines(file.path(backup, "zakrollbackpkg", "marker")),
    "old"
  )
})

test_that("rollback returns nothing when every previous version is restored", {
  library <- tempfile("zak-rollback-ok-library-")
  backup <- tempfile("zak-rollback-ok-backup-")
  dir.create(library, recursive = TRUE)
  dir.create(file.path(backup, "zakrollbackok"), recursive = TRUE)
  writeLines("old", file.path(backup, "zakrollbackok", "marker"))
  on.exit(unlink(c(library, backup), recursive = TRUE), add = TRUE)

  unrestored <- zak:::rollback_staged_packages(
    staging = tempfile("zak-rollback-ok-staging-"),
    lib = library,
    backup = backup,
    committed = character(),
    backups = "zakrollbackok"
  )

  expect_identical(unrestored, character())
  expect_identical(
    readLines(file.path(library, "zakrollbackok", "marker")),
    "old"
  )
})
