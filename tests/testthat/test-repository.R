test_that("a target archive becomes an unfiltered repository row", {
  fixture <- make_fixture_package("zakfixtureindex")
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  metadata <- zak:::inspect_archive(fixture$archive)

  target <- zak:::build_target_index(metadata)

  expect_true(fixture$package %in% rownames(target))
  expect_identical(
    unname(target[fixture$package, "Version"]),
    fixture$version
  )
})

test_that("the URL target wins over a same-named configured package", {
  target <- matrix(
    c("samepackage", "1.0.0", "file:///target"),
    nrow = 1L,
    dimnames = list("samepackage", c("Package", "Version", "Repository"))
  )
  configured <- rbind(
    samepackage = c("samepackage", "9.0.0", "file:///configured"),
    dependency = c("dependency", "1.0.0", "file:///configured")
  )
  colnames(configured) <- colnames(target)

  combined <- zak:::combine_repository_indexes(
    target,
    configured,
    "samepackage"
  )
  expect_identical(unname(combined["samepackage", "Version"]), "1.0.0")
  expect_true("dependency" %in% rownames(combined))
  expect_equal(sum(combined[, "Package"] == "samepackage"), 1L)
})

test_that("target compatibility is left for R installation to check", {
  fixture <- make_fixture_package(
    "zakfixturefuture",
    fields = list(Depends = "R (>= 99.0.0)")
  )
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  metadata <- zak:::inspect_archive(fixture$archive)

  target <- zak:::build_target_index(metadata)
  expect_true(fixture$package %in% rownames(target))
})
