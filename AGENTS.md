# Repository Guidelines

## Project Structure & Module Organization

`zak` is an R package for installing remote tar.gz and ZIP package archives with their required repository dependencies. Package metadata lives in `DESCRIPTION` and exports are recorded in the roxygen-generated `NAMESPACE`. Implementation files are under `R/`, grouped by responsibility (`archive.R`, `install.R`, `packages.R`, `remote.R`, `report.R`, `repository.R`, and `resolver.R`). Tests live in `tests/testthat/`; shared package/repository fixtures belong in `helper-fixtures.R`, while feature tests use `test-<area>.R`. Public documentation is generated into `man/` from roxygen comments in `R/`. Do not commit build outputs such as `*.tar.gz` or `*.Rcheck/`.

## Build, Test, and Development Commands

- `R -q -e 'testthat::test_local(".")'` runs the test suite directly from the working tree.
- `R CMD build .` creates the source archive (currently `zak_0.1.0.tar.gz`).
- `R CMD check --no-manual zak_0.1.0.tar.gz` performs the package checks used before review. Update the filename when `DESCRIPTION` changes.
- `R -q -e 'roxygen2::roxygenise()'` regenerates `NAMESPACE` and `man/*.Rd` after public API or documentation edits.

## Coding Style & Naming Conventions

Follow the existing base-R style: two-space indentation, spaces around operators, and opening braces on the same line. Use `snake_case` for functions and variables, descriptive helper names, and `L` suffixes for integer literals where type matters. Qualify non-base calls (`utils::untar`, `tools::write_PACKAGES`) instead of attaching packages. Keep the public surface limited to `install()` and the package explorer helpers; internal calls in tests use `zak:::` only when necessary. Never hand-edit generated `NAMESPACE` or `.Rd` files.

## Testing Guidelines

Use testthat edition 3 and write behavior-focused `test_that()` descriptions. Add unit coverage near the matching module and end-to-end fixture tests when installation order or repository behavior changes. Tests must use temporary directories and clean them with `on.exit()`; avoid real network repositories by reusing the fixture builders and mocked downloads.

## Commit & Pull Request Guidelines

History currently contains only `Initial commit`, so no detailed convention is established. Use a short, imperative subject (for example, `Report unavailable dependencies`) and keep each commit focused. Pull requests should explain user-visible behavior, list tests run, link relevant issues, and include updated roxygen output when the public interface changes. Screenshots are generally unnecessary for this CLI-style package; include captured console output when reporting-message formatting changes.

## Security & Configuration

Installing source archives executes package-supplied code. Use trusted URLs in examples and tests, do not embed credentials, and preserve R's configured `getOption("repos")` behavior unless a change explicitly targets repository selection.
