# pax — Roadmap

`pax` is an R package whose goal is to install R packages from remote sources.
The guiding constraint for the whole project: **base R only** — no Imports
beyond the packages that ship with R itself (`utils`, `tools`).

## v0.1.0 — Install a package from a URL to a `.tar.gz`

### Goal

Provide a single exported function that installs a source package from a URL
**including its dependencies**, with the same end result and a similar feel to
`install.packages()`:

```r
pax::install_url("https://example.com/pkg_1.2.3.tar.gz")
library(pkg)  # works — missing Depends/Imports/LinkingTo were installed too
```

One call is the full behaviour: download the tarball, work out which of its
dependencies are missing, install those from the configured repositories, then
install the package itself.

### Design

**Exported API**

```r
install_url(url,
            lib = .libPaths()[1L],
            dependencies = NA,      # NA = Depends/Imports/LinkingTo, as in install.packages()
            repos = getOption("repos"),
            quiet = FALSE,
            INSTALL_opts = character())
```

- `url` — one or more URLs ending in `.tar.gz` (vectorized like
  `install.packages(pkgs = ...)`).
- `lib` — target library, defaulting to the first element of `.libPaths()`,
  exactly as `install.packages()` does.
- `dependencies` — same convention as `install.packages()`: `NA` (default)
  installs missing `Depends`/`Imports`/`LinkingTo`; `TRUE` additionally
  installs `Suggests` (non-recursively); `FALSE` installs none and instead
  errors up front if hard dependencies are missing.
- `repos` — where missing dependencies are installed *from* (the tarball
  itself always comes from `url`). Defaults to the user's configured
  repositories, as in `install.packages()`.
- `quiet`, `INSTALL_opts` — passed through to the underlying install, mirroring
  the `install.packages()` arguments of the same names.

**Utility functions**

`available.packages()` and `installed.packages()` both return character
matrices, which are awkward to filter and join. Two small exported wrappers
return the same data as data frames with character columns, keeping the base R
column names so the base documentation still applies:

```r
available_packages(repos = getOption("repos"), ...)  # one row per available package
installed_packages(lib.loc = NULL, ...)              # one row per installed package
```

Both funnel through an internal `as_package_df()` that drops row names — a
package can legitimately appear twice (installed in several libraries,
distinguished by `LibPath`, or offered by several repositories) and duplicate
row names are an error for data frames. A zero-row input keeps its columns, so
callers can rely on the column set even when no repository is reachable.

These are the building blocks for the dependency step below: "what is already
installed and at which version" and "what can be installed from `repos`" both
become ordinary data frame subsetting.

A third helper goes the other way, from a package name to the address a
CRAN-like repository serves it at:

```r
package_url(pkgs, type = getOption("pkgType"), repos = getOption("repos"),
            available = NULL)
```

It builds `<Repository>/<Package>_<Version><ext>` from the `Repository` column
of `available_packages()`, with the extension following the requested type
(`.tar.gz` for source, `.zip` for Windows, `.tgz` for macOS) and an optional
`File` field overriding the file name, exactly as `utils::download.packages()`
does. Besides the type strings R itself uses, `type` accepts the OS aliases
`"windows"`, `"macos"` and `"linux"`; `"binary"` and `"both"` express a
preference rather than a file, so they resolve to `.Platform$pkgType`. Passing
a pre-fetched `available` data frame avoids contacting the repositories
repeatedly.

This closes the loop with `install_url()` — `install_url(package_url("jsonlite"))`
is the long way round to `install.packages("jsonlite")` — and gives the
dependency step a way to name the exact file it is about to install.

**How it works (all base R)**

1. **Download** the tarball with `utils::download.file(url, destfile,
   mode = "wb")` into a session temp dir from `tempfile()`. Respect the user's
   `options(download.file.method)`; fail with a clear error on non-zero status.
2. **Validate** the download: file exists, is non-empty, and is a gzip tarball
   (check the magic bytes `1f 8b` with `readBin` rather than trusting the URL
   extension). List contents with `utils::untar(tarfile, list = TRUE)` and
   verify there is a single top-level directory containing a `DESCRIPTION`
   file — i.e. it is a source package, not an arbitrary archive.
3. **Read metadata** from the extracted `DESCRIPTION` via `read.dcf()` to get
   the real `Package` name and `Version` (the filename is not trusted).
4. **Detect the package type** — a `.tar.gz` is not necessarily a *source*
   package: binary builds (`R CMD INSTALL --build` on Linux, macOS `.tgz`) are
   gzipped tarballs too, so the type cannot be assumed from the URL or
   extension. Detect it from the contents: a `Built:` field in `DESCRIPTION`
   (equivalently, a top-level `Meta/package.rds` in the tarball listing) marks
   a binary package; absence marks a source package.
   - **Source** → proceed to install (step 5).
   - **Binary** → v0.1.0 stops with a clear error stating that the tarball is
     a binary build (including the `Built:` platform string) and that only
     source tarballs are supported for now. Binary installs are scheduled for
     v0.2.0, where the `Built:` platform must additionally be checked against
     `R.version$platform` before installing.
5. **Resolve and install dependencies** — this is the part `install.packages()`
   does *not* do for local files: with `repos = NULL` it ignores its
   `dependencies` argument entirely and just runs `R CMD INSTALL`, which
   aborts with a terse "dependency 'x' is not available" error if anything is
   missing. `pax` fills that gap, in base R:
   - Parse `Depends`/`Imports`/`LinkingTo` (plus `Suggests` when
     `dependencies = TRUE`) from the tarball's `DESCRIPTION` with base string
     functions (`tools:::.split_dependencies()`-style parsing).
   - Drop R itself and base-priority packages
     (`installed_packages(priority = "base")`).
   - Determine which are missing or too old by matching against
     `installed_packages(lib.loc = .libPaths())`, honouring version
     requirements like `pkg (>= 1.2)` via `utils::compareVersion()`.
   - Install the missing ones with a single
     `utils::install.packages(missing, lib = lib, repos = repos,
     quiet = quiet)` call. This delegates *transitive* dependency resolution
     to `install.packages()` itself — recursion for free, still base R.
   - If a dependency is not available in `repos` (checked against
     `available_packages(repos = repos)`), stop before installing anything,
     with one error listing every unavailable package.
   - With `dependencies = FALSE`, skip the install and keep only the check:
     missing hard dependencies produce a single clear upfront error instead
     of a mid-install failure.
6. **Install** by delegating to the same machinery `install.packages()` uses
   for local files: call `utils::install.packages(pkgs = tarball_path,
   repos = NULL, type = "source", lib = lib, quiet = quiet,
   INSTALL_opts = INSTALL_opts)` — safe here because step 4 has verified the
   tarball really is a source package. This gives us `R CMD INSTALL`
   semantics — compilation of src/, staged install, lock directories — for
   free and matches what `install.packages()` itself does for local files.
7. **Verify & report**: confirm the package can be found in `lib` afterwards
   (`find.package(pkg, lib.loc = lib)`); return (invisibly) a data frame with
   `package`, `version`, `lib`, `url`, one row per input URL, mirroring the
   invisible-return convention of `install.packages()`.
8. **Clean up** downloaded files with `on.exit(unlink(...), add = TRUE)` even
   on error.

**Error behaviour**

- Errors are signalled with `stop()` and messages that name the failing URL.
- With multiple URLs, match `install.packages()` behaviour: attempt all,
  warn per failure, and error only if every install failed.
- No partial installs left behind: rely on `R CMD INSTALL`'s staged install /
  lock mechanism rather than reimplementing it.

### Explicit non-goals for v0.1.0

- Dependencies are installed from `repos` (CRAN-like repositories) only — a
  dependency that itself only exists as a URL tarball is not resolved
  recursively; that produces the "not available in repos" error. Chaining URL
  installs is a possible later feature.
- No upgrading of already-installed dependencies that satisfy the version
  requirements — only missing or too-old packages are touched.
- No binary packages: binary tarballs are *detected* (see step 4) but
  rejected with an informative error rather than installed; `.zip` is not
  accepted at all. No `git`/GitHub refs, no repos — source tarball URLs only.
- No authentication headers, retries, or proxies beyond what
  `download.file()` already supports.
- No caching of downloads.

### Package skeleton

```
pax/
├── DESCRIPTION        # Package: pax, Depends: R (>= 3.6), Imports: utils, tools
├── NAMESPACE          # export(install_url, available_packages, installed_packages, package_url)
├── LICENSE
├── R/
│   ├── install_url.R  # exported entry point
│   ├── packages.R     # available_packages() / installed_packages() + as_package_df()
│   ├── package_url.R  # package_url() + type/extension resolution (internal)
│   └── utils.R        # download/validate/metadata helpers (internal)
├── man/
│   ├── install_url.Rd
│   ├── available_packages.Rd
│   ├── installed_packages.Rd
│   └── package_url.Rd # hand-written, kept in sync with the roxygen comments in R/
├── tests/
│   ├── test-packages.R      # plain base-R tests run via R CMD check
│   ├── test-package_url.R
│   ├── test-install_url.R
│   └── ...            # fixtures: a tiny valid source package tarball, a corrupt file
└── README.md
```

Testing stays dependency-free too: plain `stopifnot()`-style test scripts under
`tests/` executed by `R CMD check`. Network-independent tests use `file://`
URLs pointing at fixture tarballs built during the test run with
`R CMD build` on a minimal in-test package.

### Milestones for v0.1.0

1. **M1 — Skeleton**: DESCRIPTION, NAMESPACE, license, the
   `available_packages()` / `installed_packages()` / `package_url()` utilities
   with tests, and an empty `install_url()` stub; `R CMD check` passes clean.
2. **M2 — Happy path**: download + validate + install a single URL with no
   missing dependencies; test with a `file://` fixture tarball.
3. **M3 — Dependency installation**: DESCRIPTION dependency parsing with
   version requirements, missing/outdated detection, availability check
   against `repos`, install via `install.packages()`, `dependencies =
   NA/TRUE/FALSE` semantics; tests using a fixture package that depends on a
   second fixture package served from a local `file://` repository (built
   with `tools::write_PACKAGES()` — still base R).
4. **M4 — Robustness**: magic-byte validation, DESCRIPTION metadata check,
   source-vs-binary detection (reject binary tarballs with a clear error),
   cleanup on error, clear error messages; tests for corrupt/non-package
   archives, binary tarballs, unavailable dependencies, and unreachable URLs.
5. **M5 — Parity details**: multiple URLs, `lib`/`repos`/`quiet`/
   `INSTALL_opts` passthrough, invisible return value; document behaviour
   differences (if any) from `install.packages()` in the man page.
6. **M6 — Release**: README with examples, NEWS.md, version bumped to 0.1.0,
   `R CMD check --as-cran` clean on Linux/macOS/Windows.

## Later versions (directional, not committed)

- **v0.2.0** — binary tarballs (`.tar.gz` with a `Built:` field, `.tgz`,
  `.zip`) with platform-compatibility checks against `R.version$platform`, and
  checksum verification (`sha256 =` argument using `tools::md5sum`-style
  helpers or a base implementation).
- **v0.3.0** — convenience resolvers: GitHub release/tag URLs expanded to
  tarball URLs, and recursive URL-to-URL dependency chains (a dependency that
  is itself only available as a URL tarball), still with zero added
  dependencies.
