# pax — Roadmap

`pax` is an R package whose goal is to install R packages from remote sources.
The guiding constraint for the whole project: **base R only** — no Imports
beyond the packages that ship with R itself (`utils`, `tools`).

## v0.1.0 — Install a package from a URL to a `.tar.gz`

### Goal

Provide a single exported function that installs a source package from a URL,
with the same end result and a similar feel to `install.packages()`:

```r
pax::install_url("https://example.com/pkg_1.2.3.tar.gz")
library(pkg)  # works
```

### Design

**Exported API**

```r
install_url(url,
            lib = .libPaths()[1L],
            dependencies = FALSE,   # v0.1.0: not yet supported, must be FALSE
            quiet = FALSE,
            INSTALL_opts = character())
```

- `url` — one or more URLs ending in `.tar.gz` (vectorized like
  `install.packages(pkgs = ...)`).
- `lib` — target library, defaulting to the first element of `.libPaths()`,
  exactly as `install.packages()` does.
- `quiet`, `INSTALL_opts` — passed through to the underlying install, mirroring
  the `install.packages()` arguments of the same names.

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
4. **Install** by delegating to the same machinery `install.packages()` uses
   for local files: call `utils::install.packages(pkgs = tarball_path,
   repos = NULL, type = "source", lib = lib, quiet = quiet,
   INSTALL_opts = INSTALL_opts)`. This gives us `R CMD INSTALL` semantics —
   compilation of src/, staged install, lock directories — for free and
   guarantees "works the same as install.packages()".
5. **Verify & report**: confirm the package can be found in `lib` afterwards
   (`find.package(pkg, lib.loc = lib)`); return (invisibly) a data frame with
   `package`, `version`, `lib`, `url`, one row per input URL, mirroring the
   invisible-return convention of `install.packages()`.
6. **Clean up** downloaded files with `on.exit(unlink(...), add = TRUE)` even
   on error.

**Error behaviour**

- Errors are signalled with `stop()` and messages that name the failing URL.
- With multiple URLs, match `install.packages()` behaviour: attempt all,
  warn per failure, and error only if every install failed.
- No partial installs left behind: rely on `R CMD INSTALL`'s staged install /
  lock mechanism rather than reimplementing it.

### Explicit non-goals for v0.1.0

- No dependency resolution (`dependencies = TRUE` errors with "not yet
  supported"; planned for a later release).
- No binary packages (`.zip` / `.tgz`), no `git`/GitHub refs, no repos —
  source tarball URLs only.
- No authentication headers, retries, or proxies beyond what
  `download.file()` already supports.
- No caching of downloads.

### Package skeleton

```
pax/
├── DESCRIPTION        # Package: pax, Depends: R (>= 3.6), Imports: utils, tools
├── NAMESPACE          # export(install_url); importFrom(utils, ...)
├── LICENSE
├── R/
│   ├── install_url.R  # exported function
│   └── utils.R        # download/validate/metadata helpers (internal)
├── man/
│   └── install_url.Rd # written by hand or via roxygen2 (dev-time only, not a dependency)
├── tests/
│   ├── test-install_url.R   # plain base-R tests run via R CMD check
│   └── ...            # fixtures: a tiny valid source package tarball, a corrupt file
└── README.md
```

Testing stays dependency-free too: plain `stopifnot()`-style test scripts under
`tests/` executed by `R CMD check`. Network-independent tests use `file://`
URLs pointing at fixture tarballs built during the test run with
`R CMD build` on a minimal in-test package.

### Milestones for v0.1.0

1. **M1 — Skeleton**: DESCRIPTION, NAMESPACE, license, empty `install_url()`
   stub; `R CMD check` passes clean.
2. **M2 — Happy path**: download + validate + install a single URL; test with a
   `file://` fixture tarball.
3. **M3 — Robustness**: magic-byte validation, DESCRIPTION metadata check,
   cleanup on error, clear error messages; tests for corrupt/non-package
   archives and unreachable URLs.
4. **M4 — Parity details**: multiple URLs, `lib`/`quiet`/`INSTALL_opts`
   passthrough, invisible return value; document behaviour differences (if
   any) from `install.packages()` in the man page.
5. **M5 — Release**: README with examples, NEWS.md, version bumped to 0.1.0,
   `R CMD check --as-cran` clean on Linux/macOS/Windows.

## Later versions (directional, not committed)

- **v0.2.0** — dependency resolution: parse `Depends`/`Imports`/`LinkingTo`
  from the tarball's DESCRIPTION and install missing ones from CRAN via
  `install.packages()`; `dependencies = TRUE` becomes functional.
- **v0.3.0** — binary tarballs (`.tgz`, `.zip`) with platform detection, and
  checksum verification (`sha256 =` argument using `tools::md5sum`-style
  helpers or a base implementation).
- **v0.4.0** — convenience resolvers: GitHub release/tag URLs expanded to
  tarball URLs, still with zero added dependencies.
