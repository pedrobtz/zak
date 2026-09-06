# zak

`zak` installs an R package from a configured repository, local package
directory, local archive, HTTP(S) URL, or Git/GitHub reference, including its
required dependencies. Source packages may be distributed as `.tar.gz` or
`.zip` archives.

```r
zak::install("jsonlite")
zak::install("/path/to/local/package")
zak::install("/path/to/local/package_1.0.0.tar.gz")
zak::install("https://example.org/examplePackage_1.0.0.tar.gz")
zak::install("https://example.org/examplePackage_1.0.0.zip")
zak::install("github::owner/repository@main")
zak::install("bioc::GenomicRanges")
```

To inspect dependency resolution before installation, use `plan()`. It does
not install packages or mutate the target library:

```r
package_plan <- zak::plan("jsonlite")
package_plan$dependencies
zak::lock(package_plan, "zak.lock")
```

For a project, `dependencies()` reports package metadata and static source
evidence from `DESCRIPTION`, R, R Markdown, Quarto, and Sweave files:

```r
project_dependencies <- zak::dependencies(".")
zak::init(".")
```

For a package project, `init()` resolves the local package and writes a
package lock. For a non-package project, it resolves discovered repository or
remote package references into a project lock containing one embedded plan per
reference. Both modes also record discovery evidence in `.zak/project.json`.
Neither mode installs packages, creates an isolated library, or activates a
project library. `init()` creates an isolated `.zak/library` by default;
use `isolated = FALSE` to opt out. `zak::restore()` restores a non-package
project lock into that library by validating its embedded plans and installing
their selected packages in one staged transaction. Activate it explicitly for
the current R session:

```r
zak::activate(".")
zak::deactivate()
```

`activate()` prepends the project library to `.libPaths()` and
`deactivate()` restores the previous library paths. Use
`zak::project_library(".")` to inspect the configured path.
Static discovery does not evaluate code or infer computed package names, and
skips `.git`, `.zak`, `renv`, `packrat`, and `node_modules` directories.

The plan records the selected package versions, repositories, dependency
constraints, and whether the plan is ready or blocked by unavailable
dependencies. For a URL source, `package_plan$acquisition` records the URL,
archive format, size, SHA-256 checksum, and retrieval metadata without
retaining the temporary local path. The archive is removed when planning
completes. For a local archive source, the same metadata records a `file://`
URL and the local archive is preserved.

Remote HTTP(S) archives and commit-resolved Git source trees are cached under
R's user cache directory by default. Set `options(zak.cache = "/path/to/cache")`
to relocate it. Each cache payload has a SHA-256 sidecar; a missing or
mismatched checksum causes a fresh acquisition that repairs the entry. Zak
also verifies the acquired artifact against the plan checksum before install.
Inspect the cache with `zak::cache_info()` and remove entries with
`zak::cache_clean()`. Use `dry_run = TRUE` to preview cleanup. Set
`options(zak.offline = TRUE)` to require valid cached archives and
commit-resolved Git sources without making network requests.

Git sources use an explicit reference syntax. The requested ref and resolved
commit are recorded in the plan and lockfile, while the temporary checkout
path is not retained:

```r
zak::plan("git::https://github.com/owner/repository.git@main")
zak::plan("github::owner/repository@main")
```

Bioconductor sources use `bioc::package`. They resolve against the
Bioconductor repositories returned by `BiocManager::repositories()`. Package
`DESCRIPTION` fields can also contain `Remotes:` entries such as
`github::owner/repository@main`. When a remote package is listed in
`Imports`, `Depends`, or `LinkingTo`, its source replaces the repository
candidate in the dependency graph. Git, URL, and Bioconductor remotes are
installed in dependency order alongside repository packages, and their
resolved source records are available in `package_plan$dependency_sources`.
Remote package identities must be inferable from the declaration; ambiguous
or unsupported provider syntax is rejected. Conflicting direct or transitive
remote declarations identify the package, both sources, and their declaring
packages; failed remote acquisition names the affected dependency and source.

Every plan also records normalized platform facts so future candidate
selection can be explicit about the operating system, architecture, R
version, and configured package type:

```r
package_plan$platform
```

This iteration records those inputs but does not yet choose binaries or claim
that an artifact built for one platform can be restored on another.

Plans also carry normalized candidate records in `$candidates`. The target and
each selected dependency share fields for source, repository, platform, R
compatibility, artifact size, hashes, system requirements, and provenance.
Acquired archive and Git candidates include a SHA-256 hash; platform-specific
candidate selection remains future work.

`lock()` writes a deterministic, versioned JSON record from a ready plan. It
captures selected versions, dependency decisions, per-dependency remote
sources, repositories, platform facts, system requirements, SHA-256 source
hashes, and URL provenance. Read, compare, and restore a lockfile with the
same verified source decisions:

```r
package_lock <- zak::read_lock("zak.lock")
zak::compare_lock(package_lock, package_plan)
zak::restore("zak.lock", lib = "/path/to/R/library")
```

The comparison reports whether stable source, repository, platform, target,
dependency, and artifact decisions have drifted. Fetch timestamps and methods
are ignored because they describe acquisition history rather than resolution.
`restore()` refuses to install when those decisions drift or when an acquired
source has no recorded SHA-256 checksum. Repository packages can be restored
while the configured repository metadata still matches the lockfile;
historical repository snapshots remain future work.

Source ZIP archives are extracted and installed through R's source-package
installer, whether they are local files or URL downloads. A ZIP whose
`DESCRIPTION` contains `Built` is a Windows binary and can be installed only
on Windows.

Zak records URL provenance in the installed package's `DESCRIPTION` using the
same fields as `remotes::install_url()`:

```text
RemoteType: url
RemoteUrl: https://example.org/examplePackage_1.0.0.tar.gz
```

This lets `renv::snapshot()` create a `Source: URL` record that can be restored
from the original archive. The full URL is retained, including its query
string, so do not use URLs containing embedded credentials or secret tokens.

Package databases can be explored as ordinary data frames while retaining the
columns documented by R:

```r
available <- zak::available_packages()
installed <- zak::installed_packages()
installed[installed$Package == "zak", c("Package", "Version", "LibPath")]
```

By default, `lib` is omitted from the underlying `install.packages()` calls, so
R performs its normal library selection. A specific library can be requested:

```r
zak::install("jsonlite", lib = "/path/to/R/library")
```

Set `verbose = TRUE` to report each zak step. Step messages use a `[zak]`
prefix and cover validation, download, archive inspection, dependency
resolution and installation, target installation, and URL provenance:

```r
zak::install("https://example.org/examplePackage_1.0.0.tar.gz", verbose = TRUE)
```

This does not suppress or replace R's own download, build, and installation
output.

While reading package metadata, `zak` reports whether an archive identifies a
source or binary package. Other package validity and compatibility checks are
left to R's own repository and installation functions.

Before installation, it also reports the dependency plan with the repository
version selected by R and the constraints declared by the target or transitive
dependencies:

```text
Package: 'examplePackage' 1.0.0 (source)
System requirements: libcurl, OpenSSL
Dependencies (2 to install, 0 unavailable):
  - dependencyA 2.1.0 [requires >= 2.0.0]
  - dependencyB 1.4.0 [no version constraint]
```

The system-requirements line is shown only when `DESCRIPTION` contains a
non-empty `SystemRequirements` field. Zak does not check or install system
dependencies.

This differs from passing an archive URL directly to `install.packages()`. R
infers `repos = NULL` for that input and installs the archive without running
its repository dependency resolver. `zak` presents the archive metadata to the
same resolver used by `install.packages()`, installs unresolved `Depends`,
`Imports`, and `LinkingTo` packages, and then installs the requested archive.

The function uses `getOption("repos")` and R's normal library, package-type, and
download configuration. `Suggests` and `Enhances` are not installed.

Installation is staged: required dependencies and the target are verified in
a temporary library before they are committed to the requested library. If a
dependency or target fails to install, packages from that operation are not
committed; packages already present in the requested library are preserved.

Installing packages executes package-supplied code; source packages may also
require compilers and system libraries. Only install archives from URLs you
trust. Dependency installation is not transactional: dependencies installed
before a later failure are retained.
