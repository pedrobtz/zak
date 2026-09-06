# zak 0.1.0

* Rename the package to `zak`.
* `activate()`, `deactivate()`, and `project_library()` manage an isolated
  project library without implicit session changes; `init()` creates one by
  default and supports `isolated = FALSE` as an opt-out.
* `cache_clean()` removes cached archives and Git sources, with age filtering
  and dry-run support; `cache_info()` reports cache size, validity, checksums,
  and redacted source metadata.
* `compare_lock()` reports drift between a validated lockfile and a fresh
  ready plan without installing packages.
* `dependencies()` statically reports project dependencies and source evidence;
  `init()` records project metadata and resolves non-package references into a
  project lock, while local package projects keep their package lock format.
  Neither mode installs or activates an isolated library. `restore()` now
  validates embedded project plans and installs their selected packages in one
  staged transaction.
* `install()` accepts configured repository package names, local package
  directories, local `.tar.gz` or `.zip` archives, and Git/GitHub references;
  Bioconductor package references are also supported, and all successful
  dependency/target installs are staged before committing. Remote archive and
  commit-resolved Git artifacts are cached for reuse and validated with
  SHA-256 checksums before installation.
* `lock()` writes a versioned JSON record from a ready plan, including selected
  versions, dependency decisions, repositories, platform facts, system
  requirements, redacted URL provenance, and per-dependency remote source
  records with SHA-256 checksums.
* `plan()` exposes dependency resolution and package selection without
  installing packages, and records URL acquisition metadata without exposing
  temporary archive paths. Plans also record normalized platform facts for the
  current OS, architecture, R version, and package type configuration, along
  with normalized target and dependency candidate records. Local `.tar.gz` and
  `.zip` archives are inspected in place and retain local provenance. Git
  sources record their requested ref and resolved commit. `Remotes:` fields are
  normalized and used to resolve Git, URL, and Bioconductor dependencies
  alongside repository packages in one staged installation graph. Ambiguous,
  conflicting, and unavailable remotes report their dependency and source
  context.
* `read_lock()` validates a versioned zak JSON lockfile without installing
  packages or changing a library.
* `restore()` rebuilds and verifies a lockfile plan before staged installation,
  refusing source, repository, platform, dependency, or artifact drift. It
  also restores v2 project locks into their recorded isolated library through
  one aggregate staged transaction.
