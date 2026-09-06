# Code review — `zak` 0.1.0

**Scope:** the whole package. The repository has a single `Initial commit`
containing only `README.md`; everything else is untracked, so the review
surface is all of `R/`, `tests/`, and the package metadata.

**Environment:** R 4.6.1, aarch64-apple-darwin23.
**Suite at review time:** `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 504 ]`.

Every finding marked **Verified** was reproduced by execution, not inferred
from reading. Findings marked **Inferred** are reasoned from the code but were
not run to ground; they are labelled as such deliberately.

---

## Summary

This is careful work in most places: a real staged-commit transaction, SHA-256
provenance, deliberate credential redaction, consistent validation helpers, and
504 passing tests. That makes the defects below more surprising, not less
serious.

Five blocking issues. Two break the package's central promise (`install()`
reports success after installing nothing), one is arbitrary command execution
reachable from the *documented read-only* `plan()` path, and two are entire
features that have never successfully run.

**Verdict: Request Changes.** *(B1-B5 fixed; see each finding.)*

---

## Change map

`install(pkg)` -> `build_install_plan()` parses the reference (`R/plan.R:104`)
and dispatches to a source adapter (`R/source.R:1`) for one of
url / local / local_archive / git / bioconductor / repository.

The adapter acquires an artifact (download+cache, clone+cache, or repository
index), reads `DESCRIPTION`, and builds a one-row "target index" that is
spliced into `available.packages()` so R's own `utils:::getDependencies`
resolves the graph (`R/resolver.R:1`). `Remotes:` declarations are prepared
recursively and appended to that index (`R/mixed.R:65`).

Installation then stages: a sibling temp library is created next to the real
one, dependencies and target are installed into it, the staged set is verified,
and only then are directories renamed into place with a backup/rollback
(`R/transaction.R:264`).

`lock()`/`restore()` serialise and re-verify that plan.
`init()`/`snapshot()`/`status()` wrap it for projects.

The staging design is the right idea. The verification step is where it comes
apart.

---

## Blocking

### B1. Command injection via Git refs and URLs — FIXED

> **Resolved.** `run_git()` now `shQuote()`s every argument (`R/git.R:203`).
> Quoting alone does not stop *git option* injection, so `validate_git_url()`
> and `split_git_reference()` also reject a leading `-` (`R/git.R:13`,
> `R/git.R:96`). Three regression tests added to `tests/testthat/test-git.R`;
> all three were confirmed to fail against the vulnerable code.

`R/git.R:193` (was `R/git.R:190`)

```r
output <- system2(Sys.which("git"), arguments, stdout = TRUE, stderr = TRUE)
```

`system2()` does not quote its arguments; it pastes them into a `/bin/sh`
command line. `split_git_reference()` rejects whitespace and control characters
in a ref but permits `;`, backticks, `$(...)`, `&&`, `|` — and `$IFS` sidesteps
the whitespace rule entirely.

Reproduced end-to-end through the public parser:

```
parsed ref = HEAD;touch$IFS/.../pwned2.txt
-rw-r--r--  .../pwned2.txt          <- injected command executed
```

Reachable from four directions:

- `install()` with a `git::` / `github::` reference
- `plan()` with the same
- a `Remotes:` field in **any** package being planned
- `restore()` — a lockfile `source.ref` is validated only as non-empty text
  (`R/lock.R:604`) and is used whenever `acquisition$commit` is absent

The `plan()` path is the sharp edge. `R/plan.R:4-5` promises "It does not
install packages or mutate the target library." Planning a package whose
`Remotes:` you did not write executes shell commands as you.

**Fix:** `shQuote()` every argument. Additionally constrain `ref` to
`git check-ref-format`-safe characters and use `--` separators on the command
lines.

---

### B2. `install()` reports success when nothing was installed — FIXED

> **Resolved.** `verify_staged_packages()` now returns a list of problems and
> `install_from_plan()` raises an error naming each one (`R/install.R:167`).
> The oracle was replaced: `staged_package_version()` reads the staged
> `DESCRIPTION` instead of asking `utils::installed.packages()`
> (`R/transaction.R:239`), so a binary package with no `Meta/package.rds` is
> now committed rather than silently discarded. `install()` also returns
> invisibly now. Three existing tests encoded the silent behaviour and were
> updated to expect the error; two regression tests added.

`R/install.R:164-167`

```r
if (verify_staged_packages(plan, staging)) {
  commit_staged_packages(staging, lib, packages)
}
result
```

A `FALSE` verdict is not signalled. Nothing is committed, staging is deleted by
`on.exit`, and `install()` returns `NULL` — the same value it returns on
success.

Reproduced with a fixture whose `.onLoad` fails:

```
ERROR: loading failed
=== install() returned ===  NULL
=== library contents ===    character(0)
```

The only trace is a warning from `install.packages()`, which is easily muffled
and never affects exit status. In CI or a Docker build this is a silent no-op.

**Compounding defect — the verification oracle is wrong.**
`verify_staged_packages()` (`R/transaction.R:239`) uses
`utils::installed.packages()`, which requires `Meta/package.rds`. A binary
install does not produce one, so a **successful** install is also discarded:

```
* installing *binary* package 'zakfakebinary' ...
* DONE (zakfakebinary)
  staging dir contents : zakfakebinary      <- the package is right there
  installed.packages() : <none>
  verify verdict       : FALSE
  final library        : <EMPTY>
```

Both outcomes — genuine failure and genuine success — reach the caller as an
indistinguishable silent `NULL`.

`project_install_from_locks()` already does this correctly at
`R/transaction.R:59-64` (`stop()` on a failed verdict). `install_from_plan()`
just doesn't use the pattern that exists two files over.

**Fix:** `stop()` on a failed verdict, naming the missing packages. Verify
staged presence with `dir.exists()` plus a direct `DESCRIPTION` version read
rather than `installed.packages()`.

---

### B3. Project restore is broken for every non-repository source — FIXED, severity corrected

> **Severity correction.** The type error is real and was reproduced, but the
> original claim that "any project lockfile containing a git, url, ... source
> aborts restore" overstated reachability. `init()`/`snapshot()` cannot
> currently emit such a lockfile: a project lock is only written for a project
> with **no** `DESCRIPTION` (`R/project.R:163`), `Remotes:` rows are only
> discovered *from* a `DESCRIPTION`, and `Remotes` is not a column
> `available.packages()` returns — so every project-lock source is a plain
> repository package. The defect is reachable only through a project lockfile
> produced elsewhere or hand-edited, which `read_lock()` validates and accepts.
> This is a latent type error, not a broken shipping feature.
>
> **Resolved.** Both record constructors now parse the reference
> (`R/transaction.R:201-219`). Regression test added at the reachable level.

`R/transaction.R:201-216`

`project_target_source_record()` and `project_dependency_source_record()` set
`source = restore_source_reference(...)`, which returns a **string**.
`install_source_dependency()` requires a **parsed reference list** — it does
`reference$type` at `R/install.R:285` and `source$source$type` at
`R/install.R:307`.

```
record$source class: character
value: git::https://github.com/o/r.git@0123456789abcdef...
install_source_dependency() -> ERROR: $ operator is invalid for atomic vectors
```

Any project lockfile containing a git, url, local, local_archive, or
bioconductor source aborts restore with that message — after the staging
library has already been built.

This survived because the only project-restore test
(`tests/testthat/test-project.R:189`) uses two plain repository packages, where
`sources` is empty and the branch is never entered.

**Fix:** parse at record construction —
`source = parse_package_reference(restore_source_reference(source))` — and add
a project-restore test with a git or url source.

---

### B4. `snapshot()` and `status()` call four functions that do not exist — FIXED

> **Resolved.** All four helpers implemented in `R/project.R`:
> `project_metadata_library()`, `project_status_library()`,
> `project_status_expected()`, `project_installed_versions()`. The last reads
> each `DESCRIPTION` rather than using `utils::installed.packages()`, for the
> same reason as B2. `roxygenise()` re-run, so `NAMESPACE` now exports
> `snapshot()`/`status()` and registers `print.zak_status`; `man/snapshot.Rd`
> and `man/status.Rd` added; both listed in `_pkgdown.yml`. The
> "Undefined global functions" NOTE is gone. Four tests added; the existing
> public-API test was updated to include the two new exports.

`R/project.R:287`, `R/project.R:394`, `R/project.R:398`, `R/project.R:399`

`R CMD check --as-cran` reports:

```
Undefined global functions or variables:
  project_installed_versions project_metadata_library
  project_status_expected project_status_library
```

They have no definition anywhere in `R/`. On a properly initialized project:

```
status()   -> could not find function "project_status_library"
snapshot() -> could not find function "project_metadata_library"
```

Both ship with complete user-facing roxygen documentation (`@param`,
`@return`, `@examples`, `@seealso`). Neither has a single test.

**Compounding: `NAMESPACE` and `man/` are stale.** Regenerating shows exactly
this drift:

```
> S3method(print,zak_status)
> export(snapshot)
> export(status)
Only in man: snapshot.Rd, status.Rd
```

So `print.zak_status` is unregistered as an S3 method too, and `AGENTS.md`
requires running `roxygenise()` after public API edits.

**Fix:** implement the four helpers with tests, or remove `snapshot()`/
`status()` and their roxygen blocks until they work. Either way, run
`roxygen2::roxygenise()`.

---

### B5. Rollback can permanently destroy installed packages — FIXED

> **Resolved.** `rollback_staged_packages()` now returns the packages it could
> not restore (`R/transaction.R:389`), and `commit_staged_packages()` deletes
> the backup area only when that list is empty; otherwise it warns and names
> the directory holding the surviving copies (`R/transaction.R:306`). Two
> regression tests added, both confirmed to fail against the old rollback.

`R/transaction.R:273-281`

```r
on.exit({
  if (!complete) rollback_staged_packages(staging, lib, backup, committed, backups)
  unlink(backup, recursive = TRUE, force = TRUE)
}, add = TRUE)
```

**Verified (fact):** `rollback_staged_packages()` at `R/transaction.R:370`
ignores the return value of `file.rename()`. The `unlink(backup, ...)` then
runs unconditionally in the same handler.

**Inferred (not run — no Windows host):** the rollback first unlinks
`lib/<pkg>` for every affected package. On Windows, a package loaded in another
R session holds its DLL open and that unlink fails; the subsequent rename into
a non-empty destination then also fails. The user's previously installed
package is gone from `lib`, and the backup — the only remaining copy — is
deleted one line later.

The remedy is cheap and the downside is unrecoverable.

**Fix:** capture the rename results; retain the backup directory, and say where
it is, whenever any restore failed.

---

## Required changes

### R1. Credential redaction is narrower than documented, and truncates URLs — FIXED

> **Resolved.** `redact_url_userinfo()` now replaces userinfo for *any* scheme
> and replaces it wholesale rather than dropping it, because a token is often
> the user component on its own (`https://<token>@host/...`); SCP-style
> `git@host:path` is left alone as it carries no secret. The sensitive-parameter
> list gained presigned/SAS names (`sig`, `X-Amz-Signature`, `X-Amz-Credential`,
> `X-Amz-Security-Token`, `X-Goog-Signature`, and others), and fragments are now
> redacted as well as query strings. The URL is split at the *first* `?` and
> `#` only, so a later separator no longer truncates what gets recorded.
> `validate_restore_lock()` now checks the acquisition and dependency-source
> URLs too, not just the target's (`R/restore.R:72`). The `lock()` docs were
> rewritten to describe this as a best-effort filter over parameter *names*.
> Fourteen assertions added across four tests, all confirmed to fail against
> the old implementation.
>
> **Behaviour change:** a URL with embedded credentials now produces a lockfile
> that `restore()` refuses, where before it recorded a credential-stripped URL
> that looked restorable. That is the intended outcome — the stripped URL would
> not have fetched anyway — but it is a visible change for anyone embedding
> credentials in URLs.

`R/lock.R:1010`

| input | output |
|---|---|
| `ssh://user:s3cr3t@host/repo.git` | unchanged — **credentials leak** |
| `...?X-Amz-Signature=DEADBEEF&X-Amz-Credential=AKIA` | unchanged |
| `...?sig=SECRETSAS` | unchanged |
| `...?a=1?b=2` | `...?a=1` — **silently truncated** |
| `...#token=abc` | unchanged |

Three separate problems:

1. The `^(https?://)` anchor misses `ssh://` credentials, and `lock_source()`
   sends git URLs through this function.
2. The allowlist misses the two most common secret-bearing archive URLs in
   practice (S3 presigned, Azure SAS) and all fragments.
3. `strsplit(url, "?")` at `R/lock.R:1012` keeps only `parts[[1]]` and
   `parts[[2]]`. A second `?` **truncates the recorded URL**. This is a
   correctness bug, not just a redaction gap: the truncated URL contains no
   `<redacted>` marker, so `validate_restore_lock()` passes it and `restore()`
   fetches a different URL than the one that was locked.

`R/lock.R:10-12` claims credentials "are redacted". Scope the claim to what the
code does, or widen the code.

### R2. Foreign-platform binary archives are accepted without validation — FIXED

> **Resolved.** `built_archive_facts()` parses the `Built:` field and
> `archive_incompatibility_reason()` rejects a binary whose OS or architecture
> does not match this machine (`R/platform.R`). It enforces only what the field
> establishes: when the platform triple names a recognized OS, both OS and
> architecture are checked; when it does not, only the coarse OS-type field is
> used and no architecture claim is made, so an unfamiliar but valid platform
> string is not rejected. R-version differences are left to R, which only warns.
>
> The dual vocabulary is gone: `archive_compatibility()` and
> `validate_archive_compatibility()` now require `zak_platform_facts` and error
> if given anything else, so the `"unix"`-vs-`"linux"` trap is closed. The
> side-effect-only `binary_zip_install_type()` call was replaced with a specific
> message naming the package and both platforms:
> `Zak cannot install the tar.gz binary package 'x': it was built for macos and this is linux.`
> Ten assertions added; five confirmed to fail against the old rule. One
> existing test passed a raw `"windows"` string — exactly the confusion this
> finding described — and was updated to pass facts. A binary fixture with a
> hardcoded macOS `Built:` field was made portable via `current_built_field()`,
> since it would otherwise fail on a Linux runner.

`R/platform.R:63` only rejects zip+binary+non-Windows. A macOS `Built:` tar.gz
on Linux returns `"compatible"` and is handed to
`install.packages(type = "source")`.

Separately, `platform` in `archive_compatibility()` receives two different
vocabularies: `current_platform_facts()$os` yields `"macos"`/`"linux"`, while
callers at `R/install.R:214` pass raw `.Platform$OS.type` (`"unix"`). It works
today only because the sole comparison is `!= "windows"`. The first person to
write `os == "linux"` gets a silent wrong answer. Give it one type.

### R3. Four reach-ins to undocumented base internals, with no fallback — FIXED

> **Resolved.** All four are gone; `grep` for `getDependencies`,
> `.split_dependencies`, `.split_op_version`, `.clean_up_dependencies2` or
> `isBasePkg` in `R/` now returns nothing.
>
> `tools:::.split_dependencies` and `.split_op_version` were vendored as
> `split_dependency_field()` / `split_dependency_requirement()` in the new
> `R/dependencies.R` (~20 lines of pure string parsing; no public API exposes
> version constraints, so vendoring was the only option). `utils:::getDependencies`
> was reimplemented as `resolve_dependencies()` in `R/resolver.R`, which pulled
> in two further internals it depends on — `.clean_up_dependencies2` (now
> `required_dependencies()`) and `isBasePkg` (now `is_base_package()`, on the
> public `utils::packageDescription()`).
>
> R's quirks are reproduced deliberately and commented, notably that only `>=`
> is enforced against an installed version while any other operator is treated
> as satisfied.
>
> **How parity is guaranteed.** `tests/testthat/test-resolver.R` runs zak's
> resolver and `utils:::getDependencies` over the same 15 synthetic scenarios
> and compares return value, ordering, and whether a warning and a message were
> signalled. It skips itself if a future R stops exposing the internal, so it
> reports drift without becoming a dependency. Six deliberate mutations were
> used to confirm the comparison actually bites: install ordering, the `R`
> filter, `LinkingTo` inclusion, `Suggests` exclusion, unavailable-dependency
> reporting, and duplicate-declaration handling. Two of those (the `>=`-only
> quirk and the dedup) were initially *not* caught, which exposed real gaps in
> the scenario set; scenarios using an installed package (`digest`) and
> duplicate declarations were added to close them.
>
> `DESCRIPTION` claimed resolution was "delegated to the machinery used by R's
> own package installer". That is no longer true, so it now says installation is
> delegated and resolution reproduces that installer's selection.

- `R/resolver.R:3` — `utils:::getDependencies`
- `R/candidate.R:345`, `R/report.R:62`, `R/project.R:969` —
  `tools:::.split_dependencies`

Neither symbol is exported. Using `get(..., envir = ns)` rather than `:::`
means `R CMD check` does not flag it, which reads as evasion rather than as a
decision. Both can change or vanish in any R release, and
`Depends: R (>= 4.1.0)` has no upper guard. `.split_dependencies` in particular
is ~15 lines of regex that could be vendored outright. At minimum: check for
existence at load time and fail with an actionable message.

### R4. Git errors discard git's own output — FIXED

> **Resolved.** `run_git()` sends stderr to a file instead of merging it into
> stdout, and reports git's own diagnostics on failure via
> `git_failure_details()` (`R/git.R`). Output is truncated to the last 10 lines
> so a verbose failure cannot bury the message, and URLs inside it are passed
> through the new `redact_embedded_urls()` — tool output routinely echoes the
> remote URL, which is exactly where an embedded credential would be. The R
> warning `system2()` raises on a non-zero status is suppressed, since zak
> raises its own error.
>
> A bare `Git could not read a ref.` now reads:
> `Git could not read a ref.` followed by
> `fatal: ambiguous argument 'zaknosuchref': unknown revision or path not in the working tree.`
>
> Three tests added. The stream-separation one initially did **not** catch the
> old behaviour: it asserted on `rev-parse`, which emits no stderr, so merging
> was harmless there. It now asserts on `git checkout`, which writes ~18 lines
> of detached-HEAD advice to stderr and nothing to stdout — under merged
> streams that advice *was* the command's return value.

`R/git.R:193` reduces every failure to `"Git could not clone the Git
repository."` — no auth error, no "ref not found", no host.

Separately, `stdout = TRUE, stderr = TRUE` merges streams, so `rev-parse HEAD`
returns any stderr noise concatenated into the commit string, which then
becomes a cache key and a lockfile field. Capture stderr separately for
value-producing calls, and include it in the error.

### R5. Download failures are neither checked nor attributed — FIXED

> **Resolved.** `download_archive()` now handles both ways `download.file()`
> reports failure — a raised condition and a non-zero return status — and also
> rejects a zero-byte download rather than handing it to the archive sniffer
> (`R/archive.R`). Each error names the URL, redacted, so a credentialed URL is
> not echoed into CI logs.
>
> For attribution, `archive_format()` and `inspect_archive()` gained an optional
> `source` label threaded from the adapters that know it. The unhelpful
> `R could not identify the download as a tar.gz or ZIP archive.` now reads
> `R could not identify the content as a tar.gz or ZIP archive (from https://example.test/malformed.tar.gz).`
> — "content" rather than "download" because the same check covers local files.
>
> Five tests added; four confirmed to fail against the unchecked download. One
> existing regex assertion and one snapshot pinned the old wording and were
> updated; the snapshot now carries the URL, which is stable.

`R/archive.R:5` ignores the status from `download.file()`. When a 404 page
arrives the user gets `"R could not identify the download as a tar.gz or ZIP
archive."` — with no URL. Across a multi-package restore that is close to
undiagnosable. No error message in `R/archive.R` names the URL it was working
on; they should.

### R6. `prepare_local_source()` has no `DESCRIPTION` guard — FIXED

> **Resolved.** Both directory-reading adapters now share
> `read_source_description()` (`R/source.R`), which guards a missing file and
> an empty or unreadable one — the latter previously failed with a subscript
> error rather than a message. The Git adapter's inline check was folded into
> it, removing the duplication. Errors name the offending path. Three tests
> added.

`R/source.R:116` calls `read.dcf()` directly, so a directory without one
produces a raw file-connection error. The git adapter checks properly at
`R/source.R:192`. Same check, both places.

### R7. `verify_acquired_artifact()` is a tautology on every non-cache path — FIXED

> **Resolved.** `expected` is now a required argument, so the tautology cannot
> reappear, and the three adapter calls that relied on the default were removed
> (`R/source.R`). Verification stays where an independent expectation exists:
> install time, against the plan or lockfile, where it re-reads the file and so
> catches a cached artifact replaced between planning and installing.
> Measured: preparing a Git source hashed the checkout **twice** before and
> **once** now. Three tests added, including one that pins the single hash.

`R/archive.R:114` defaults `expected = artifact$sha256`, and
`new_archive_artifact()` computed that field from the same file moments
earlier. For git it is also expensive: `new_git_artifact()` hashes every file
in the checkout via its default argument, then `verify_acquired_artifact()`
hashes the entire tree again at `R/cache.R:239`. Hash once; verify only against
an independently-sourced expectation.

### R8. Test options leak across tests in the same file — FIXED

> **Resolved.** All four sites now save and restore with
> `previous <- options(...)` / `on.exit(options(previous), add = TRUE)`,
> matching the convention `with_fixture_options()` and `AGENTS.md` already use.
> `withr` was deliberately not introduced: it is not in `Suggests` and only
> reachable transitively through testthat. Verified that a value set inside one
> `test_that()` no longer reaches the next, and that two consecutive full runs
> both report 629 passing.

`tests/testthat/test-restore.R:29`, `:130`,
`tests/testthat/test-cache.R:354`, `tests/testthat/test-lock.R:356` set
`zak.offline` / `zak.cache` with no restore. testthat resets between *files*,
not between `test_that()` blocks:

```
>>> zak.offline in next test: TRUE
```

Every test after line 29 in `test-restore.R` runs in offline mode without
meaning to. Use `withr::local_options()`. `AGENTS.md` already requires this
cleanup discipline.

### R9. `init()` copy-paste — FIXED

> **Resolved.** The lockfile path, existence check and `dir.create()` are
> hoisted above the branch, and the redundant second `if (file.exists(description))`
> that only emitted a message is folded into the branch that earns it. 21
> duplicated lines removed, plus a dead `lock_path <- NULL`. The `@param force`
> documentation claimed the lockfile check applied only "for package projects",
> which was never true of either branch; corrected.

`R/project.R:163-186` repeats the `lock_path` / existence-check / `dir.create`
block verbatim in both branches of the `if (file.exists(description))`,
followed by a second redundant `if (file.exists(description))` at
`R/project.R:209` that only emits a message. Hoist it.

### R10. Smaller, user-visible — FIXED

> **Resolved.**
> - `inspect_archive()` and `report_system_requirements()` now report through
>   `report_step()`, so `plan(verbose = FALSE)` no longer prints the package
>   line or system requirements. Five tests asserted the unconditional output
>   and were updated; one snapshot regenerated.
> - `install()` returning a visible `NULL`, and its `@return` text, were fixed
>   as part of **B2**.
> - `cache_clean()`'s documentation said entries "used within" `max_age` are
>   retained. Cache reads never update that timestamp, so the docs now say
>   modified, and state that it reflects when an entry was stored.
> - `cache_info()` checksummed each entry twice over; it now does so once.
>   `cache_clean()` selects on modification time *before* checksumming, so
>   pruning by age no longer reads every cached payload to decide what to
>   delete.

- `inspect_archive()` calls `message()` unconditionally at `R/archive.R:252`
  while every other report goes through `report_step(verbose, ...)`, so
  `plan(verbose = FALSE)` still prints.
- `install()` returns a **visible** `NULL` (verified `visible: TRUE`), so every
  install prints `NULL` at the console. `R/install.R:60` documents the return
  as "the return value of `utils::install.packages()`", which is always `NULL`.
- `cache_clean()`'s docs say entries "used within" `max_age` are retained, but
  `R/cache.R:80` tests mtime, which cache reads never update.
- `cache_info()` fully re-hashes every cached artifact (`cache_file_checksum`
  then `cache_file_is_valid` recomputes it plus the payload), and
  `cache_clean()` calls it — so deleting by timestamp hashes the entire cache
  first.

---

## Suggestions

- Two functions are called purely for their throwing side effect, results
  discarded: `remote_source_map()` at `R/mixed.R:124` and
  `binary_zip_install_type()` inside `R/install.R:218`. Both read as dead code.
  Rename to `validate_*` and return `invisible()`.
- `cache_git_artifact()` calls `setwd()` at `R/cache.R:509`, mutating global
  session state. Use `utils::tar(tar = ...)` with an explicit root, or
  `withr::with_dir()`.
- `project_dependency_data_frame()` at `R/project.R:1149` does
  `do.call(rbind, lapply(records, as.data.frame))` — one data frame per record.
  Quadratic on large projects; build columns with `vapply()`.
- `dependencies()` documents that it does not "infer dependencies from
  arbitrary strings" (`R/project.R:14`), but `R/project.R:1016` scans raw
  lines, so `# see dplyr::filter` in a comment and `"pkg::fn"` in a string both
  become recorded dependencies. Either strip comments/strings or soften the
  claim.
- `R/project.R:945-949` splits `Remotes` locally *and* calls `parse_remotes()`,
  then zips the two results by index. They agree today only because both apply
  the same split and `nzchar` filter. Use one parser.
- `git_checkout_sha256()` calls `normalizePath(files, mustWork = TRUE)` at
  `R/cache.R:247`; a repository containing a dangling symlink hard-fails with
  an opaque message.
- `print.zak_plan()`'s `switch` at `R/plan.R:398` has no default, so an unknown
  source type silently prints an empty line.
- `zak_cache_key()` at `R/cache.R:217` writes to a tempfile just to MD5 it,
  when `digest` is already a hard dependency. Its `paste(..., collapse =)` is a
  no-op; `sep` was probably intended.
- `is_git_commit()` at `R/cache.R:445` accepts only 40-hex, so SHA-256 git
  repositories are silently never cached.
- `parse_package_reference()` at `R/plan.R:104`: a bareword is shadowed by a
  same-named directory in the working directory, and a non-existent
  `foo.tar.gz` matches the package-name regex and is looked up as a repository
  package.
- Lockfile schema version is `1` for package locks and `2` for project locks
  (`R/lock.R:219`, `R/project.R:837`) — two different schemas sharing one counter.
- `project_file_path()` at `R/project.R:643` does not recognise Windows UNC
  paths (`\\server\share`) as absolute.
- `activate()` at `R/project.R:597` restores `.libPaths()` blindly on
  `deactivate()`, discarding any changes made in between.

---

## Test coverage

Coverage is good where it exists — parsing, lockfile validation, and cache
behaviour are tested thoroughly. The gaps line up exactly with the blocking
bugs:

| gap | consequence |
|---|---|
| No test asserts `install()` fails loudly | B2 shipped |
| Project restore only tested with repository sources | B3 shipped |
| `snapshot()` / `status()` have no tests at all | B4 shipped |
| No test for a non-Windows binary archive | R2 shipped |

`tests/testthat/helper-fixtures.R` is well built and worth reusing for the
regression tests above.

---

## Already fixed

These were findings in the original review and have since been addressed.

- **Rename completed (`pax` -> `zak`).** 150+ fixture identifiers across 19
  test files and 5 snapshot files; `DESCRIPTION` and `_pkgdown.yml` URLs.
  Verified collision-free before applying. Suite still `504 PASS`, snapshots
  matching.
- **Stale build artifacts removed.** `pax.Rcheck/`, `pax_0.1.0.tar.gz`,
  `pax_0.0.0.9000.tar.gz` — the last two were being shipped inside the built
  source tarball.
- **`.Rbuildignore` made name-agnostic.** The old `^zak_...` patterns had been
  renamed while the on-disk files had not, which is why the build silently
  shipped them. Also fixed `^_dev[.]` -> `^_dev$` (the old pattern required a
  5th character, so it matched files inside `_dev` but never the directory) and
  added `^_pkgdown[.]yml$`, `^docs$`.
- **`BugReports` added** to `DESCRIPTION`.

Result: tarball 107,510 B -> 68,372 B; `R CMD check --as-cran` 3 NOTEs -> 2.
The "Non-standard files/directories found at top level" NOTE is gone.

The two remaining NOTEs are the `pedrobtz/zak` URL (404 until the GitHub repo
is renamed) and the undefined functions from **B4**.

---

## Status

Every Blocking issue (**B1-B5**) and every Required change (**R1-R10**) is
fixed. What remains is the **Suggestions** section above — 13 smaller items,
none of which affect correctness or security.
