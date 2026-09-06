# duplicate Remotes mappings fail before source acquisition

    Code
      zak:::remote_source_map(zak:::parse_remotes(c(Remotes = "github::owner/zakfixturedup, github::other/zakfixturedup")))
    Condition
      Error:
      ! Remote source conflict: the root package defines multiple sources for package 'zakfixturedup' from git::https://github.com/owner/zakfixturedup.git@HEAD and git::https://github.com/other/zakfixturedup.git@HEAD.

# ambiguous remote identities fail before source acquisition

    Code
      zak:::remote_source_map(zak:::parse_remotes(c(Remotes = "url::https://example.test/123-download")))
    Condition
      Error:
      ! Remote source identity is ambiguous; zak cannot infer a package name from: url::https://example.test/123-download.

# remote preparation errors name the dependency and source

    Code
      zak:::prepare_remote_source_record("zakfixtureunavailable", list(type = "url",
        url = "https://example.test/zakfixtureunavailable_1.0.0.tar.gz"), lib = NULL,
      platform = zak:::current_platform_facts(), verbose = FALSE)
    Condition
      Error:
      ! Could not prepare remote dependency 'zakfixtureunavailable' from url::https://example.test/zakfixtureunavailable_1.0.0.tar.gz: fixture source unavailable

# conflicting nested remote declarations name both parents

    Code
      suppressMessages(zak::plan(target$source, lib = library))
    Condition
      Error:
      ! Remote source conflict for package 'zakfixtureconflictshared': url::https://first.test/zakfixtureconflictshared_1.0.0.tar.gz declared by zakfixtureconflictfirst conflicts with url::https://second.test/zakfixtureconflictshared_1.0.0.tar.gz declared by zakfixtureconflictsecond.

