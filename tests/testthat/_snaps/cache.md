# offline mode reports an uncached archive

    Code
      zak:::acquire_archive(
        "https://example.test/zakfixtureuncachedoffline_1.0.0.tar.gz")
    Condition
      Error:
      ! Offline mode is enabled and no valid cached archive is available for https://example.test/zakfixtureuncachedoffline_1.0.0.tar.gz.

# offline mode reports an uncached Git source

    Code
      zak:::acquire_git_repository(
        "https://example.test/zakfixtureuncachedoffline.git", strrep("0", 40L))
    Condition
      Error:
      ! Offline mode is enabled and no valid cached Git source is available for https://example.test/zakfixtureuncachedoffline.git at commit 0000000000000000000000000000000000000000.

