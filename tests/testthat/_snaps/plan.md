# plan rejects malformed archives

    Code
      zak::plan("https://example.test/malformed.tar.gz")
    Condition
      Error:
      ! R could not identify the content as a tar.gz or ZIP archive (from https://example.test/malformed.tar.gz).

