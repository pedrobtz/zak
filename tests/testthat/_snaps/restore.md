# restore blocks a lockfile with changed artifact provenance

    Code
      zak::restore(lockfile, lib = library)
    Condition
      Error:
      ! Zak lockfile drift prevents restore:
      - acquisition: locked provenance does not match the current plan.

# restore rejects lockfiles without acquired source checksums

    Code
      zak::restore(lockfile, lib = library)
    Condition
      Error:
      ! Zak cannot restore an acquired source without its SHA-256 checksum.

