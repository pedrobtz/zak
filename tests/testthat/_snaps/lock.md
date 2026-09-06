# blocked plans cannot be locked

    Code
      zak::lock(package_plan, file)
    Condition
      Error:
      ! Only ready installation plans can be locked.

# read_lock rejects unsupported schema versions

    Code
      zak::read_lock(file)
    Condition
      Error:
      ! Unsupported zak lockfile schema version: 2.

