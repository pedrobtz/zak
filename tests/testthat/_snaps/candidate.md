# candidate normalization rejects incomplete platform facts

    Code
      zak:::new_package_candidate(package = "example", version = "1.0.0", source = list(
        type = "repository", package = "example"), repository = NULL, platform = platform,
      type = "source", format = "repository", fields = c(Package = "example",
        Version = "1.0.0"))
    Condition
      Error:
      ! Package candidates need complete platform facts.

