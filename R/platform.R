current_platform_facts <- function(
  os_type = .Platform$OS.type,
  r_os = R.version$os,
  r_arch = R.version$arch,
  r_version = as.character(getRversion()),
  package_type = getOption("pkgType", default = "both")
) {
  structure(
    list(
      os = normalize_platform_os(os_type, r_os),
      architecture = normalize_platform_architecture(r_arch),
      r_version = normalize_platform_value(r_version, "R version"),
      package_type = normalize_platform_value(package_type, "package type")
    ),
    class = c("zak_platform_facts", "list")
  )
}

normalize_platform_os <- function(os_type, r_os) {
  os_type <- normalize_platform_value(os_type, "OS type")
  r_os <- normalize_platform_value(r_os, "R OS")
  if (identical(tolower(os_type), "windows")) {
    return("windows")
  }
  if (grepl("darwin", r_os, ignore.case = TRUE)) {
    return("macos")
  }
  if (grepl("linux", r_os, ignore.case = TRUE)) {
    return("linux")
  }
  "unix"
}

normalize_platform_architecture <- function(architecture) {
  architecture <- tolower(
    normalize_platform_value(architecture, "architecture")
  )
  switch(
    architecture,
    aarch64 = "arm64",
    arm64 = "arm64",
    amd64 = "x86_64",
    x86_64 = "x86_64",
    i386 = "x86",
    i686 = "x86",
    x86 = "x86",
    architecture
  )
}

normalize_platform_value <- function(value, name) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value))
  ) {
    stop(sprintf("`%s` must be a single non-empty value.", name), call. = FALSE)
  }
  trimws(value)
}

# R writes `Built: R <version>; <platform>; <datetime>; <os type>` into a binary
# package's DESCRIPTION. The platform triple is empty for packages that need no
# compilation, in which case only the OS family is meaningful.
built_archive_facts <- function(fields) {
  if (is.null(fields) || !length(fields) || !"Built" %in% names(fields)) {
    return(NULL)
  }
  built <- unname(fields["Built"])
  if (!length(built) || is.na(built) || !nzchar(trimws(built))) {
    return(NULL)
  }
  parts <- trimws(strsplit(built, ";", fixed = TRUE)[[1L]])
  list(
    platform = if (length(parts) >= 2L) parts[[2L]] else "",
    os_type = if (length(parts) >= 4L && nzchar(parts[[4L]])) {
      tolower(parts[[4L]])
    } else {
      NA_character_
    }
  )
}

built_platform_os <- function(platform) {
  if (!length(platform) || is.na(platform) || !nzchar(platform)) {
    return(NA_character_)
  }
  if (grepl("darwin", platform, ignore.case = TRUE)) {
    return("macos")
  }
  if (grepl("linux", platform, ignore.case = TRUE)) {
    return("linux")
  }
  if (grepl("mingw|windows|w64|msys|cygwin", platform, ignore.case = TRUE)) {
    return("windows")
  }
  if (
    grepl(
      "solaris|sunos|freebsd|openbsd|netbsd|dragonfly|aix",
      platform,
      ignore.case = TRUE
    )
  ) {
    return("unix")
  }
  NA_character_
}

built_platform_architecture <- function(platform) {
  if (!length(platform) || is.na(platform) || !nzchar(platform)) {
    return(NA_character_)
  }
  normalize_platform_architecture(
    strsplit(platform, "-", fixed = TRUE)[[1L]][[1L]]
  )
}

platform_os_family <- function(os) {
  if (identical(os, "windows")) "windows" else "unix"
}

# Returns NULL when the archive can be installed here, or a sentence saying why
# it cannot. Only hard incompatibilities are reported: R itself merely warns
# about a binary built under a different R version, so that is left to R.
archive_incompatibility_reason <- function(
  metadata,
  platform = current_platform_facts()
) {
  if (!identical(metadata$type, "binary")) {
    return(NULL)
  }
  if (identical(metadata$format, "zip") && !identical(platform$os, "windows")) {
    return(sprintf(
      "it is a Windows binary ZIP archive and this is %s",
      platform$os
    ))
  }

  built <- built_archive_facts(metadata$fields)
  if (is.null(built)) {
    return(NULL)
  }

  # Only enforce what the `Built:` field actually establishes. When the
  # platform triple names an OS we recognize, both its OS and architecture are
  # trustworthy. When it does not, fall back to the coarse OS-type field and
  # make no claim about architecture, so an unfamiliar but valid platform
  # string is not rejected.
  built_os <- built_platform_os(built$platform)
  if (is.na(built_os)) {
    if (
      !is.na(built$os_type) &&
        !identical(built$os_type, platform_os_family(platform$os))
    ) {
      return(sprintf(
        "it was built for %s and this is %s",
        built$os_type,
        platform_os_family(platform$os)
      ))
    }
    return(NULL)
  }

  if (!identical(built_os, platform$os)) {
    return(sprintf(
      "it was built for %s and this is %s",
      built_os,
      platform$os
    ))
  }

  built_architecture <- built_platform_architecture(built$platform)
  if (
    !is.na(built_architecture) &&
      !identical(built_architecture, platform$architecture)
  ) {
    return(sprintf(
      "it was built for %s and this is %s",
      built_architecture,
      platform$architecture
    ))
  }
  NULL
}

archive_compatibility <- function(
  metadata,
  platform = current_platform_facts()
) {
  if (!inherits(platform, "zak_platform_facts")) {
    stop(
      "Archive compatibility needs zak platform facts.",
      call. = FALSE
    )
  }
  if (is.null(archive_incompatibility_reason(metadata, platform))) {
    "compatible"
  } else {
    "incompatible"
  }
}
