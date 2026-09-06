# a failed staged commit restores replaced packages

    Code
      zak:::commit_staged_packages(staging, library, c("zakfixtureexisting",
        "zakfixturemissing"))
    Condition
      Error:
      ! The staged package 'zakfixturemissing' is missing.

