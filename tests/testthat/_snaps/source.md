# unsupported source adapters fail explicitly

    Code
      zak:::source_adapter(list(type = "unknown", reference = "example"))
    Condition
      Error:
      ! No source adapter is registered for type: unknown.

