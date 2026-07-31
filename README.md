# pax

Install R packages straight from a URL, using only the packages that ship with
R itself.

See [roadmap.md](roadmap.md) for the plan.

## Development

Run the checks the way CRAN would:

```sh
R CMD build . && R CMD check pax_*.tar.gz
```

Tests that talk to a real CRAN mirror are opt-in, so a check never fails for
want of internet access:

```sh
PAX_NETWORK_TESTS=true R CMD check pax_*.tar.gz
```

`NOT_CRAN=true` enables them too, which most CI setups already set.
