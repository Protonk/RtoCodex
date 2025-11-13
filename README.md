# RtoCodex

RtoCodex is an R package designed as a small, controlled testbed for understanding how GPT-5 Codex behaves when working with R projects locally and inside OpenAI’s universal container. The repository focuses on how differences in environment, toolchains, and package installation can affect compilation and testing of R packages that include native code.


## Status

- R package skeleton intended for experimentation, not production.
- Initial focus: detecting compilation issues across environments.
- Future focus: accumulating small, targeted examples of environment-sensitive behavior discovered while developing with GPT-5 Codex.

## Using RtoCodex locally

RtoCodex is meant to be cloned and used as a working directory rather than installed from CRAN.

```r
# in an R session started at the project root
install.packages("devtools")      # if not already installed
devtools::load_all()
devtools::test()
devtools::check()
```

## Harnessing build/test matrices

- `make matrix` (or `Rscript scripts/run_matrix.R`) iterates over every `(USE_CPP20, USE_OPENMP)` configuration, placing install/test logs plus `sessionInfo()` under `artifacts/matrix/<combo>/`.
- Pass `ARGS="--filter=USE_CPP20=0"` or `ARGS="--skip-tests"` to `make matrix` to focus on a subset without editing the script.
- `make matrix-container` mounts the repo into OpenAI’s universal Ubuntu container (via Docker/Podman), bootstraps compilers + R per `AGENTS.md`, then runs the same harness for apples-to-apples comparisons.
- Use `make clean-matrix` to wipe `build-lib/matrix` and `artifacts/matrix` between runs if you want a fresh slate.
- Complex or potentially problematic changes (e.g., a problem partially resolved by changes to the project but discovered due to PEBKAC) are logged under `test-narratives/` (see `AGENTS.md` for the format); `test-narratives/event_11_25_Path.md` shows the current template in practice for a deliberately trivial example.
