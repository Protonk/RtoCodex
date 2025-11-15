# RtoCodex

RtoCodex is an R package designed as a small, controlled testbed for understanding how GPT-5 Codex behaves when working with R projects locally and inside OpenAI’s universal container. The repository focuses on how differences in environment, toolchains, and package installation can affect compilation and testing of R packages that include native code.

## Status

- R package skeleton intended for experimentation, not production.
- Initial focus: detecting compilation issues across environments.
- Future focus: accumulating small, targeted examples of environment-sensitive behavior discovered while developing with GPT-5 Codex.

## Using RtoCodex locally

RtoCodex is meant to be cloned and used as a working directory rather than installed from CRAN.

```sh
# from the project root
R CMD INSTALL --preclean -l build-lib/unit .
Rscript tests/run_unit_tests.R --lib=build-lib/unit
R CMD check RtoCodex
```
The `Makefile` includes a `test` target that automates the install + harness
invocation if you prefer `make test`.

## Harness automation

- `make fuzz-all` (or `Rscript scripts/run_fuzz.R --n=9999`) tells the fuzz harness to cover the entire `(USE_CPP20, USE_OPENMP)` grid while logging under `artifacts/fuzz/run_<timestamp-id>/combo/`. The `make matrix` target now simply aliases `make fuzz-all` for compatibility.
- `make fuzz` (or `Rscript scripts/run_fuzz.R`) samples a handful of rows from the same deterministic grid, installs/tests them with the shared helpers, and records the results under `artifacts/fuzz/run_<timestamp-id>/`.
- `make fuzz-once` requests a single random combo. Pass `ARGS="--n=8 --seed=123"` with any fuzz target to control the sample size and RNG seed; artifacts always include `config.rds` (host profile, runtime metadata, seed) and `summary.csv` (matrix toggles, expected vs observed statuses, `expectation_match`, and log paths).
- `make fuzz-clean` removes `build-lib/fuzz` and `artifacts/fuzz` if you need to reclaim space or reset between fuzz campaigns.

## Debug or "event" narratives

Complex or potentially problematic changes (e.g., a problem partially resolved by changes to the project but discovered due to PEBKAC) are logged under `event-narratives/` (see `AGENTS.md` for the format); `event-narratives/event_11_25_Path.md` shows the current template in practice for a deliberately trivial example.
