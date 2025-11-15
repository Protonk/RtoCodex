<!-- probes/AGENTS.md coaches capability probe authors on structure, wiring, and validation. -->
# Probes · AGENTS guide

This guide layers on top of the root-level `AGENTS.md` and focuses solely on `probes/`. Capability probes are tiny R scripts that classify sandbox quirks up front so the rest of the harness can skip, expect, or document behavior rather than fail mid-run.

## Probe workflow at a glance
- Each `probes/cap_<name>.R` script is invoked via `Rscript`, emits a single status token on stdout (e.g., `supported`, `missing`, `error`), and exits with `0` unless the classification itself signals `error`.
- `scripts/run_probes.R` enumerates every `cap_*.R`, calls them, stores per-capability text files under `artifacts/caps/`, and writes a JSON summary consumed by harness tooling.
- `Makefile` targets (`make cap_<name>`, `make caps`, `make caps-summary`) are thin wrappers so any environment can regenerate capability artifacts with one command.
- Package code and tests read the precomputed artifacts through `R/caps_helpers.R` helpers such as `needs_cap()`; they never re-run a probe directly.

## Directory landmarks
- `probes/cap_<name>.R`: individual probes. Start each file with a short “why this exists” comment, match the naming scheme exactly, and keep dependencies limited to base R or already-installed packages (document any expectation in the comment).
- `scripts/run_probes.R`: shared runner. It accepts `--cap=cap_name` to run exactly one probe, `--probes=DIR`, and `--artifacts=DIR`. Use this instead of hand-rolled loops.
- `artifacts/caps/`: run outputs. Every probe writes `artifacts/caps/cap_<name>.txt` containing its status token; `caps.json` mirrors those tokens for quick inspection.
- `tests/unit/test_capabilities.R`: regression tests that ensure each probe’s public contract (tokens + exit codes) stays stable. Extend this file whenever you add a probe.

## Authoring a new probe
1. **Capture the quirk.** Reproduce the sandbox behavior you care about and describe it in a short comment. Probes should highlight first-order blockers (compiler flag availability, filesystem behavior, locale handling, etc.), not long multi-step pipelines.
2. **Define stable tokens.** Choose a small set of lowercase, underscore-separated statuses. Keep them environment-agnostic (“missing” vs “WSL broken”), and decide which token means the harness should consider the probe itself failed (`error`, `probe_error`, etc.).
3. **Implement the script.** Use the `#!/usr/bin/env Rscript` shebang, follow it with a file-level comment, and write the detection logic. Print only the classification token to stdout; send diagnostics to stderr if they aid debugging. Exit with `quit(status = 0L)` for informational outcomes and a non-zero status for `error`-style tokens.
4. **Document assumptions.** If the probe shells out to system tools or relies on optional R packages, mention that in the top comment and consider downgrading missing tools to a non-fatal classification instead of crashing.

## Wiring probes into the harness
- Add a dedicated Make target that mirrors the existing pattern:
  ```make
  cap_new_capability:
  	mkdir -p $(CAP_ARTIFACTS)
  	Rscript $(PROBE_DIR)/cap_new_capability.R > $(CAP_ARTIFACTS)/cap_new_capability.txt
  ```
  Include the target name in the `.PHONY` declaration for convenience.
- `scripts/run_probes.R` automatically discovers any `cap_*.R` files, so once the script exists you can regenerate everything with `make caps` or just this probe with `Rscript scripts/run_probes.R --cap=cap_new_capability`.
- Confirm the JSON summary with `make caps-summary`; the command prints `artifacts/caps/caps.json`, which should contain your new capability token.

## Consuming capability data
- Tests and package code read `artifacts/caps/*.txt` via `R/caps_helpers.R`. Gate environment-sensitive work like this:
  ```r
  skip_if(!RtoCodex:::needs_cap("cap_new_capability", "supported"),
          "new capability not available here")
  ```
- Keep skip messages and assertions explicit about what behavior depends on the capability (e.g., “OpenMP flags missing”).
- When adding a probe you should also extend `tests/unit/test_capabilities.R` so the allowed tokens and exit-status contract are enforced. Use existing tests as templates: capture the probe output, assert that the token is within the supported set, and verify the exit code matches the classification semantics.

## Debugging and evolution
- Regenerate a single probe artifact with `make cap_<name>` or by running `Rscript scripts/run_probes.R --cap=<name>`. This captures stdout/stderr isolation identical to CI.
- If a probe fails unexpectedly, inspect the stderr snippet that `scripts/run_probes.R` prints and preserve the observation in `event-narratives/` when it signals a new environment quirk.
- Update `probes/AGENTS.md` whenever you codify a new category of quirk so future agents understand when to create a probe versus extending existing ones.
