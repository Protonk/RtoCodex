<!-- Captures the addition of regression tests around scripts/run_matrix.R -->
# Event 2025-11-13 Matrix Runner Coverage & Tooling Gap

## Summary
The matrix runner gained helper utilities, eight dedicated tests, and two toggles that contrasted the embedded harness with an external subprocess driver plus a guard-override flag. Even with these additions, every attempt to run the subprocess driver inside the Codex CLI still failed immediately because `processx` (loaded as part of the external driver) cannot read `kern.boottime`; macOS denies `sysctl kern.boottime`, so pkgbuild never runs its compiler probe and the test stage aborts before touching package code.

## Sequence
- Identified that scripts/run_matrix.R lacked coverage even though it drives every matrix run, so added a helper to load the script without calling `main()` and wrote unit tests plus a stubbed end-to-end run to pin down current behavior.
- Invoked the subprocess runner to exercise the new tests but pkgbuild stopped with “Could not find tools necessary to compile a package,” prompting deeper diagnostics.
- Traced the failure path (pkgbuild → callr → processx → ps) and reproduced the `processx` `.onLoad` error by loading the package directly; `ps::ps_boot_time()` needs `sysctl kern.boottime`, which this CLI cannot access.
- Executed `sysctl kern.boottime` manually to trigger the macOS permission prompt, but the command still returned “Operation not permitted,” confirming the restriction exists outside of R.
- Extended the matrix runner with explicit toggles so the harness now captures both the default “probe the toolchain via processx” path and the override that forces `pkgbuild.has_compiler = TRUE`, then re-ran the subprocess driver with the override set—the driver still failed because it loads `processx` before honoring the option.
- Added an explicit `ps::ps_boot_time()` detector/guard inside the matrix runner: the detector logs whether the sandbox blocks `kern.boottime`, and the guard temporarily overrides `ps::ps_boot_time()` (only for combos that opt into the workaround) so `processx` can initialize while we continue to run “no workaround” combos to document the failure mode.

## Locus
- **Trigger:** Upcoming matrix-runner refactors require regression coverage, and we need visibility into how the subprocess driver behaves with and without pkgbuild’s toolchain probe.
- **Diagnosis:** The Codex CLI environment lacks permission to read `kern.boottime`; `sysctl kern.boottime` and `ps::ps_boot_time()` both return “Operation not permitted,” causing `processx` (and therefore the external driver/pkgbuild) to fail before tests run. The matrix now exposes both subprocess modes, but neither can execute locally until the OS grants that permission or an alternate runner avoids `processx`.
- **Remediation:** Added helper utilities and tests for scripts/run_matrix.R, introduced matrix toggles + logging for the new subprocess scenarios, documented the permission failure path (including the unsuccessful `sysctl` attempt), and implemented a targeted `ps::ps_boot_time()` detector/guard so override-enabled combos can apply the workaround while guard-on combos still surface the sandbox failure.
- **Follow-up:** Provide this CLI session the macOS entitlement needed for `sysctl kern.boottime` (or an approved alternative for `ps::ps_boot_time()`), then rerun `scripts/run_matrix.R` to capture both subprocess matrix branches; if that is not possible, define a sanctioned test runner that bypasses `processx` so those combos can still be exercised automatically.
