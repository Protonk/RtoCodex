<!-- Event narrative documenting sink lifecycle bug when running the fuzz harness. -->

# Event 2025-11-14 Sink Lifecycle Bug

## Summary
- Running `make fuzz-once` aborted immediately after the first combo because the embedded matrix runner could not close its log sinks, leaving fuzz artifacts incomplete.
- Investigation showed `scripts/matrix_lib.R` opened a file connection for stdout/messages but closed it before unwinding both sink stacks, so R threw `cannot close 'message' sink connection`.
- Fixing the teardown to track baseline sink depth and unwind both stacks before closing the connection allowed fuzz runs to finish again.

## Sequence
- Invoked `make fuzz-once` to validate new probes; it failed with `Error in close.connection(con) : cannot close 'message' sink connection`.
- Inspected `artifacts/fuzz/run_20251114-210159-3188/USE_CPP20=0_USE_OPENMP=0_USE_DEVTOOLS=0_PKGBUILD_ASSUME_TOOLS=1/tests.log`, confirming the failure occurred after the embedded matrix completed 16 combos.
- Traced the error to `scripts/matrix_lib.R::run_tests()` where the embedded driver wraps stdout/stderr in sinks tied to a single connection but only partially unwinds them on exit.
- Updated `run_tests()` so it records the incoming sink depth, unwinds message/output sinks safely, and only then closes the connection; revalidated with `make test` and `make fuzz-once`.

## Locus
- **Trigger:** Running `make fuzz-once` after adding new capability probes.
- **Diagnosis:** Embedded test harness closed the log connection while it was still registered as the message sink; confirmed by reading the matrix log referenced above.
- **Remediation:** Track baseline sink depth, remove message/output sinks in `on.exit`, guard against errors, then close the connection in `scripts/matrix_lib.R`.
- **Follow-up:** None beyond keeping an eye on sink usage in future harness changes; re-running `make fuzz-once` should remain part of probe work to catch regressions.
