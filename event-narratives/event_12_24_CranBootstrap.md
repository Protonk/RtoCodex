<!-- Event narrative: documents CRAN mirror failures leaving the subprocess helper packages uninstalled. -->
# Event 2024-12-05 Container bootstrap misses helper packages

## Summary
- The subprocess test harness failed inside the matrix container because the bootstrap treated CRAN 403 responses as ignorable warnings, so the required helper packages never installed.
- Updating the bootstrap to verify those helper packages exist now surfaces mirror outages immediately instead of letting the harness fail later.

## Sequence
- Ran `make matrix` inside the universal container; the script skipped the C++20 builds and the subprocess test harness aborted because its helper packages were missing.
- Inspected `scripts/run_container_matrix.sh` and saw it only called `install.packages()` without checking whether the requested packages were actually installed.
- Replayed the bootstrap manually and observed CRAN returning HTTP 403, leaving only warning messages in the log while the script continued.
- Added explicit checks for the helper packages after installation, forcing a clear failure message when none appear in `.libPaths()`.

## Locus
- **Trigger:** The subprocess test harness failed during `make matrix` since its helper packages were absent in the container library.
- **Diagnosis:** The bootstrap ignored CRAN download failures, so network-restricted environments never installed the required packages.
- **Remediation:** Hardened `scripts/run_container_matrix.sh` to verify package installation and stop with actionable guidance when the mirror is unreachable.
- **Follow-up:** None.
