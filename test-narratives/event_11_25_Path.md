<!-- Event narrative: documents PATH update required for locating Rscript. -->
# Event 2024-11-25 PATH visibility

## Summary
- `make matrix` failed on the user's host because `Rscript` was not on PATH even though the agent sandbox succeeded.
- Adding `/usr/local/bin` (where `Rscript` lives) to the user’s PATH aligned both environments so the harness could run untouched.

## Sequence
- User reported `make matrix` failing immediately with “Rscript: command not found.”
- Agent verified local PATH (includes `/usr/local/bin`) and confirmed `which Rscript` resolves.
- User shared their PATH, revealing `/usr/local/bin` was missing.
- After the user prepended `/usr/local/bin` to PATH, `which Rscript` worked and the matrix harness behaved identically across machines.

## Locus
- **Trigger:** Running `make matrix` in the user terminal aborted because the shell could not find `Rscript`.
- **Diagnosis:** Homebrew/CRAN installed `Rscript` under `/usr/local/bin`, but the user’s PATH lacked that directory.
- **Remediation:** Updated the shell profile to include `/usr/local/bin` (e.g., `export PATH="/usr/local/bin:$PATH"`), then re-ran `make matrix` successfully.
- **Follow-up:** None; just remember to log future environment tweaks under `test-narratives/`.
