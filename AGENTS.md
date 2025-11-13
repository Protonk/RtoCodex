# RtoCodex · AGENTS guide

This repository is an R package used to probe how GPT-5 Codex behaves in different environments when working with R, compilation, and test infrastructure.

## Environments

Currently the project is tested in two environments, MacOS 13+ on Apple Silicon and OpenAI's universal Ubuntu container. The container setup script is as follows:

```
# update package lists
sudo apt-get update

# Install build tools: gcc, g++, gfortran, make
sudo apt-get install -y build-essential gfortran

# Install R and development headers
sudo apt-get install -y r-base r-base-dev

# 1) Choose a stable CRAN mirror (Posit PPM is fast and cacheable)
export CRAN="https://packagemanager.posit.co/cran/__linux__/jammy/latest"

## R packages: devtools and testthat are now required
Rscript -e "options(repos = c(CRAN='${CRAN}')); install.packages(c('devtools','testthat'), Ncpus = parallel::detectCores())"
```

Local environmental behavior and access is left for the agent (you) to determine.

## Scope and intent

- Preserve the intent of this project: it is a test harness, not a production package. Code and tests may be written specifically to surface environment-dependent behavior (e.g., compilation issues) rather than to “fix” everything.
- Every source file that you touch should start with brief, file-level comments that explain the purpose, relevant tooling, and produced artifacts; assume the audience is a motivated first-year CS student who is learning how the harness works.
- New experiments may be at any level of abstraction, from different build flags to tests of R or C++ edge cases in project code. The goal is to surface unexpected 

## How to work in this repo

- Treat the project root as an R package root. Use `devtools::load_all()`, `devtools::test()`, and `devtools::check()` to run diagnostics.
- Do not rename the package or substantially change the package structure; changes should support clearer diagnostics of environment differences.
- Never hand-edit `NAMESPACE` or the files under `man/`; regenerate them with `devtools::document()` (roxygen2) after updating roxygen comments.
- If modifying or adding tests:
  - Make the purpose explicit in test names and messages (e.g., “container vs local: compilation of X”).
  - Document any assumptions about the universal container or local toolchain in comments near the relevant code.
- If a test is failing, first decide whether the failure is part of the intended “experiment.” Only change behavior that is clearly unintended or explicitly requested.
- Prefer automation via the shared harness scripts (matrix runner, container wrapper, automation targets) so every environment can rerun the same experiments with one command.

## Interpreting matrix runs

- Run `make matrix` to exercise environment variable combinations; the harness prints the detected host profile and expected install/test outcomes for each combo before anything builds.
- Each stage log line (`- install`, `- tests`) includes a relative artifact path such as `artifacts/matrix/USE_CPP20=1_USE_OPENMP=0/install.log`; open that file or scroll the console output—both contain the same information, so use whichever is easier in your environment.
- After all combos complete, read the “Execution timeline” block or the CSV `artifacts/matrix/matrix_timeline_<timestamp>.csv` for the ordered list of stages, exit codes, and whether the result matched the heuristic expectation.
- For a tabular snapshot, load `artifacts/matrix/matrix_summary_<timestamp>.csv`; it mirrors the console summary and treats build failures and test failures as first-class signals.
- When failures differ from the printed expectations, capture the observation (and which combo/stage/log you inspected) in a test narrative so other agents can replay it without rerunning the matrix immediately.

## Creating test narratives

Sometimes failures occur due to a mix of user error, misconfiguration, and/or poor specification. If a failure is complex, only some of the solution can be captured by new tests or CI processes. This project contains narratives of these failures and solutions as `.md` files in the directory `test-narratives/`.

When you record a test or event narrative:

1. Create `test-narratives/event_MM_YY_ShortName.md` (month, year, one-word slug). Example: `event_11_25_Path.md`.
2. Begin with an HTML comment explaining the file, then add a title `# Event YYYY-MM-DD Short Title`.
3. Provide three sections:
   - **Summary:** 2‑3 sentences describing what happened and why it mattered.
   - **Sequence:** Bullet list in the order *you* observed the clues; focus on user-visible observations, not hidden reasoning.
   - **Locus:** Bullets for **Trigger**, **Diagnosis**, **Remediation**, **Follow-up** (if no action, say “None.”).
4. Keep the tone factual and reproducible so another agent can replay the steps without re-reading the chat log.
5. Revise event narratives by adding to follow-up. If revisions become dramatic, append a new 3 section summary (as detailed in Step 3) at the end. Only one new summary is needed, it can be overwritten indefinitely. 