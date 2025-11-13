#!/usr/bin/env bash
# Container matrix wrapper: launches OpenAI's universal Ubuntu image (or any
# docker/podman image you specify) and runs scripts/run_matrix.R inside it to
# reproduce macOS vs. container compiler behavior from a clean toolchain.
# Artifacts still land under artifacts/matrix/ via the shared volume.

set -euo pipefail

IMAGE="ghcr.io/openai/universal:latest"
MATRIX_ARGS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --image)
      IMAGE="$2"
      shift 2
      ;;
    --)
      shift
      MATRIX_ARGS+=("$@")
      break
      ;;
    *)
      MATRIX_ARGS+=("$1")
      shift
      ;;
  esac
done

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

matrix_args_escaped=""
if [[ ${#MATRIX_ARGS[@]} -gt 0 ]]; then
  printf -v matrix_args_escaped ' %q' "${MATRIX_ARGS[@]}"
fi

read -r -d '' container_cmd <<'INNER'
set -euo pipefail
if command -v sudo >/dev/null 2>&1; then
  SUDO="sudo"
else
  SUDO=""
fi
$SUDO apt-get update
$SUDO apt-get install -y build-essential gfortran r-base r-base-dev
export CRAN="https://packagemanager.posit.co/cran/__linux__/jammy/latest"
Rscript - <<'RSCRIPT'
repos <- Sys.getenv("CRAN")
if (!nzchar(repos)) {
  stop("CRAN repository mirror was not configured via the CRAN environment variable.")
}
options(repos = c(CRAN = repos))
pkgs <- c("devtools", "testthat")
message("Ensuring required R packages are installed: ", paste(pkgs, collapse = ", "))
installed <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
if (!all(installed)) {
  cores <- parallel::detectCores()
  if (is.na(cores) || cores < 1L) {
    cores <- 1L
  }
  install.packages(pkgs[!installed], Ncpus = cores)
}
installed <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
if (all(installed)) {
  message("Verified required R packages: ", paste(pkgs, collapse = ", "))
} else {
  message(
    "Failed to install required R packages: ",
    paste(pkgs[!installed], collapse = ", "),
    "\nThe setup script cannot proceed without network access to the specified CRAN mirror."
  )
  quit(status = 1L)
}
RSCRIPT
Rscript scripts/run_matrix.R __MATRIX_ARGS__
INNER

container_cmd=${container_cmd//__MATRIX_ARGS__/$matrix_args_escaped}

if ! command -v docker >/dev/null 2>&1 && ! command -v podman >/dev/null 2>&1; then
  echo "Neither docker nor podman is available; install one to run the container harness." >&2
  exit 1
fi

runtime="docker"
if ! command -v docker >/dev/null 2>&1; then
  runtime="podman"
fi

$runtime run --rm -it \
  -v "$repo_root:/workspace" \
  -w /workspace \
  "$IMAGE" \
  bash -lc "$container_cmd"
