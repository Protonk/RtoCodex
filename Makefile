# RtoCodex automation targets: keep command names short so agents can rerun
# matrix experiments (local + container) without memorizing long invocations.
# Use `make matrix ARGS="--filter=USE_CPP20=1"` to forward flags to the
# underlying scripts/run_matrix.R harness.

MATRIX_SCRIPT := scripts/run_matrix.R
MATRIX_LIB ?= build-lib/matrix
MATRIX_ARTIFACTS ?= artifacts/matrix
CAP_ARTIFACTS ?= artifacts/caps
PROBE_DIR ?= probes
PROBE_RUNNER ?= scripts/run_probes.R

.PHONY: matrix matrix-container check clean-matrix \
	cap_sysctl_kern_boottime cap_cxx20_flags cap_openmp_flags \
	caps caps-summary

matrix:
	Rscript $(MATRIX_SCRIPT) --lib=$(MATRIX_LIB) --artifacts=$(MATRIX_ARTIFACTS) $(ARGS)

matrix-container:
	./scripts/run_container_matrix.sh $(ARGS)

check:
	Rscript -e "devtools::check()"

clean-matrix:
	rm -rf $(MATRIX_LIB) $(MATRIX_ARTIFACTS)

# Capability probes help catalogue sandbox quirks without entangling package tests.
cap_sysctl_kern_boottime:
	mkdir -p $(CAP_ARTIFACTS)
	Rscript $(PROBE_DIR)/cap_sysctl_kern_boottime.R > $(CAP_ARTIFACTS)/cap_sysctl_kern_boottime.txt

cap_cxx20_flags:
	mkdir -p $(CAP_ARTIFACTS)
	Rscript $(PROBE_DIR)/cap_cxx20_flags.R > $(CAP_ARTIFACTS)/cap_cxx20_flags.txt

cap_openmp_flags:
	mkdir -p $(CAP_ARTIFACTS)
	Rscript $(PROBE_DIR)/cap_openmp_flags.R > $(CAP_ARTIFACTS)/cap_openmp_flags.txt

caps:
	Rscript $(PROBE_RUNNER) --probes=$(PROBE_DIR) --artifacts=$(CAP_ARTIFACTS)

caps-summary: caps
	@cat $(CAP_ARTIFACTS)/caps.json
