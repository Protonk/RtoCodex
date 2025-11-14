# RtoCodex automation targets: keep command names short so agents can rerun
# matrix experiments (local + container) without memorizing long invocations.
# Use `make matrix ARGS="--filter=USE_CPP20=1"` to forward flags to the
# underlying scripts/run_matrix.R harness.

MATRIX_SCRIPT := scripts/run_matrix.R
MATRIX_LIB ?= build-lib/matrix
MATRIX_ARTIFACTS ?= artifacts/matrix
TEST_LIB ?= build-lib/unit
FUZZ_SCRIPT := scripts/run_fuzz.R
FUZZ_LIB ?= build-lib/fuzz
FUZZ_ARTIFACTS ?= artifacts/fuzz
CAP_ARTIFACTS ?= artifacts/caps
PROBE_DIR ?= probes
PROBE_RUNNER ?= scripts/run_probes.R

.PHONY: matrix matrix-container check clean-matrix fuzz fuzz-once fuzz-clean \
	test \
	cap_sysctl_kern_boottime cap_cxx20_flags cap_openmp_flags \
	caps caps-summary

matrix:
	Rscript $(MATRIX_SCRIPT) --lib=$(MATRIX_LIB) --artifacts=$(MATRIX_ARTIFACTS) $(ARGS)

matrix-container:
	./scripts/run_container_matrix.sh $(ARGS)

test:
	mkdir -p $(TEST_LIB)
	R CMD INSTALL --preclean -l $(TEST_LIB) .
	Rscript tests/run_unit_tests.R --lib=$(TEST_LIB) --driver=cli

check:
	R CMD check --no-manual RtoCodex

clean-matrix:
	rm -rf $(MATRIX_LIB) $(MATRIX_ARTIFACTS)

fuzz:
	Rscript $(FUZZ_SCRIPT) --lib=$(FUZZ_LIB) --artifacts=$(FUZZ_ARTIFACTS) $(ARGS)

fuzz-once:
	Rscript $(FUZZ_SCRIPT) --lib=$(FUZZ_LIB) --artifacts=$(FUZZ_ARTIFACTS) --n=1 $(ARGS)

fuzz-clean:
	rm -rf $(FUZZ_LIB) $(FUZZ_ARTIFACTS)

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
