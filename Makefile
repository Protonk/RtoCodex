# RtoCodex automation targets: keep command names short so agents can rerun
# fuzz campaigns without memorizing long invocations. Pass `ARGS="..."` to
# forward additional options directly to the harness scripts.

TEST_LIB ?= build-lib/unit
FUZZ_SCRIPT := scripts/run_fuzz.R
FUZZ_LIB ?= build-lib/fuzz
FUZZ_ARTIFACTS ?= artifacts/fuzz
CAP_ARTIFACTS ?= artifacts/caps
PROBE_DIR ?= probes
PROBE_RUNNER ?= scripts/run_probes.R

.PHONY: fuzz-all matrix check fuzz fuzz-once fuzz-clean \
        test \
        cap_sysctl_kern_boottime cap_cxx20_flags cap_openmp_flags \
        cap_locale_utf8 cap_fs_case_sensitivity cap_fortran_config \
        caps caps-summary

fuzz-all:
	Rscript $(FUZZ_SCRIPT) --lib=$(FUZZ_LIB) --artifacts=$(FUZZ_ARTIFACTS) --n=9999 $(ARGS)

matrix: fuzz-all

test:
	mkdir -p $(TEST_LIB)
	R CMD INSTALL --preclean -l $(TEST_LIB) .
	Rscript tests/run_unit_tests.R --lib=$(TEST_LIB) --driver=cli

check:
	R CMD check --no-manual RtoCodex

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

cap_locale_utf8:
	mkdir -p $(CAP_ARTIFACTS)
	Rscript $(PROBE_DIR)/cap_locale_utf8.R > $(CAP_ARTIFACTS)/cap_locale_utf8.txt

cap_fs_case_sensitivity:
	mkdir -p $(CAP_ARTIFACTS)
	Rscript $(PROBE_DIR)/cap_fs_case_sensitivity.R > $(CAP_ARTIFACTS)/cap_fs_case_sensitivity.txt

cap_fortran_config:
	mkdir -p $(CAP_ARTIFACTS)
	Rscript $(PROBE_DIR)/cap_fortran_config.R > $(CAP_ARTIFACTS)/cap_fortran_config.txt

caps:
	Rscript $(PROBE_RUNNER) --probes=$(PROBE_DIR) --artifacts=$(CAP_ARTIFACTS)

caps-summary: caps
	@cat $(CAP_ARTIFACTS)/caps.json
