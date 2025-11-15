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
	cap_fortran_shlib cap_utf8_locale cap_temp_symlink_exec \
	cap_pkg_config_path cap_long_tmp_paths cap_case_sensitive_tmpfs \
	cap_unicode_filenames cap_open_files_limit cap_blas_backend \
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

cap_fortran_shlib:
	mkdir -p $(CAP_ARTIFACTS)
	Rscript $(PROBE_DIR)/cap_fortran_shlib.R > $(CAP_ARTIFACTS)/cap_fortran_shlib.txt

cap_utf8_locale:
	mkdir -p $(CAP_ARTIFACTS)
	Rscript $(PROBE_DIR)/cap_utf8_locale.R > $(CAP_ARTIFACTS)/cap_utf8_locale.txt

cap_temp_symlink_exec:
	mkdir -p $(CAP_ARTIFACTS)
	Rscript $(PROBE_DIR)/cap_temp_symlink_exec.R > $(CAP_ARTIFACTS)/cap_temp_symlink_exec.txt

cap_pkg_config_path:
	mkdir -p $(CAP_ARTIFACTS)
	Rscript $(PROBE_DIR)/cap_pkg_config_path.R > $(CAP_ARTIFACTS)/cap_pkg_config_path.txt

cap_long_tmp_paths:
	mkdir -p $(CAP_ARTIFACTS)
	Rscript $(PROBE_DIR)/cap_long_tmp_paths.R > $(CAP_ARTIFACTS)/cap_long_tmp_paths.txt

cap_case_sensitive_tmpfs:
	mkdir -p $(CAP_ARTIFACTS)
	Rscript $(PROBE_DIR)/cap_case_sensitive_tmpfs.R > $(CAP_ARTIFACTS)/cap_case_sensitive_tmpfs.txt

cap_unicode_filenames:
	mkdir -p $(CAP_ARTIFACTS)
	Rscript $(PROBE_DIR)/cap_unicode_filenames.R > $(CAP_ARTIFACTS)/cap_unicode_filenames.txt

cap_open_files_limit:
	mkdir -p $(CAP_ARTIFACTS)
	Rscript $(PROBE_DIR)/cap_open_files_limit.R > $(CAP_ARTIFACTS)/cap_open_files_limit.txt

cap_blas_backend:
	mkdir -p $(CAP_ARTIFACTS)
	Rscript $(PROBE_DIR)/cap_blas_backend.R > $(CAP_ARTIFACTS)/cap_blas_backend.txt

caps:
	Rscript $(PROBE_RUNNER) --probes=$(PROBE_DIR) --artifacts=$(CAP_ARTIFACTS)

caps-summary: caps
	@cat $(CAP_ARTIFACTS)/caps.json
