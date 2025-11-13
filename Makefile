# RtoCodex automation targets: keep command names short so agents can rerun
# matrix experiments (local + container) without memorizing long invocations.
# Use `make matrix ARGS="--filter=USE_CPP20=1"` to forward flags to the
# underlying scripts/run_matrix.R harness.

MATRIX_SCRIPT := scripts/run_matrix.R
MATRIX_LIB ?= build-lib/matrix
MATRIX_ARTIFACTS ?= artifacts/matrix

.PHONY: matrix matrix-container check clean-matrix

matrix:
	Rscript $(MATRIX_SCRIPT) --lib=$(MATRIX_LIB) --artifacts=$(MATRIX_ARTIFACTS) $(ARGS)

matrix-container:
	./scripts/run_container_matrix.sh $(ARGS)

check:
	Rscript -e "devtools::check()"

clean-matrix:
	rm -rf $(MATRIX_LIB) $(MATRIX_ARTIFACTS)
