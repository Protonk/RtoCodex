#!/usr/bin/env Rscript

# probes/cap_blas_backend.R reads extSoftVersion()['BLAS'] to classify the
# numeric backend (Accelerate/OpenBLAS/MKL/reference/other) so harness logs can
# flag when macOS vs container builds run with different BLAS implementations.

classify_backend <- function(blas_value) {
  if (!nzchar(blas_value)) {
    return("unknown")
  }
  value <- tolower(blas_value)
  if (grepl("accelerate|veclib", value)) {
    return("accelerate")
  }
  if (grepl("openblas", value)) {
    return("openblas")
  }
  if (grepl("mkl", value)) {
    return("mkl")
  }
  if (grepl("librblas|reference|atlas", value)) {
    return("reference")
  }
  "other"
}

probe_backend <- function() {
  info <- tryCatch(
    extSoftVersion(),
    error = function(e) e
  )
  if (inherits(info, "error")) {
    return(list(label = "probe_error", message = conditionMessage(info), exit = 1L))
  }
  blas <- info[["BLAS"]]
  if (is.null(blas) || is.na(blas)) {
    blas <- ""
  }
  blas <- as.character(blas)
  status <- classify_backend(blas)
  list(label = status, message = "", exit = 0L)
}

result <- probe_backend()
if (identical(result$label, "probe_error") && nzchar(result$message)) {
  writeLines(result$message, con = stderr())
}
cat(result$label, sep = "\n")
quit(status = result$exit)
