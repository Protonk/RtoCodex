#!/usr/bin/env Rscript

# probes/cap_fortran_config.R inspects `R CMD config` for Fortran compiler
# entries to detect when environments ship gfortran (common on Linux) versus
# stubbed or missing toolchains (often on fresh macOS installs). Record oddball
# outputs in an event narrative alongside the fuzz run that triggered them.

trim_collapse <- function(x) {
  if (!length(x)) {
    return("")
  }
  trimws(paste(x, collapse = " \n"))
}

run_config <- function(key) {
  err_file <- tempfile(paste0("cap_fortran_", key, "_stderr_"))
  on.exit(unlink(err_file), add = TRUE)
  out <- tryCatch(
    suppressWarnings(system2("R", c("CMD", "config", key), stdout = TRUE, stderr = err_file)),
    error = function(e) {
      structure(character(), status = 127L, stderr = conditionMessage(e))
    }
  )
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  stderr_text <- if (!is.null(attr(out, "stderr"))) {
    attr(out, "stderr")
  } else if (file.exists(err_file)) {
    trim_collapse(readLines(err_file, warn = FALSE))
  } else {
    ""
  }
  list(text = trim_collapse(out), status = status, stderr = stderr_text)
}

normalize_token <- function(text) {
  value <- tolower(trimws(text))
  value <- gsub("\"", "", value, fixed = TRUE)
  value <- gsub("'", "", value, fixed = TRUE)
  value
}

classify_fortran <- function(fc, f77) {
  if (fc$status != 0L && f77$status != 0L) {
    return(list(label = "error", exit = min(fc$status, f77$status), stderr = trimws(paste(fc$stderr, f77$stderr))))
  }
  tokens <- c(fc$text, f77$text)
  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens)) {
    return(list(label = "missing", exit = 0L, stderr = trimws(paste(fc$stderr, f77$stderr))))
  }
  normalized <- normalize_token(tokens[[1L]])
  if (!nzchar(normalized) || normalized %in% c("none", "false", "missing", "n/a")) {
    return(list(label = "missing", exit = 0L, stderr = trimws(paste(fc$stderr, f77$stderr))))
  }
  list(label = "available", exit = 0L, stderr = trimws(paste(fc$stderr, f77$stderr)))
}

fc <- run_config("FC")
f77 <- run_config("F77")
label <- classify_fortran(fc, f77)

if (label$exit != 0L && nzchar(label$stderr)) {
  message(label$stderr)
}

cat(label$label, sep = "\n")
quit(status = if (identical(label$label, "error")) 1L else 0L)
