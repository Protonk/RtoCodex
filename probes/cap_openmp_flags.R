#!/usr/bin/env Rscript

# probes/cap_openmp_flags.R inspects SHLIB_OPENMP_* flags from `R CMD config`
# to determine whether the toolchain exposes usable OpenMP compilation options.

trim_collapse <- function(x) {
  if (!length(x)) {
    return("")
  }
  trimws(paste(x, collapse = "\n"))
}

run_config <- function(key) {
  err_file <- tempfile(paste0("openmp_", key, "_stderr_"))
  on.exit(unlink(err_file), add = TRUE)
  out <- tryCatch(
    system2("R", c("CMD", "config", key), stdout = TRUE, stderr = err_file),
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

keys <- c("SHLIB_OPENMP_CFLAGS", "SHLIB_OPENMP_CXXFLAGS", "SHLIB_OPENMP_LDFLAGS")
probes <- lapply(keys, run_config)
normalize_for_missing <- function(res) {
  if (res$status == 0L) {
    return(res)
  }
  msg <- tolower(paste(res$stderr, res$text))
  if (grepl("no information|not .*defined", msg)) {
    res$status <- 0L
    res$text <- ""
    res$stderr <- ""
  }
  res
}
probes <- lapply(probes, normalize_for_missing)
statuses <- vapply(probes, function(x) x$status, integer(1))
if (any(statuses != 0L)) {
  msg <- paste(vapply(probes, function(x) x$stderr, character(1)), collapse = "\n")
  if (nzchar(msg)) writeLines(msg, con = stderr())
  cat("error", sep = "\n")
  quit(status = 1L)
}
values <- vapply(probes, function(x) x$text, character(1))
if (any(nzchar(values))) {
  cat("available", sep = "\n")
  quit(status = 0L)
}
cat("missing", sep = "\n")
quit(status = 0L)
