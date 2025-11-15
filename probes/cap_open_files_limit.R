#!/usr/bin/env Rscript

# probes/cap_open_files_limit.R queries `ulimit -n` via POSIX sh so harnesses can
# predict whether macOS shells (low descriptors) or containers (high/unlimited)
# will hit open-file ceilings during builds; it classifies the reported limit as
# low/medium/high/unlimited or probe_error on failures.

trim_collapse <- function(x) {
  if (!length(x)) {
    return("")
  }
  trimws(paste(x, collapse = "\n"))
}

run_ulimit <- function() {
  err_file <- tempfile("cap_open_files_err_")
  on.exit(unlink(err_file, recursive = FALSE, force = TRUE), add = TRUE)
  output <- tryCatch(
    system2("sh", c("-c", "ulimit -n"), stdout = TRUE, stderr = err_file),
    error = function(e) {
      structure(character(), status = 127L, stderr = conditionMessage(e))
    }
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  stderr_text <- if (!is.null(attr(output, "stderr"))) {
    attr(output, "stderr")
  } else if (file.exists(err_file)) {
    trim_collapse(readLines(err_file, warn = FALSE))
  } else {
    ""
  }
  list(status = status, stdout = trim_collapse(output), stderr = stderr_text)
}

parse_limit <- function(result) {
  if (result$status != 0L) {
    return(list(label = "probe_error", message = result$stderr, exit = 1L))
  }
  text <- result$stdout
  if (!nzchar(text)) {
    return(list(label = "probe_error", message = "ulimit -n returned empty output", exit = 1L))
  }
  if (tolower(text) == "unlimited") {
    return(list(label = "unlimited", message = "", exit = 0L))
  }
  value <- suppressWarnings(as.numeric(text))
  if (!is.finite(value)) {
    return(list(label = "probe_error", message = sprintf("unable to parse ulimit value: %s", text), exit = 1L))
  }
  if (value < 512) {
    return(list(label = "low", message = "", exit = 0L))
  }
  if (value < 2048) {
    return(list(label = "medium", message = "", exit = 0L))
  }
  list(label = "high", message = "", exit = 0L)
}

result <- parse_limit(run_ulimit())
if (identical(result$label, "probe_error") && nzchar(result$message)) {
  writeLines(result$message, con = stderr())
}
cat(result$label, sep = "\n")
quit(status = result$exit)
