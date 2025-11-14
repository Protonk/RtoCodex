#!/usr/bin/env Rscript

# probes/cap_cxx20_flags.R inspects `R CMD config CXX20` output to determine if
# the toolchain advertises C++20 flags (supported/missing/error classification).

trim_collapse <- function(x) {
  if (!length(x)) {
    return("")
  }
  trimws(paste(x, collapse = "\n"))
}

run_config <- function(key) {
  err_file <- tempfile("cxx20_stderr_")
  on.exit(unlink(err_file), add = TRUE)
  out <- tryCatch(
    system2("R", c("CMD", "config", key), stdout = TRUE, stderr = err_file),
    error = function(e) {
      structure(character(), status = 127L, stderr = conditionMessage(e))
    }
  )
  status <- attr(out, "status")
  if (is.null(status)) {
    status <- 0L
  }
  stderr_text <- ""
  if (!is.null(attr(out, "stderr"))) {
    stderr_text <- attr(out, "stderr")
  } else if (file.exists(err_file)) {
    stderr_lines <- readLines(err_file, warn = FALSE)
    stderr_text <- trim_collapse(stderr_lines)
  }
  list(text = trim_collapse(out), status = status, stderr = stderr_text)
}

classify <- function(result) {
  if (result$status != 0L) {
    return(list(label = "error", exit = result$status))
  }
  text <- result$text
  if (nzchar(text) && grepl("20", text)) {
    return(list(label = "supported", exit = 0L))
  }
  list(label = "missing", exit = 0L)
}

result <- run_config("CXX20")
label <- classify(result)
if (identical(label$label, "error")) {
  msg <- if (nzchar(result$stderr)) result$stderr else result$text
  if (nzchar(msg)) {
    writeLines(msg, con = stderr())
  }
}
cat(label$label, sep = "\n")
quit(status = if (identical(label$label, "error")) 1L else 0L)
