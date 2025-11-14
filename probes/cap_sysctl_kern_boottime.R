#!/usr/bin/env Rscript

# probes/cap_sysctl_kern_boottime.R probes ps::ps_boot_time() to classify whether
# this host can read kern.boottime; results are capability hints for harness runs.

write_status <- function(value) {
  cat(value, sep = "\n")
}

is_expected_block <- function(err) {
  msg <- conditionMessage(err) %||% ""
  msg_lower <- tolower(msg)
  has_kern <- grepl("kern\\.boottime", msg_lower, fixed = FALSE)
  perm_signals <- c("operation not permitted", "permission denied", "eperm", "eacces")
  has_perm <- any(vapply(perm_signals, grepl, logical(1), x = msg_lower, fixed = TRUE))
  has_perm && (has_kern || inherits(err, "ps_error"))
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0) rhs else lhs
}

exit_code <- tryCatch({
  ps::ps_boot_time()
  write_status("supported")
  0L
}, error = function(err) {
  message_text <- conditionMessage(err)
  if (is_expected_block(err)) {
    write_status("blocked_expected")
    0L
  } else {
    write_status("blocked_unexpected")
    if (!is.null(message_text) && nzchar(message_text)) {
      writeLines(message_text, con = stderr())
    }
    1L
  }
})

quit(status = exit_code)
