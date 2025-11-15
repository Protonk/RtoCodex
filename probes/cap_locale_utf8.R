#!/usr/bin/env Rscript

# probes/cap_locale_utf8.R checks whether reading and writing UTF-8 text via the
# default locale round-trips multibyte characters; discrepancies may appear on
# systems running in the C locale or with limited iconv support. Log persistent
# mismatches in event narratives when behavior diverges across hosts.

sample_text <- enc2utf8("München – 東京")
probe_file <- tempfile("cap_locale_utf8_", fileext = ".txt")
on.exit(unlink(probe_file), add = TRUE)

write_result <- tryCatch({
  writeLines(sample_text, probe_file, useBytes = TRUE)
  TRUE
}, error = function(e) {
  attr(e, "message")
})

if (!isTRUE(write_result)) {
  message(write_result)
  cat("error\n")
  quit(status = 1L)
}

read_default <- tryCatch(readLines(probe_file, warn = FALSE), error = function(e) e)
match_utf8 <- function(value) {
  is.character(value) && length(value) >= 1L && identical(enc2utf8(value[[1L]]), sample_text)
}

label <- "mismatch"
if (inherits(read_default, "error")) {
  label <- "error"
  message(conditionMessage(read_default))
} else if (match_utf8(read_default)) {
  label <- "utf8"
} else {
  read_latin1 <- tryCatch(readLines(probe_file, warn = FALSE, encoding = "Latin-1"), error = function(e) e)
  if (inherits(read_latin1, "error")) {
    label <- "mismatch"
  } else if (match_utf8(read_latin1)) {
    label <- "latin1_only"
  }
}

cat(label, sep = "\n")
quit(status = if (identical(label, "error")) 1L else 0L)
