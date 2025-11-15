#!/usr/bin/env Rscript

# probes/cap_utf8_locale.R round-trips a UTF-8 text payload through the default
# locale plus iconv() to decide whether multibyte strings can be written and
# decoded reliably; run_probes stores artifacts/caps/cap_utf8_locale.txt so
# failures can trigger follow-up event narratives about locale setup.

write_utf8_payload <- function(path, payload) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(payload, con = con, sep = "\n", useBytes = FALSE)
}

read_utf8_payload <- function(path) {
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

payload <- c(
  "ascii-sanity",
  "accent-\u00e9-check",
  "math-\u221e-symbol",
  "cjk-\u6f22\u5b57"
)
tmp <- tempfile("cap_utf8_locale_", fileext = ".txt")
on.exit(unlink(tmp, recursive = FALSE, force = TRUE), add = TRUE)

io_success <- tryCatch({
  write_utf8_payload(tmp, payload)
  TRUE
}, error = function(e) {
  writeLines(conditionMessage(e), con = stderr())
  FALSE
})

if (!io_success) {
  cat("probe_error", sep = "\n")
  quit(status = 1L)
}

read_back <- tryCatch(
  read_utf8_payload(tmp),
  error = function(e) {
    writeLines(conditionMessage(e), con = stderr())
    structure("<<read_error>>", class = "cap_utf8_error")
  }
)

if (inherits(read_back, "cap_utf8_error")) {
  cat("probe_error", sep = "\n")
  quit(status = 1L)
}

round_trip <- identical(read_back, payload)
native_conversion <- suppressWarnings(iconv(payload, from = "UTF-8", to = "", sub = NA_character_))
native_ok <- all(!is.na(native_conversion))

status <- if (round_trip && native_ok) {
  "utf8_native"
} else if (round_trip) {
  "utf8_decode_only"
} else {
  "utf8_unsupported"
}

cat(status, sep = "\n")
quit(status = 0L)
