#!/usr/bin/env Rscript

# probes/cap_unicode_filenames.R creates a UTF-8 filename containing accented
# characters to learn whether the filesystem preserves NFC byte sequences,
# normalizes to NFD (macOS default), or rejects such names outright due to the
# current locale; harnesses use this to predict when Unicode paths will fail.

write_probe_file <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  file_conn <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(file_conn), add = TRUE)
  writeLines("unicode filename probe", con = file_conn, sep = "\n", useBytes = TRUE)
  invisible(TRUE)
}

classify_normalization <- function(actual, expected_nfc, expected_nfd) {
  if (!length(actual) || !nzchar(actual)) {
    return("probe_error")
  }
  if (identical(actual, expected_nfc)) {
    return("nfc_preserved")
  }
  if (identical(actual, expected_nfd)) {
    return("nfd_normalized")
  }
  "unknown_normalization"
}

probe_unicode_filename <- function() {
  workdir <- tempfile("cap_unicode_filenames_")
  dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(workdir, recursive = TRUE, force = TRUE), add = TRUE)
  prefix <- sprintf("cap_unicode_file_%s_", Sys.getpid())
  nfc_suffix <- "\u00e9"
  nfd_suffix <- "\u0065\u0301"
  nfc_basename <- paste0(prefix, nfc_suffix)
  nfd_basename <- paste0(prefix, nfd_suffix)
  nfc_filename <- paste0(nfc_basename, ".txt")
  nfd_filename <- paste0(nfd_basename, ".txt")
  nfc_path <- file.path(workdir, nfc_filename)
  created <- tryCatch(
    {
      write_probe_file(nfc_path)
      TRUE
    },
    error = function(e) e
  )
  if (!isTRUE(created)) {
    msg <- conditionMessage(created)
    if (grepl("unable to translate|invalid argument|not representable", msg, ignore.case = TRUE)) {
      return(list(label = "path_encoding_blocked", message = msg, exit = 0L))
    }
    return(list(label = "probe_error", message = msg %||% "failed to create unicode filename", exit = 1L))
  }
  entries <- list.files(workdir)
  target <- entries[startsWith(entries, prefix)]
  if (!length(target)) {
    return(list(label = "probe_error", message = "No unicode file discovered after creation", exit = 1L))
  }
  status <- classify_normalization(target[[1]], nfc_filename, nfd_filename)
  list(label = status, message = "", exit = if (identical(status, "probe_error")) 1L else 0L)
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0) rhs else lhs
}

result <- probe_unicode_filename()
if (nzchar(result$message) && identical(result$label, "probe_error")) {
  writeLines(result$message, con = stderr())
}
cat(result$label, sep = "\n")
quit(status = result$exit)
