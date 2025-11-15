#!/usr/bin/env Rscript

# probes/cap_fs_case_sensitivity.R distinguishes between case-sensitive and
# case-insensitive temporary filesystems by writing a sentinel file and querying
# path variants; APFS (default macOS) often collapses case while Linux ext4 does
# not. Capture unexpected flips in event narratives alongside the fuzz logs.

target_dir <- tempfile("cap_case_fs_")
if (!dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)) {
  cat("error\n")
  quit(status = 1L)
}
on.exit(unlink(target_dir, recursive = TRUE, force = TRUE), add = TRUE)

probe_name <- "CaseProbe.txt"
probe_path <- file.path(target_dir, probe_name)
write_ok <- tryCatch({
  writeLines("case probe", probe_path)
  TRUE
}, error = function(e) {
  message(conditionMessage(e))
  FALSE
})

if (!write_ok || !file.exists(probe_path)) {
  cat("error\n")
  quit(status = 1L)
}

lower_exists <- file.exists(file.path(target_dir, tolower(probe_name)))
upper_exists <- file.exists(file.path(target_dir, toupper(probe_name)))

label <- if (lower_exists || upper_exists) "case_insensitive" else "case_sensitive"
cat(label, sep = "\n")
quit(status = 0L)
