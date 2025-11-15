#!/usr/bin/env Rscript

# probes/cap_case_sensitive_tmpfs.R determines whether the temporary filesystem
# treats paths case-sensitively or case-insensitively so build scripts can adapt
# to macOS (case-insensitive) vs Linux/container (case-sensitive) behavior and
# so diagnostics can log mismatches in event narratives.

write_probe_file <- function(dir, name) {
  path <- file.path(dir, name)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("case probe", path)
  path
}

classify_case_behavior <- function() {
  base <- file.path(tempdir(), sprintf("cap_case_sensitivity_%s", Sys.getpid()))
  on.exit(unlink(base, recursive = TRUE, force = TRUE), add = TRUE)
  canonical_name <- "CaseProbe.txt"
  variant_name <- "caseprobe.txt"
  canonical_path <- write_probe_file(base, canonical_name)
  if (!file.exists(canonical_path)) {
    return(list(label = "error", message = sprintf("Failed to create %s", canonical_path)))
  }
  variant_path <- file.path(base, variant_name)
  if (file.exists(variant_path)) {
    return(list(label = "case_insensitive", message = ""))
  }
  list(label = "case_sensitive", message = "")
}

result <- classify_case_behavior()
if (identical(result$label, "error") && nzchar(result$message)) {
  writeLines(result$message, con = stderr())
}
cat(result$label, sep = "\n")
quit(status = if (identical(result$label, "error")) 1L else 0L)
