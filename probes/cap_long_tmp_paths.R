#!/usr/bin/env Rscript

# probes/cap_long_tmp_paths.R builds >200 character nested directories under
# tempdir() to classify whether the filesystem enforces restrictive path length
# limits (supported/path_limit/error) before native build steps try similar work
# and to document unexpected failures in event narratives.

build_long_path <- function(base) {
  segments <- character()
  while (TRUE) {
    path <- do.call(file.path, as.list(c(base, segments)))
    if (nchar(path) >= 220) {
      return(path)
    }
    idx <- length(segments) + 1L
    segments <- c(segments, sprintf("cap_long_tmp_paths_seg_%02d_xxxxxxxxxxxxxxxxxx", idx))
    if (idx > 50L) {
      return(path)
    }
  }
}

ensure_dir <- function(path) {
  ok <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!ok && dir.exists(path)) {
    ok <- TRUE
  }
  ok
}

write_probe_file <- function(dir) {
  file_path <- file.path(dir, "cap_long_tmp_paths_probe.txt")
  writeLines("long path writable", file_path)
  file_path
}

classify_long_path <- function(long_dir) {
  temp_short <- file.path(tempdir(), sprintf("cap_long_tmp_paths_short_%s", Sys.getpid()))
  on.exit(unlink(temp_short, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(temp_short, recursive = TRUE, showWarnings = FALSE)
  long_created <- ensure_dir(long_dir)
  if (!long_created) {
    short_ok <- dir.exists(temp_short)
    if (short_ok) {
      return(list(label = "path_limit", message = sprintf("Failed to create %s", long_dir)))
    }
    return(list(label = "error", message = sprintf("Unable to provision temp dir: %s", temp_short)))
  }
  probe_file <- tryCatch(
    write_probe_file(long_dir),
    error = function(e) e
  )
  if (inherits(probe_file, "error")) {
    msg <- conditionMessage(probe_file)
    if (grepl("File name too long", msg, ignore.case = TRUE)) {
      return(list(label = "path_limit", message = msg))
    }
    return(list(label = "error", message = msg))
  }
  if (!file.exists(probe_file)) {
    return(list(label = "error", message = "Failed to verify probe file existence"))
  }
  list(label = "supported", message = "")
}

long_dir <- build_long_path(file.path(tempdir(), sprintf("cap_long_tmp_paths_%s", Sys.getpid())))
on.exit(unlink(long_dir, recursive = TRUE, force = TRUE), add = TRUE)
result <- classify_long_path(long_dir)
if (identical(result$label, "error") && nzchar(result$message)) {
  writeLines(result$message, con = stderr())
}
cat(result$label, sep = "\n")
quit(status = if (identical(result$label, "error")) 1L else 0L)
