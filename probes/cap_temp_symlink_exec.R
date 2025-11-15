#!/usr/bin/env Rscript

# probes/cap_temp_symlink_exec.R checks whether tempdir() allows creating and
# executing symlinked scripts, a common sandbox variance between macOS and
# Linux. run_probes records artifacts/caps/cap_temp_symlink_exec.txt so agents
# can skip flaky tests or log follow-up event narratives when symlinks fail.

trim_collapse <- function(x) {
  if (!length(x)) {
    return("")
  }
  trimws(paste(x, collapse = "\n"))
}

write_probe_script <- function(path) {
  lines <- c("#!/usr/bin/env sh", "echo temp_symlink_exec_ok")
  writeLines(lines, path)
  Sys.chmod(path, mode = "700")
}

create_symlink <- function(target, link) {
  if (file.exists(link)) {
    unlink(link, recursive = FALSE, force = TRUE)
  }
  tryCatch({
    isTRUE(file.symlink(target, link)) && file.exists(link)
  }, warning = function(w) {
    FALSE
  }, error = function(e) {
    FALSE
  })
}

run_symlinked_script <- function(path) {
  err_file <- tempfile("cap_temp_symlink_exec_stderr_")
  on.exit(unlink(err_file, recursive = FALSE, force = TRUE), add = TRUE)
  output <- tryCatch(
    system2(path, stdout = TRUE, stderr = err_file),
    error = function(e) {
      structure(character(), status = 127L, stderr = conditionMessage(e))
    }
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  stderr_lines <- if (!is.null(attr(output, "stderr"))) {
    attr(output, "stderr")
  } else if (file.exists(err_file)) {
    readLines(err_file, warn = FALSE)
  } else {
    character()
  }
  list(status = status, stdout = output, stderr = stderr_lines)
}

workdir <- tempfile("cap_temp_symlink_exec_")
dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(workdir, recursive = TRUE, force = TRUE), add = TRUE)

script_path <- file.path(workdir, "probe.sh")
write_probe_script(script_path)
link_dir <- file.path(workdir, "links", "nested")
dir.create(link_dir, recursive = TRUE, showWarnings = FALSE)
link_path <- file.path(link_dir, paste0("symlink_", Sys.getpid(), ".sh"))

if (!create_symlink(script_path, link_path)) {
  cat("symlink_blocked", sep = "\n")
  quit(status = 0L)
}

result <- run_symlinked_script(link_path)
stdout_text <- trimws(paste(result$stdout, collapse = "\n"))
stderr_text <- trim_collapse(result$stderr)

status <- if (result$status == 0L && grepl("temp_symlink_exec_ok", stdout_text, fixed = TRUE)) {
  "supported"
} else if (result$status %in% c(13L, 126L) || grepl("permission denied|operation not permitted", tolower(stderr_text))) {
  "noexec"
} else {
  "error"
}

if (identical(status, "error") && nzchar(stderr_text)) {
  writeLines(stderr_text, con = stderr())
}

cat(status, sep = "\n")
quit(status = if (identical(status, "error")) 1L else 0L)
