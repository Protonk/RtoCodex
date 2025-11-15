#!/usr/bin/env Rscript

# probes/cap_pkg_config_path.R checks whether `pkg-config` is callable on PATH,
# then points it at a synthetic .pc file to classify the environment as
# available/missing/error for builds that rely on pkg-config metadata so
# harnesses can flag surprising gaps in event narratives.

trim_collapse <- function(x) {
  if (!length(x)) {
    return("")
  }
  trimws(paste(x, collapse = "\n"))
}

write_stub_pc <- function(dir, name) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, sprintf("%s.pc", name))
  stub <- c(
    sprintf("prefix=%s", tempdir()),
    "exec_prefix=${prefix}",
    "libdir=${exec_prefix}/lib",
    "includedir=${prefix}/include",
    "",
    sprintf("Name: %s", name),
    "Description: Capability probe stub for pkg-config detection",
    "Version: 1.0.0",
    "Cflags: -I${includedir} -DUSE_CAP_PROBE",
    "Libs: -L${libdir} -lcap_probe"
  )
  writeLines(stub, path)
  path
}

run_pkg_config <- function(pc_dir, name) {
  err_file <- tempfile("cap_pkg_config_err_")
  on.exit(unlink(err_file, recursive = FALSE, force = TRUE), add = TRUE)
  output <- tryCatch(
    system2(
      "pkg-config",
      c("--cflags", name),
      stdout = TRUE,
      stderr = err_file,
      env = sprintf("PKG_CONFIG_PATH=%s", pc_dir)
    ),
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

classify_pkg_config <- function(run) {
  if (run$status == 0L && nzchar(run$stdout) && grepl("-I", run$stdout, fixed = TRUE)) {
    return(list(label = "available", message = ""))
  }
  missing_patterns <- c("not found", "no such file", "pkg-config: not found")
  stderr_lower <- tolower(run$stderr)
  if (run$status == 127L || any(vapply(missing_patterns, grepl, logical(1), x = stderr_lower, fixed = TRUE))) {
    return(list(label = "missing", message = run$stderr))
  }
  list(label = "error", message = trim_collapse(c(run$stderr, run$stdout)))
}

probe_name <- "cap_pkg_config_path_stub"
pc_dir <- tempfile("cap_pkg_config_path_dir_")
on.exit(unlink(pc_dir, recursive = TRUE, force = TRUE), add = TRUE)
stub_file <- write_stub_pc(pc_dir, probe_name)
if (!file.exists(stub_file)) {
  stop(sprintf("Unable to create stub pkg-config file at %s", stub_file))
}

result <- classify_pkg_config(run_pkg_config(pc_dir, probe_name))
if (identical(result$label, "error") && nzchar(result$message)) {
  writeLines(result$message, con = stderr())
}
cat(result$label, sep = "\n")
quit(status = if (identical(result$label, "error")) 1L else 0L)
