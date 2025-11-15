#!/usr/bin/env Rscript

# probes/cap_fortran_shlib.R compiles a tiny Fortran subroutine with
# `R CMD SHLIB` to label whether the host toolchain exposes a working Fortran
# compiler; run_probes writes artifacts/caps/cap_fortran_shlib.txt so harnesses
# can skip Fortran-dependent builds or log mismatches in event narratives.

trim_collapse <- function(x) {
  if (!length(x)) {
    return("")
  }
  trimws(paste(x, collapse = "\n"))
}

with_dir <- function(dir, code) {
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(dir)
  eval.parent(substitute(code))
}

write_fortran_source <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, "cap_fortran_probe.f90")
  lines <- c(
    "subroutine cap_probe(result)",
    "  double precision result",
    "  result = 42.0d0",
    "end subroutine cap_probe"
  )
  writeLines(lines, path)
  path
}

compile_fortran <- function(workdir) {
  src <- write_fortran_source(workdir)
  err_file <- tempfile("cap_fortran_shlib_stderr_")
  on.exit(unlink(err_file, recursive = FALSE, force = TRUE), add = TRUE)
  output <- with_dir(workdir, tryCatch(
    system2("R", c("CMD", "SHLIB", basename(src)), stdout = TRUE, stderr = err_file),
    error = function(e) {
      structure(character(), status = 127L, stderr = conditionMessage(e))
    }
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  stderr_lines <- if (!is.null(attr(output, "stderr"))) {
    attr(output, "stderr")
  } else if (file.exists(err_file)) {
    readLines(err_file, warn = FALSE)
  } else {
    character()
  }
  list(
    status = status,
    stdout = output,
    stderr = stderr_lines,
    src = src,
    workdir = workdir
  )
}

classify_fortran_status <- function(run) {
  lib_name <- paste0(tools::file_path_sans_ext(basename(run$src)), .Platform$dynlib.ext)
  lib_path <- file.path(run$workdir, lib_name)
  log_text <- trim_collapse(c(run$stderr, run$stdout))
  if (run$status == 0L && file.exists(lib_path)) {
    return(list(label = "available", message = ""))
  }
  lower <- tolower(log_text)
  missing_patterns <- c("gfortran: not found", "command not found", "no such file or directory")
  missing <- FALSE
  if (grepl("no fortran compiler", lower) || grepl("fortran compiler.*not.*available", lower)) {
    missing <- TRUE
  } else if (grepl("gfortran", lower)) {
    missing <- any(vapply(missing_patterns, grepl, logical(1), x = lower, fixed = FALSE))
  }
  if (missing) {
    return(list(label = "missing", message = ""))
  }
  list(label = "error", message = log_text)
}

workdir <- tempfile("cap_fortran_shlib_")
dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(workdir, recursive = TRUE, force = TRUE), add = TRUE)

result <- compile_fortran(workdir)
classification <- classify_fortran_status(result)
if (identical(classification$label, "error") && nzchar(classification$message)) {
  writeLines(classification$message, con = stderr())
}
cat(classification$label, sep = "\n")
quit(status = if (identical(classification$label, "error")) 1L else 0L)
