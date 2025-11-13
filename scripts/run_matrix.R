#!/usr/bin/env Rscript
# RtoCodex matrix runner: drives R CMD INSTALL/test cycles across
# predefined compiler toggles (USE_CPP20, USE_OPENMP) so new agents
# can exercise the native code experiments with one command.
# Artifacts: per-combination logs under artifacts/matrix/<combo>/
# capturing install output, testthat results, and session metadata.

find_repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  matches <- args[grepl(file_arg, args)]
  if (length(matches) == 0) {
    return(normalizePath(getwd()))
  }
  script_path <- normalizePath(sub(file_arg, "", matches[length(matches)]))
  normalizePath(file.path(dirname(script_path), ".."))
}
setwd(find_repo_root())

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("The testthat package is required; install it before running the matrix harness.")
}

parse_args <- function() {
  defaults <- list(
    lib_root = "build-lib/matrix",
    artifacts_root = "artifacts/matrix",
    skip_tests = FALSE,
    filter = NULL
  )
  raw <- commandArgs(trailingOnly = TRUE)
  for (arg in raw) {
    if (startsWith(arg, "--lib=")) {
      defaults$lib_root <- sub("^--lib=", "", arg)
    } else if (startsWith(arg, "--artifacts=")) {
      defaults$artifacts_root <- sub("^--artifacts=", "", arg)
    } else if (identical(arg, "--skip-tests")) {
      defaults$skip_tests <- TRUE
    } else if (startsWith(arg, "--filter=")) {
      defaults$filter <- sub("^--filter=", "", arg)
    } else {
      warning(sprintf("Ignoring unrecognized argument: %s", arg))
    }
  }
  defaults
}

format_combo_name <- function(combo_row) {
  values <- as.list(combo_row)
  pieces <- sprintf("%s=%s", names(values), unlist(values, use.names = FALSE))
  paste(pieces, collapse = "_")
}

run_install <- function(lib_path, env_vars, log_path) {
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  con <- file(log_path, open = "wt")
  on.exit(close(con))
  status <- system2(
    command = "R",
    args = c("CMD", "INSTALL", "--preclean", "-l", lib_path, "."),
    stdout = con,
    stderr = con,
    env = env_vars
  )
  status
}

run_tests <- function(lib_path, log_path) {
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  con <- file(log_path, open = "wt")
  on.exit({
    sink(NULL)
    sink(NULL, type = "message")
    close(con)
  })
  sink(con)
  sink(con, type = "message")
  exit_code <- 0
  tryCatch({
    suppressPackageStartupMessages(library("RtoCodex", lib.loc = lib_path, character.only = TRUE))
    on.exit(detach("package:RtoCodex", unload = TRUE), add = TRUE)
    testthat::test_dir("tests/testthat", reporter = "summary")
  }, error = function(e) {
    exit_code <<- 1
    message("Test run failed: ", conditionMessage(e))
  })
  exit_code
}

write_session_info <- function(path, combo_name, combo_row) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  run_cfg <- function(key) {
    tryCatch(
      paste(system2("R", c("CMD", "config", key), stdout = TRUE, stderr = TRUE), collapse = "\n"),
      error = function(e) sprintf("Unable to query %s: %s", key, conditionMessage(e))
    )
  }
  info <- capture.output({
    cat("Combo:", combo_name, "\n")
    cat("Env toggles:\n")
    for (nm in names(combo_row)) {
      cat("  ", nm, "=", combo_row[[nm]], "\n", sep = "")
    }
    cat("\n## sessionInfo()\n")
    print(sessionInfo())
    cat("\n## Compiler probes\n")
    cat("CC:\n", run_cfg("CC"), "\n\n", sep = "")
    cat("CXX:\n", run_cfg("CXX"), "\n\n", sep = "")
    cat("CXX20:\n", run_cfg("CXX20"), "\n", sep = "")
  })
  writeLines(info, path)
}

main <- function() {
  opts <- parse_args()
  lib_root <- normalizePath(opts$lib_root, mustWork = FALSE)
  artifacts_root <- normalizePath(opts$artifacts_root, mustWork = FALSE)
  dir.create(lib_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(artifacts_root, recursive = TRUE, showWarnings = FALSE)

  combos <- expand.grid(USE_CPP20 = c(0, 1), USE_OPENMP = c(0, 1))
  combo_names <- vapply(seq_len(nrow(combos)), function(i) {
    format_combo_name(combos[i, , drop = FALSE])
  }, character(1))
  if (!is.null(opts$filter)) {
    keep <- grepl(opts$filter, combo_names, fixed = TRUE)
    if (!any(keep)) stop("Filter matched zero combinations.")
    combos <- combos[keep, , drop = FALSE]
    combo_names <- combo_names[keep]
  }

  results <- vector("list", length = nrow(combos))
  any_failure <- FALSE

  for (i in seq_len(nrow(combos))) {
    combo <- combos[i, , drop = FALSE]
    combo_name <- combo_names[i]
    lib_path <- file.path(lib_root, combo_name)
    artifact_dir <- file.path(artifacts_root, combo_name)
    install_log <- file.path(artifact_dir, "install.log")
    test_log <- file.path(artifact_dir, "tests.log")
    session_log <- file.path(artifact_dir, "sessionInfo.txt")

    unlink(lib_path, recursive = TRUE, force = TRUE)
    dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)

    env_vars <- c(
      sprintf("USE_CPP20=%s", combo[["USE_CPP20"]]),
      sprintf("USE_OPENMP=%s", combo[["USE_OPENMP"]])
    )

    cat("\n=== Building", combo_name, "===\n")
    install_status <- run_install(lib_path, env_vars, install_log)
    test_status <- NA_integer_
    if (install_status == 0L && !opts$skip_tests) {
      cat("Running tests for", combo_name, "...\n")
      test_status <- run_tests(lib_path, test_log)
    } else if (install_status != 0L) {
      cat("Skipping tests for", combo_name, "because build failed.\n")
      test_status <- NA_integer_
    } else {
      test_status <- NA_integer_
    }

    write_session_info(session_log, combo_name, combo)

    results[[i]] <- list(
      combo = combo_name,
      install_status = install_status,
      test_status = test_status,
      install_log = install_log,
      test_log = if (is.na(test_status)) NA_character_ else test_log
    )
    if (install_status != 0L || (!is.na(test_status) && test_status != 0L)) {
      any_failure <- TRUE
    }
  }

  summary_df <- do.call(rbind, lapply(results, as.data.frame))
  cat("\nMatrix summary:\n")
  print(summary_df, row.names = FALSE)

  if (any_failure) {
    cat("\nAt least one combination failed. Inspect artifacts under", artifacts_root, "for details.\n")
    quit(status = 1, save = "no")
  } else {
    cat("\nAll combinations succeeded. Artifacts saved under", artifacts_root, "for reference.\n")
    quit(status = 0, save = "no")
  }
}

main()
