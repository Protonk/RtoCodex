#!/usr/bin/env Rscript
# RtoCodex matrix runner: exercises the fixed USE_CPP20/USE_OPENMP grid,
# captures install/test logs, and mirrors the host-detection heuristics so
# agents can compare expectation vs observation in artifacts/matrix/*.csv.

locate_matrix_lib <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  matches <- args[grepl(file_arg, args)]
  if (length(matches) > 0) {
    script_path <- normalizePath(sub(file_arg, "", matches[length(matches)]))
    return(file.path(dirname(script_path), "matrix_lib.R"))
  }
  candidate <- file.path(getwd(), "scripts", "matrix_lib.R")
  normalizePath(candidate, mustWork = TRUE)
}

source(locate_matrix_lib())

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

main <- function() {
  opts <- parse_args()
  lib_root <- normalizePath(opts$lib_root, mustWork = FALSE)
  artifacts_root <- normalizePath(opts$artifacts_root, mustWork = FALSE)
  dir.create(lib_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(artifacts_root, recursive = TRUE, showWarnings = FALSE)

  profile <- build_env_profile()
  summarize_profile(profile)
  options(
    RtoCodex.sandbox_blocks_kern_boottime = isTRUE(profile$sandbox$kern_boottime$blocked),
    RtoCodex.sandbox_kern_boottime_message = profile$sandbox$kern_boottime$message
  )

  combos <- build_matrix()
  combo_names <- vapply(seq_len(nrow(combos)), function(i) {
    format_combo_name(combos[i, , drop = FALSE])
  }, character(1))
  if (!is.null(opts$filter)) {
    keep <- grepl(opts$filter, combo_names, fixed = TRUE)
    if (!any(keep)) stop("Filter matched zero combinations.")
    combos <- combos[keep, , drop = FALSE]
    combo_names <- combo_names[keep]
  }

  log_line("INFO", "Running %d matrix combinations (skip_tests=%s).", nrow(combos), opts$skip_tests)

  timeline <- event_store()
  combo_results <- vector("list", length = nrow(combos))
  any_failure <- FALSE
  run_tag <- format(Sys.time(), "%Y%m%d-%H%M%S")

  for (i in seq_len(nrow(combos))) {
    combo <- combos[i, , drop = FALSE]
    combo_vals <- as.list(combo[1, , drop = FALSE])
    combo_name <- combo_names[i]
    expectations <- derive_expectations(combo_vals, profile, opts$skip_tests)

    log_line("INFO", "")
    rule()
    log_line("INFO", "[%d/%d] %s", i, nrow(combos), combo_name)
    log_line("INFO", "  Expected install: %s (%s)", toupper(expectations$install$label), expectations$install$notes)
    log_line("INFO", "  Expected tests  : %s (%s)", toupper(expectations$tests$label), expectations$tests$notes)

    lib_path <- file.path(lib_root, combo_name)
    artifact_dir <- file.path(artifacts_root, combo_name)
    install_log <- file.path(artifact_dir, "install.log")
    test_log <- file.path(artifact_dir, "tests.log")
    session_log <- file.path(artifact_dir, "sessionInfo.txt")
    install_log_rel <- rel_path(install_log)
    test_log_rel <- rel_path(test_log)

    unlink(lib_path, recursive = TRUE, force = TRUE)
    dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)

    env_vars <- c(
      sprintf("USE_CPP20=%s", combo_vals[["USE_CPP20"]]),
      sprintf("USE_OPENMP=%s", combo_vals[["USE_OPENMP"]])
    )

    log_line("INFO", "  - install   -> start (log: %s)", install_log_rel)
    install_exit <- run_install(lib_path, env_vars, install_log)
    install_status <- if (install_exit == 0L) "passed" else "failed"
    install_note <- if (install_status == "passed") {
      "Build completed."
    } else {
      sprintf("Build exited with status %s.", install_exit)
    }
    log_line("INFO", "  - install   -> %s (exit=%s)", toupper(install_status), install_exit)
    timeline <- record_event(timeline, combo_name, "install", install_status, install_exit, install_log_rel, expectations$install, install_note)

    test_status <- "skipped"
    test_exit <- NA_integer_
    test_note <- "Tests were not executed."
    run_tests_flag <- install_status == "passed" && !opts$skip_tests
    if (run_tests_flag) {
      log_line("INFO", "  - tests     -> start (log: %s)", test_log_rel)
      test_exit <- run_tests(lib_path, test_log, combo_vals)
      test_status <- if (test_exit == 0L) "passed" else "failed"
      test_note <- if (test_status == "passed") {
        "Tests completed."
      } else {
        sprintf("Tests exited with status %s.", test_exit)
      }
    } else if (opts$skip_tests) {
      test_note <- "User requested --skip-tests."
    } else if (install_status != "passed") {
      test_note <- "Tests skipped because install failed."
    }
    log_line("INFO", "  - tests     -> %s%s", toupper(test_status), if (!is.na(test_exit)) sprintf(" (exit=%s)", test_exit) else "")
    timeline <- record_event(
      timeline,
      combo_name,
      "tests",
      test_status,
      if (is.na(test_exit)) NULL else test_exit,
      if (test_status == "skipped") NA_character_ else test_log_rel,
      expectations$tests,
      test_note
    )

    write_session_info(session_log, combo_name, combo_vals)

    combo_results[[i]] <- data.frame(
      combo = combo_name,
      install_status = install_status,
      install_exit = install_exit,
      install_expected = expectations$install$label,
      test_status = test_status,
      test_exit = if (is.na(test_exit)) NA_integer_ else test_exit,
      test_expected = expectations$tests$label,
      install_log = install_log_rel,
      test_log = if (test_status == "skipped") NA_character_ else test_log_rel,
      stringsAsFactors = FALSE
    )
    if (install_status != "passed" || test_status != "passed") {
      any_failure <- TRUE
    }
  }

  rule()
  log_line("INFO", "")
  timeline_df <- do.call(rbind, timeline$events)
  summary_df <- do.call(rbind, combo_results)
  timeline_path <- file.path(artifacts_root, sprintf("matrix_timeline_%s.csv", run_tag))
  summary_path <- file.path(artifacts_root, sprintf("matrix_summary_%s.csv", run_tag))
  write_csv(timeline_df, timeline_path)
  write_csv(summary_df, summary_path)
  timeline_path_rel <- rel_path(timeline_path)
  summary_path_rel <- rel_path(summary_path)

  log_line("INFO", "Execution timeline:")
  for (idx in seq_len(nrow(timeline_df))) {
    row <- timeline_df[idx, , drop = FALSE]
    expectation_note <- if (isTRUE(row$expectation_match)) {
      ""
    } else {
      sprintf(" (expected %s)", toupper(row$expected))
    }
    log_path <- row$log_path
    if (is.na(log_path) || !nzchar(log_path)) {
      log_path <- "NA"
    }
    log_line(
      "INFO",
      " %2d. [%s] %s -> %s%s | log=%s",
      row$order,
      row$combo,
      row$stage,
      toupper(row$status),
      expectation_note,
      log_path
    )
  }

  log_line("INFO", "")
  log_line("INFO", "Matrix summary:")
  print(summary_df, row.names = FALSE)
  log_line("INFO", "")
  log_line("INFO", "Timeline CSV : %s", timeline_path_rel)
  log_line("INFO", "Summary CSV  : %s", summary_path_rel)

  if (any_failure) {
    log_line("INFO", "Matrix completed with failures. Inspect artifacts for details.")
    quit(status = 1, save = "no")
  } else {
    log_line("INFO", "All combinations succeeded.")
    quit(status = 0, save = "no")
  }
}

main()
