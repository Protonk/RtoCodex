#!/usr/bin/env Rscript
# RtoCodex matrix runner: drives the USE_CPP20/USE_OPENMP matrix and emits a
# structured timeline so humans or agents can see (1) which paths were taken,
# (2) the stage that failed, and (3) whether the result matches an environment
# guess derived from R CMD config probes. Artifacts include per-combo logs plus
# CSV summaries under artifacts/matrix for later replay.

fmt_time <- function() format(Sys.time(), "%H:%M:%S")

log_line <- function(level = "INFO", msg, ...) {
  cat(sprintf("[%s] %-5s %s\n", fmt_time(), level, sprintf(msg, ...)))
}

rule <- function(char = "-", width = 72) {
  log_line("INFO", paste(rep(char, width), collapse = ""))
}

trim_text <- function(x) {
  if (length(x) == 0) {
    return("")
  }
  trimws(paste(x, collapse = "\n"))
}

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

repo_root <- find_repo_root()
setwd(repo_root)
repo_root_norm <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)

rel_path <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path)) {
    return(path)
  }
  normalized <- tryCatch(
    suppressWarnings(normalizePath(path, winslash = "/", mustWork = FALSE)),
    error = function(e) path
  )
  prefix <- paste0(repo_root_norm, "/")
  if (identical(normalized, repo_root_norm)) {
    return(".")
  }
  if (startsWith(normalized, prefix)) {
    rel <- substr(normalized, nchar(prefix) + 1, nchar(normalized))
    if (!nzchar(rel)) return(".")
    return(rel)
  }
  path
}

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

probe_r_config <- function(key) {
  out <- tryCatch(
    system2("R", c("CMD", "config", key), stdout = TRUE, stderr = TRUE),
    error = function(e) {
      attr <- list(value = sprintf("error: %s", conditionMessage(e)), success = FALSE, status = NA_integer_)
      return(attr)
    }
  )
  if (is.list(out)) {
    return(out)
  }
  status <- attr(out, "status")
  success <- is.null(status) || identical(status, 0L)
  list(
    value = trim_text(out),
    success = success,
    status = if (is.null(status)) 0L else status
  )
}

detect_cpp20_support <- function(cxx20_probe) {
  supported <- cxx20_probe$success && grepl("\\+\\+2", cxx20_probe$value)
  reason <- if (!cxx20_probe$success && nzchar(cxx20_probe$value)) {
    sprintf("CXX20 unavailable (%s).", cxx20_probe$value)
  } else if (supported) {
    sprintf("CXX20 flags include %s.", cxx20_probe$value)
  } else if (nzchar(cxx20_probe$value)) {
    sprintf("CXX20 does not advertise C++20 support (%s).", cxx20_probe$value)
  } else {
    "CXX20 probe returned an empty string."
  }
  list(supported = supported, reason = reason)
}

detect_openmp_support <- function(openmp_probes) {
  available <- any(vapply(openmp_probes, function(p) {
    isTRUE(p$success) && nzchar(p$value)
  }, logical(1)))
  if (available) {
    flags <- trim_text(vapply(openmp_probes, `[[`, character(1), "value"))
    return(list(state = "available", reason = sprintf("Flags: %s", flags)))
  }
  missing_info <- any(vapply(openmp_probes, function(p) {
    !isTRUE(p$success) && grepl("no information", tolower(p$value))
  }, logical(1)))
  if (missing_info) {
    return(list(state = "missing", reason = "R CMD config reports no OpenMP variables."))
  }
  list(state = "unknown", reason = "OpenMP support undetermined from R CMD config.")
}

build_env_profile <- function() {
  sys <- Sys.info()
  config_keys <- c("CC", "CXX", "CXX17", "CXX20", "SHLIB_OPENMP_CFLAGS", "SHLIB_OPENMP_CXXFLAGS", "SHLIB_OPENMP_LDFLAGS")
  probes <- lapply(config_keys, probe_r_config)
  names(probes) <- config_keys
  cpp20 <- detect_cpp20_support(probes[["CXX20"]])
  openmp <- detect_openmp_support(probes[c("SHLIB_OPENMP_CFLAGS", "SHLIB_OPENMP_CXXFLAGS", "SHLIB_OPENMP_LDFLAGS")])
  clean_label <- function(x) {
    gsub("[^[:alnum:]_\\-]+", "", x)
  }
  label <- paste(
    clean_label(tolower(sys[["sysname"]])),
    clean_label(sys[["machine"]]),
    clean_label(if (nzchar(probes[["CXX"]]$value)) probes[["CXX"]]$value else "unknown"),
    sep = "-"
  )
  list(
    sys = sys,
    probes = probes,
    cpp20 = cpp20,
    openmp = openmp,
    label = label
  )
}

summarize_profile <- function(profile) {
  log_line("INFO", "Detected host: %s %s (%s)", profile$sys[["sysname"]], profile$sys[["release"]], profile$label)
  log_line("INFO", "  CXX20 probe: %s", profile$cpp20$reason)
  log_line("INFO", "  OpenMP probe: %s", profile$openmp$reason)
}

derive_expectations <- function(combo_row, profile, skip_tests) {
  install_notes <- character()
  install_label <- "pass"
  if (combo_row[["USE_CPP20"]] == 1 && !profile$cpp20$supported) {
    install_label <- "fail"
    install_notes <- c(install_notes, profile$cpp20$reason)
  }
  if (combo_row[["USE_OPENMP"]] == 1 && identical(profile$openmp$state, "missing")) {
    install_notes <- c(install_notes, profile$openmp$reason)
  }
  tests_label <- if (skip_tests) {
    "skip"
  } else if (install_label == "fail") {
    "skip"
  } else {
    "pass"
  }
  tests_notes <- if (skip_tests) {
    "User requested --skip-tests."
  } else if (tests_label == "skip") {
    "Tests depend on a successful build."
  } else {
    "Execute testthat suite."
  }
  list(
    install = list(label = install_label, notes = if (length(install_notes) == 0) "None" else paste(install_notes, collapse = " | ")),
    tests = list(label = tests_label, notes = tests_notes)
  )
}

run_install <- function(lib_path, env_vars, log_path) {
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  con <- file(log_path, open = "wt")
  on.exit(close(con))
  system2(
    command = "R",
    args = c("CMD", "INSTALL", "--preclean", "-l", lib_path, "."),
    stdout = con,
    stderr = con,
    env = env_vars
  )
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
  exit_code <- 0L
  tryCatch({
    suppressPackageStartupMessages(library("RtoCodex", lib.loc = lib_path, character.only = TRUE))
    on.exit(detach("package:RtoCodex", unload = TRUE), add = TRUE)
    testthat::test_dir("tests/testthat", reporter = "summary")
  }, error = function(e) {
    exit_code <<- 1L
    message("Test run failed: ", conditionMessage(e))
  })
  exit_code
}

write_session_info <- function(path, combo_name, combo_row) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  run_cfg <- function(key) {
    out <- probe_r_config(key)
    sprintf("success=%s status=%s value=%s", out$success, out$status, out$value)
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

status_category <- function(status) {
  switch(
    status,
    passed = "pass",
    failed = "fail",
    skipped = "skip",
    status
  )
}

record_event <- function(store, combo_name, stage, status, exit_code, log_path, expectation, note) {
  if (is.null(store$counter)) {
    store$counter <- 0L
  }
  store$counter <- store$counter + 1L
  normalized <- status_category(status)
  match_expectation <- identical(normalized, expectation$label)
  store$events[[length(store$events) + 1L]] <- data.frame(
    order = store$counter,
    timestamp = fmt_time(),
    combo = combo_name,
    stage = stage,
    status = status,
    exit_code = if (is.null(exit_code)) NA_integer_ else exit_code,
    log_path = log_path,
    expected = expectation$label,
    expectation_match = match_expectation,
    expectation_notes = expectation$notes,
    note = note,
    stringsAsFactors = FALSE
  )
  invisible(store)
}

event_store <- function() {
  list(counter = 0L, events = list())
}

write_csv <- function(df, path) {
  utils::write.csv(df, file = path, row.names = FALSE)
}

main <- function() {
  opts <- parse_args()
  lib_root <- normalizePath(opts$lib_root, mustWork = FALSE)
  artifacts_root <- normalizePath(opts$artifacts_root, mustWork = FALSE)
  dir.create(lib_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(artifacts_root, recursive = TRUE, showWarnings = FALSE)

  profile <- build_env_profile()
  summarize_profile(profile)

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

  log_line("INFO", "Running %d matrix combinations (skip_tests=%s).", nrow(combos), opts$skip_tests)

  timeline <- event_store()
  combo_results <- vector("list", length = nrow(combos))
  any_failure <- FALSE
  run_tag <- format(Sys.time(), "%Y%m%d-%H%M%S")

  for (i in seq_len(nrow(combos))) {
    combo <- combos[i, , drop = FALSE]
    combo_name <- combo_names[i]
    expectations <- derive_expectations(combo, profile, opts$skip_tests)

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
      sprintf("USE_CPP20=%s", combo[["USE_CPP20"]]),
      sprintf("USE_OPENMP=%s", combo[["USE_OPENMP"]])
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
      test_exit <- run_tests(lib_path, test_log)
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

    write_session_info(session_log, combo_name, combo)

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
