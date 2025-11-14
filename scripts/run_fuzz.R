#!/usr/bin/env Rscript
# RtoCodex fuzz runner: samples a subset of the deterministic matrix,
# reuses the shared harness helpers, and drops artifacts/fuzz/run_<id>/ with
# config.rds plus summary.csv for quick comparison of expected vs observed.

locate_script_file <- function(name) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  matches <- args[grepl(file_arg, args)]
  base_dir <- if (length(matches) > 0) {
    dirname(normalizePath(sub(file_arg, "", matches[length(matches)])))
  } else {
    file.path(getwd(), "scripts")
  }
  normalizePath(file.path(base_dir, name), mustWork = TRUE)
}

source(locate_script_file("matrix_lib.R"))
source(locate_script_file("load_probe_runtime.R"))

parse_args <- function() {
  defaults <- list(
    n = 4L,
    lib_root = "build-lib/fuzz",
    artifacts_root = "artifacts/fuzz",
    seed = NA_integer_
  )
  raw <- commandArgs(trailingOnly = TRUE)
  for (arg in raw) {
    if (startsWith(arg, "--n=")) {
      defaults$n <- as.integer(sub("^--n=", "", arg))
    } else if (startsWith(arg, "--lib=")) {
      defaults$lib_root <- sub("^--lib=", "", arg)
    } else if (startsWith(arg, "--artifacts=")) {
      defaults$artifacts_root <- sub("^--artifacts=", "", arg)
    } else if (startsWith(arg, "--seed=")) {
      defaults$seed <- as.integer(sub("^--seed=", "", arg))
    } else {
      warning(sprintf("Ignoring unrecognized argument: %s", arg))
    }
  }
  defaults
}

load_runtime_info <- function() {
  tryCatch(
    load_probe_runtime(),
    error = function(e) {
      log_line("WARN", "Probe runtime unavailable: %s", conditionMessage(e))
      NULL
    }
  )
}

main <- function() {
  opts <- parse_args()
  if (is.na(opts$n) || opts$n <= 0) {
    stop("--n must be a positive integer.")
  }

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

  runtime_info <- load_runtime_info()
  runtime_version <- if (!is.null(runtime_info) && !is.null(runtime_info$runtime_version)) {
    runtime_info$runtime_version
  } else if (exists("RUNTIME_VERSION", inherits = TRUE)) {
    get("RUNTIME_VERSION", inherits = TRUE)
  } else {
    NA_character_
  }

  all_combos <- build_matrix()
  matrix_size <- nrow(all_combos)
  if (matrix_size == 0) stop("Matrix definition returned zero rows.")
  sample_n <- min(opts$n, matrix_size)
  if (!is.na(opts$seed)) {
    set.seed(opts$seed)
  }
  sample_idx <- sample(seq_len(matrix_size), size = sample_n, replace = FALSE)
  combos <- all_combos[sample_idx, , drop = FALSE]
  combo_names <- vapply(seq_len(nrow(combos)), function(i) {
    format_combo_name(combos[i, , drop = FALSE])
  }, character(1))

  run_id <- sprintf("%s-%04d", format(Sys.time(), "%Y%m%d-%H%M%S"), sample.int(9999, 1))
  run_dir <- file.path(artifacts_root, sprintf("run_%s", run_id))
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  lib_run_root <- file.path(lib_root, sprintf("run_%s", run_id))
  dir.create(lib_run_root, recursive = TRUE, showWarnings = FALSE)

  config <- list(
    run_id = run_id,
    timestamp = Sys.time(),
    seed = if (is.na(opts$seed)) NA_integer_ else opts$seed,
    n = sample_n,
    requested_n = opts$n,
    matrix_size = matrix_size,
    env_profile = profile,
    runtime_version = runtime_version,
    runtime_dir = if (is.null(runtime_info)) NA_character_ else runtime_info$runtime_dir
  )
  saveRDS(config, file.path(run_dir, "config.rds"))

  log_line("INFO", "Fuzz run %s: executing %d/%d combos (seed=%s).", run_id, sample_n, matrix_size, if (is.na(opts$seed)) "NA" else opts$seed)

  results <- vector("list", length = nrow(combos))
  unexpected <- FALSE

  for (i in seq_len(nrow(combos))) {
    combo <- combos[i, , drop = FALSE]
    combo_vals <- as.list(combo[1, , drop = FALSE])
    combo_name <- combo_names[i]
    expectations <- derive_expectations(combo_vals, profile, skip_tests = FALSE)

    log_line("INFO", "")
    rule()
    log_line("INFO", "[%d/%d] %s", i, nrow(combos), combo_name)
    log_line("INFO", "  Expected install: %s (%s)", toupper(expectations$install$label), expectations$install$notes)
    log_line("INFO", "  Expected tests  : %s (%s)", toupper(expectations$tests$label), expectations$tests$notes)

    lib_path <- file.path(lib_run_root, combo_name)
    artifact_dir <- file.path(run_dir, combo_name)
    install_log <- file.path(artifact_dir, "install.log")
    test_log <- file.path(artifact_dir, "tests.log")
    session_log <- file.path(artifact_dir, "sessionInfo.txt")
    install_log_rel <- rel_path(install_log)
    test_log_rel <- rel_path(test_log)

    unlink(lib_path, recursive = TRUE, force = TRUE)
    dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)

    env_vars <- c(
      sprintf("USE_CPP20=%s", combo_vals[["USE_CPP20"]]),
      sprintf("USE_OPENMP=%s", combo_vals[["USE_OPENMP"]]),
      sprintf("USE_DEVTOOLS=%s", combo_vals[["USE_DEVTOOLS"]]),
      sprintf("PKGBUILD_ASSUME_TOOLS=%s", combo_vals[["PKGBUILD_ASSUME_TOOLS"]])
    )

    log_line("INFO", "  - install   -> start (log: %s)", install_log_rel)
    install_exit <- run_install(lib_path, env_vars, install_log)
    install_status <- if (install_exit == 0L) "passed" else "failed"
    install_note <- if (install_status == "passed") "Build completed." else sprintf("Build exited with status %s.", install_exit)
    log_line("INFO", "  - install   -> %s (exit=%s)", toupper(install_status), install_exit)

    test_status <- "skipped"
    test_exit <- NA_integer_
    test_note <- "Tests were not executed."
    if (install_status == "passed") {
      driver_label <- if (combo_vals[["USE_DEVTOOLS"]] == 1) "subprocess" else "embedded"
      log_line("INFO", "  - tests[%s] -> start (log: %s)", driver_label, test_log_rel)
      test_exit <- run_tests(lib_path, test_log, combo_vals)
      test_status <- if (test_exit == 0L) "passed" else "failed"
      test_note <- if (test_status == "passed") "Tests completed." else sprintf("Tests exited with status %s.", test_exit)
    } else {
      test_note <- "Tests skipped because install failed."
    }
    log_line("INFO", "  - tests     -> %s%s", toupper(test_status), if (!is.na(test_exit)) sprintf(" (exit=%s)", test_exit) else "")

    write_session_info(session_log, combo_name, combo_vals)

    install_match <- identical(status_category(install_status), expectations$install$label)
    test_match <- identical(status_category(test_status), expectations$tests$label)
    expectation_match <- install_match && test_match
    unexpected <- unexpected || !expectation_match

    results[[i]] <- data.frame(
      combo = combo_name,
      USE_CPP20 = combo_vals[["USE_CPP20"]],
      USE_OPENMP = combo_vals[["USE_OPENMP"]],
      USE_DEVTOOLS = combo_vals[["USE_DEVTOOLS"]],
      PKGBUILD_ASSUME_TOOLS = combo_vals[["PKGBUILD_ASSUME_TOOLS"]],
      install_expected = expectations$install$label,
      install_status = install_status,
      install_exit = install_exit,
      install_log = install_log_rel,
      tests_expected = expectations$tests$label,
      tests_status = test_status,
      tests_exit = if (is.na(test_exit)) NA_integer_ else test_exit,
      tests_log = if (test_status == "skipped") NA_character_ else test_log_rel,
      install_note = install_note,
      tests_note = test_note,
      install_expectation_match = install_match,
      tests_expectation_match = test_match,
      expectation_match = expectation_match,
      stringsAsFactors = FALSE
    )
  }

  summary_df <- do.call(rbind, results)
  summary_path <- file.path(run_dir, "summary.csv")
  write_csv(summary_df, summary_path)
  summary_path_rel <- rel_path(summary_path)

  log_line("INFO", "")
  log_line("INFO", "Fuzz summary (%s):", run_id)
  print(summary_df[, c("combo", "install_status", "tests_status", "expectation_match")], row.names = FALSE)
  log_line("INFO", "Summary CSV  : %s", summary_path_rel)
  log_line("INFO", "Config (RDS): %s", rel_path(file.path(run_dir, "config.rds")))

  if (unexpected) {
    log_line("INFO", "Fuzz run completed with unexpected results.")
    quit(status = 1, save = "no")
  }
  log_line("INFO", "Fuzz run completed; all outcomes matched expectations.")
  quit(status = 0, save = "no")
}

main()
