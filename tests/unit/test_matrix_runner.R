# tests/unit/test_matrix_runner.R exercises scripts/run_matrix.R helpers so the
# matrix harness remains stable across refactors in both the CLI and fuzz modes.

test_case("matrix runner parses CLI arguments", {
  env <- load_run_matrix_env()
  tmp_lib <- file.path(tempdir(), "matrix-lib")
  tmp_artifacts <- file.path(tempdir(), "matrix-artifacts")
  opts <- with_mocked_command_args(
    env,
    c(
      sprintf("--lib=%s", tmp_lib),
      sprintf("--artifacts=%s", tmp_artifacts),
      "--skip-tests",
      "--filter=USE_CPP20=1"
    ),
    env$parse_args()
  )
  assert_equal(opts$lib_root, tmp_lib)
  assert_equal(opts$artifacts_root, tmp_artifacts)
  assert_true(opts$skip_tests)
  assert_equal(opts$filter, "USE_CPP20=1")
})

test_case("matrix runner rel_path shortens repo paths", {
  env <- load_run_matrix_env()
  repo <- env$repo_root_norm
  assert_equal(env$rel_path(repo), ".")
  script_path <- file.path(repo, "scripts", "run_matrix.R")
  assert_match(env$rel_path(script_path), "^scripts/")
  outside <- file.path(tempdir(), "rtocodex-outside")
  assert_equal(env$rel_path(outside), outside)
})

test_case("matrix builder spans every toggle combination", {
  env <- load_run_matrix_env()
  combos <- env$build_matrix()
  expected <- expand.grid(
    USE_CPP20 = c(0, 1),
    USE_OPENMP = c(0, 1),
    stringsAsFactors = FALSE
  )
  assert_equal(nrow(combos), 4L)
  assert_true(identical(combos, expected))
})

test_case("matrix runner formats combination names predictably", {
  env <- load_run_matrix_env()
  combo <- data.frame(USE_OPENMP = 1, USE_CPP20 = 0)
  assert_equal(env$format_combo_name(combo), "USE_OPENMP=1_USE_CPP20=0")
  combo2 <- data.frame(USE_CPP20 = 1, USE_OPENMP = 0)
  assert_equal(env$format_combo_name(combo2), "USE_CPP20=1_USE_OPENMP=0")
})

test_case("matrix runner derives install and test expectations", {
  env <- load_run_matrix_env()
  combo <- list(USE_CPP20 = 1, USE_OPENMP = 1)
  profile <- list(
    cpp20 = list(supported = FALSE, reason = "C++20 disabled"),
    openmp = list(state = "missing", reason = "OpenMP not in toolchain"),
    sandbox = list(kern_boottime = list(blocked = TRUE, detected = TRUE, message = "Operation not permitted"))
  )
  exp <- env$derive_expectations(combo, profile, skip_tests = FALSE)
  assert_equal(exp$install$label, "fail")
  assert_match(exp$install$notes, "C\\+\\+20 disabled")
  assert_equal(exp$tests$label, "skip")
  assert_match(exp$tests$notes, "depend on a successful build")

  combo2 <- list(USE_CPP20 = 0, USE_OPENMP = 0)
  exp2 <- env$derive_expectations(combo2, profile, skip_tests = TRUE)
  assert_equal(exp2$install$label, "pass")
  assert_equal(exp2$tests$label, "skip")
  assert_match(exp2$tests$notes, "User requested")

  combo3 <- list(USE_CPP20 = 0, USE_OPENMP = 0)
  exp3 <- env$derive_expectations(combo3, profile, skip_tests = FALSE)
  assert_equal(exp3$tests$label, "fail")
  assert_match(exp3$tests$notes, "guard aborts")

  profile_no_block <- profile
  profile_no_block$sandbox$kern_boottime$blocked <- FALSE
  exp4 <- env$derive_expectations(combo3, profile_no_block, skip_tests = FALSE)
  assert_equal(exp4$tests$label, "pass")
  assert_match(exp4$tests$notes, "embedded harness")

  combo_openmp <- list(USE_CPP20 = 0, USE_OPENMP = 1)
  exp_openmp <- env$derive_expectations(combo_openmp, profile_no_block, skip_tests = FALSE)
  assert_equal(exp_openmp$tests$label, "pass")
  assert_match(exp_openmp$tests$notes, "embedded harness")
})

test_case("matrix runner detects C++20 support signals", {
  env <- load_run_matrix_env()
  supported <- env$detect_cpp20_support(list(success = TRUE, value = "-std=gnu++20"))
  assert_true(supported$supported)
  assert_match(supported$reason, "CXX20 flags")

  failure <- env$detect_cpp20_support(list(success = FALSE, value = "no compiler found"))
  assert_false(failure$supported)
  assert_match(failure$reason, "CXX20 unavailable")
})

test_case("matrix runner classifies OpenMP probe states", {
  env <- load_run_matrix_env()
  probes_available <- list(
    list(success = FALSE, value = ""),
    list(success = TRUE, value = "-fopenmp")
  )
  available <- env$detect_openmp_support(probes_available)
  assert_equal(available$state, "available")
  assert_match(available$reason, "Flags")

  probes_missing <- list(
    list(success = FALSE, value = "no information in this build"),
    list(success = FALSE, value = "")
  )
  missing <- env$detect_openmp_support(probes_missing)
  assert_equal(missing$state, "missing")

  probes_unknown <- list(
    list(success = FALSE, value = "timeout"),
    list(success = FALSE, value = "")
  )
  unknown <- env$detect_openmp_support(probes_unknown)
  assert_equal(unknown$state, "unknown")
})

test_case("matrix runner records timeline events with expectation comparisons", {
  env <- load_run_matrix_env()
  store <- env$event_store()
  expectation <- list(label = "pass", notes = "None")
  store <- env$record_event(
    store,
    combo_name = "USE_CPP20=0_USE_OPENMP=0",
    stage = "install",
    status = "passed",
    exit_code = 0L,
    log_path = "artifacts/matrix/foo/install.log",
    expectation = expectation,
    note = "Build completed."
  )
  assert_equal(store$counter, 1L)
  event <- store$events[[1L]]
  assert_equal(event$stage, "install")
  assert_true(event$expectation_match)

  expectation_fail <- list(label = "pass", notes = "None")
  store <- env$record_event(
    store,
    combo_name = "USE_CPP20=1_USE_OPENMP=1",
    stage = "tests",
    status = "failed",
    exit_code = 2L,
    log_path = "artifacts/matrix/bar/tests.log",
    expectation = expectation_fail,
    note = "Tests exited with status 2."
  )
  event2 <- store$events[[2L]]
  assert_false(event2$expectation_match)
  assert_equal(event2$exit_code, 2L)
})

test_case("matrix runner main produces timelines and summaries with stubbed installs", {
  env <- load_run_matrix_env()
  tmp_lib <- file.path(tempdir(), "matrix-lib-main")
  tmp_artifacts <- file.path(tempdir(), paste0("matrix-artifacts-", as.integer(Sys.time())))
  opts <- list(
    lib_root = tmp_lib,
    artifacts_root = tmp_artifacts,
    skip_tests = FALSE,
    filter = NULL
  )
  env$parse_args <- function() opts
  profile <- list(
    sys = list(sysname = "TestOS", release = "1.0"),
    cpp20 = list(supported = FALSE, reason = "C++20 disabled in harness"),
    openmp = list(state = "missing", reason = "OpenMP variables absent"),
    sandbox = list(kern_boottime = list(blocked = FALSE, detected = TRUE, message = "ps::ps_boot_time() succeeded.")),
    label = "test-profile"
  )
  env$build_env_profile <- function() profile
  captured_status <- NULL
  env$quit <- function(status = 0, ...) {
    captured_status <<- status
    invisible(NULL)
  }
  env$run_install <- function(lib_path, env_vars, log_path) {
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(c("fake install", paste(env_vars, collapse = ",")), log_path)
    if (any(grepl("USE_CPP20=1", env_vars))) {
      return(2L)
    }
    0L
  }
  env$run_tests <- function(lib_path, log_path, combo_row) {
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(c("fake tests", basename(lib_path)), log_path)
    if (combo_row[["USE_OPENMP"]] == 1) {
      return(3L)
    }
    0L
  }
  env$write_session_info <- function(path, combo_name, combo_row) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(sprintf("combo=%s", combo_name), path)
  }
  on.exit(unlink(c(tmp_lib, tmp_artifacts), recursive = TRUE, force = TRUE), add = TRUE)

  env$main()
  assert_equal(captured_status, 1)

  timeline_files <- list.files(tmp_artifacts, pattern = "^matrix_timeline_.*\\.csv$", full.names = TRUE)
  summary_files <- list.files(tmp_artifacts, pattern = "^matrix_summary_.*\\.csv$", full.names = TRUE)
  assert_length(timeline_files, 1L)
  assert_length(summary_files, 1L)

  timeline <- utils::read.csv(timeline_files[[1L]], stringsAsFactors = FALSE)
  summary <- utils::read.csv(summary_files[[1L]], stringsAsFactors = FALSE)
  assert_equal(nrow(summary), 4L)
  assert_equal(sum(timeline$stage == "install"), 4L)
  assert_equal(sum(timeline$stage == "tests"), 4L)

  failing_installs <- subset(summary, grepl("USE_CPP20=1", combo))
  assert_true(all(failing_installs$install_status == "failed"))
  assert_true(all(failing_installs$test_status == "skipped"))

  failing_tests_openmp <- subset(summary, grepl("USE_OPENMP=1", combo) & !grepl("USE_CPP20=1", combo))
  assert_true(all(failing_tests_openmp$test_status == "failed"))
  assert_true(all(failing_tests_openmp$test_exit == 3))
  assert_true(all(failing_tests_openmp$test_expected == "pass"))

  passing_tests <- subset(summary, !grepl("USE_OPENMP=1", combo) & !grepl("USE_CPP20=1", combo))
  assert_true(all(passing_tests$test_status == "passed"))
  assert_true(all(passing_tests$test_expected == "pass"))
})

test_case("environment helper restores overrides", {
  env <- load_run_matrix_env()
  reset <- env$set_env_vars(list(RTOCODEX_TEMP_A = "alpha", RTOCODEX_TEMP_B = 2))
  assert_equal(Sys.getenv("RTOCODEX_TEMP_A"), "alpha")
  assert_equal(Sys.getenv("RTOCODEX_TEMP_B"), "2")
  reset()
  assert_equal(Sys.getenv("RTOCODEX_TEMP_A", unset = NA_character_), NA_character_)
  assert_equal(Sys.getenv("RTOCODEX_TEMP_B", unset = NA_character_), NA_character_)
})
