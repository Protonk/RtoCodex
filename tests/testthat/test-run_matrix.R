# Matrix runner tests: verify scripts/run_matrix.R behaviors so CLI refactors
# keep argument parsing, expectation logic, and artifact bookkeeping intact.

test_that("matrix runner parses CLI arguments", {
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
  expect_equal(opts$lib_root, tmp_lib)
  expect_equal(opts$artifacts_root, tmp_artifacts)
  expect_true(opts$skip_tests)
  expect_equal(opts$filter, "USE_CPP20=1")
})

test_that("matrix runner rel_path shortens repo paths", {
  env <- load_run_matrix_env()
  repo <- env$repo_root_norm
  expect_equal(env$rel_path(repo), ".")
  script_path <- file.path(repo, "scripts", "run_matrix.R")
  expect_match(env$rel_path(script_path), "^scripts/")
  outside <- file.path(tempdir(), "totally-outside")
  expect_equal(env$rel_path(outside), outside)
})

test_that("matrix runner formats combination names predictably", {
  env <- load_run_matrix_env()
  combo <- data.frame(USE_OPENMP = 1, USE_CPP20 = 0)
  expect_equal(env$format_combo_name(combo), "USE_OPENMP=1_USE_CPP20=0")
  combo2 <- data.frame(USE_CPP20 = 1, USE_OPENMP = 0)
  expect_equal(env$format_combo_name(combo2), "USE_CPP20=1_USE_OPENMP=0")
})

test_that("matrix runner derives install and test expectations", {
  env <- load_run_matrix_env()
  combo <- data.frame(USE_CPP20 = 1, USE_OPENMP = 1, USE_DEVTOOLS = 1, PKGBUILD_ASSUME_TOOLS = 0)
  profile <- list(
    cpp20 = list(supported = FALSE, reason = "C++20 disabled"),
    openmp = list(state = "missing", reason = "OpenMP not in toolchain")
  )
  exp <- env$derive_expectations(combo, profile, skip_tests = FALSE)
  expect_equal(exp$install$label, "fail")
  expect_match(exp$install$notes, "C\\+\\+20 disabled")
  expect_equal(exp$tests$label, "skip")
  expect_match(exp$tests$notes, "depend on a successful build")

  combo2 <- data.frame(USE_CPP20 = 0, USE_OPENMP = 0, USE_DEVTOOLS = 1, PKGBUILD_ASSUME_TOOLS = 1)
  exp2 <- env$derive_expectations(combo2, profile, skip_tests = TRUE)
  expect_equal(exp2$install$label, "pass")
  expect_equal(exp2$tests$label, "skip")
  expect_match(exp2$tests$notes, "User requested")

  combo3 <- data.frame(USE_CPP20 = 0, USE_OPENMP = 0, USE_DEVTOOLS = 1, PKGBUILD_ASSUME_TOOLS = 0)
  exp3 <- env$derive_expectations(combo3, profile, skip_tests = FALSE)
  expect_match(exp3$tests$notes, "devtools::test")
  expect_match(exp3$tests$notes, "pkgbuild will probe toolchain")
})

test_that("matrix runner detects C++20 support signals", {
  env <- load_run_matrix_env()
  supported <- env$detect_cpp20_support(list(success = TRUE, value = "-std=gnu++20"))
  expect_true(supported$supported)
  expect_match(supported$reason, "CXX20 flags")

  failure <- env$detect_cpp20_support(list(success = FALSE, value = "no compiler found"))
  expect_false(failure$supported)
  expect_match(failure$reason, "CXX20 unavailable")
})

test_that("matrix runner classifies OpenMP probe states", {
  env <- load_run_matrix_env()
  probes_available <- list(
    list(success = FALSE, value = ""),
    list(success = TRUE, value = "-fopenmp")
  )
  available <- env$detect_openmp_support(probes_available)
  expect_equal(available$state, "available")
  expect_match(available$reason, "Flags")

  probes_missing <- list(
    list(success = FALSE, value = "no information in this build"),
    list(success = FALSE, value = "")
  )
  missing <- env$detect_openmp_support(probes_missing)
  expect_equal(missing$state, "missing")

  probes_unknown <- list(
    list(success = FALSE, value = "timeout"),
    list(success = FALSE, value = "")
  )
  unknown <- env$detect_openmp_support(probes_unknown)
  expect_equal(unknown$state, "unknown")
})

test_that("matrix runner records timeline events with expectation comparisons", {
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
  expect_equal(store$counter, 1L)
  event <- store$events[[1L]]
  expect_equal(event$stage, "install")
  expect_true(event$expectation_match)

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
  expect_false(event2$expectation_match)
  expect_equal(event2$exit_code, 2L)
})

test_that("matrix runner main produces timelines and summaries with stubbed installs", {
  env <- load_run_matrix_env()
  tmp_lib <- file.path(tempdir(), "matrix-lib-main")
  tmp_artifacts <- file.path(tempdir(), paste0("matrix-artifacts-", as.integer(runif(1, 1, 100000))))
  opts <- list(
    lib_root = tmp_lib,
    artifacts_root = tmp_artifacts,
    skip_tests = FALSE,
    filter = NULL
  )
  env$parse_args <- function() opts
  # Emulate a host that lacks C++20 and OpenMP tooling so expectation logic
  # stays deterministic even without a real compiler toolchain.
  profile <- list(
    sys = list(sysname = "TestOS", release = "1.0"),
    cpp20 = list(supported = FALSE, reason = "C++20 disabled in harness"),
    openmp = list(state = "missing", reason = "OpenMP variables absent"),
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
    driver <- if (combo_row[["USE_DEVTOOLS"]] == 1) "devtools" else "testthat"
    writeLines(c("fake tests", basename(lib_path), driver), log_path)
    if (combo_row[["USE_DEVTOOLS"]] == 1 && combo_row[["PKGBUILD_ASSUME_TOOLS"]] == 0) {
      return(4L)
    }
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
  expect_equal(captured_status, 1)

  timeline_files <- list.files(tmp_artifacts, pattern = "^matrix_timeline_.*\\.csv$", full.names = TRUE)
  summary_files <- list.files(tmp_artifacts, pattern = "^matrix_summary_.*\\.csv$", full.names = TRUE)
  expect_length(timeline_files, 1L)
  expect_length(summary_files, 1L)

  timeline <- utils::read.csv(timeline_files[[1L]], stringsAsFactors = FALSE)
  summary <- utils::read.csv(summary_files[[1L]], stringsAsFactors = FALSE)
  expect_equal(nrow(summary), 16L)
  expect_equal(sum(timeline$stage == "install"), 16L)
  expect_equal(sum(timeline$stage == "tests"), 16L)

  failing_installs <- subset(summary, grepl("USE_CPP20=1", combo))
  expect_true(all(failing_installs$install_status == "failed"))
  expect_true(all(failing_installs$test_status == "skipped"))

  failing_tests_openmp <- subset(summary, grepl("USE_OPENMP=1", combo) & !grepl("USE_CPP20=1", combo))
  expect_true(all(failing_tests_openmp$test_status == "failed"))
  expect_true(all(failing_tests_openmp$test_expected == "pass"))

  devtools_permission <- subset(summary, grepl("USE_DEVTOOLS=1", combo) & grepl("PKGBUILD_ASSUME_TOOLS=0", combo))
  devtools_permission <- subset(devtools_permission, !grepl("USE_CPP20=1", combo))
  expect_true(all(devtools_permission$test_status == "failed"))
  expect_true(all(grepl("USE_DEVTOOLS=1", devtools_permission$combo)))
  expect_true(all(devtools_permission$test_expected == "pass"))
})
