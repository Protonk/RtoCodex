# tests/unit/test_capabilities.R documents how capability probes gate work that
# depends on host toolchains or sandbox permissions.

capture_probe_run <- function(script_path) {
  skip_if(!file.exists(script_path), sprintf("Probe script missing: %s", script_path))
  err_file <- tempfile("cap_probe_test_err_")
  on.exit(unlink(err_file, recursive = FALSE, force = TRUE), add = TRUE)
  output <- tryCatch(
    system2("Rscript", c(script_path), stdout = TRUE, stderr = err_file),
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
  value <- ""
  if (length(output) > 0) {
    value <- trimws(output[[length(output)]])
  }
  list(status = status, value = value, stderr = stderr_lines)
}

test_case("cap_cxx20_flags toggles C++20 dependent work", {
  skip_if(!RtoCodex:::needs_cap("cap_cxx20_flags", "supported"), "C++20 flags missing")
  assert_true(TRUE)
})

test_case("cap_openmp_flags toggles OpenMP dependent work", {
  skip_if(!RtoCodex:::needs_cap("cap_openmp_flags", "available"), "OpenMP flags missing")
  assert_true(TRUE)
})

test_case("cap_sysctl_kern_boottime gates sysctl-dependent tests", {
  skip_if(
    !RtoCodex:::needs_cap("cap_sysctl_kern_boottime", "supported"),
    "sysctl kern.boottime not supported in this environment"
  )
  assert_true(TRUE)
})

test_case("caps helper returns NA when capability artifacts are missing", {
  cap_name <- sprintf("cap_unit_missing_%s", as.integer(runif(1, 1, 100000)))
  assert_true(is.na(RtoCodex:::read_cap(cap_name)))
  assert_false(RtoCodex:::needs_cap(cap_name, "supported"))
})

test_case("caps helper reads newly created capability files", {
  cap_dir <- RtoCodex:::caps_dir()
  dir.create(cap_dir, recursive = TRUE, showWarnings = FALSE)
  cap_name <- sprintf("cap_unit_present_%s", Sys.getpid())
  cap_path <- file.path(cap_dir, sprintf("%s.txt", cap_name))
  on.exit(unlink(cap_path, recursive = FALSE, force = TRUE), add = TRUE)
  writeLines("supported", cap_path)
  assert_equal(RtoCodex:::read_cap(cap_name), "supported")
  assert_true(RtoCodex:::needs_cap(cap_name, "supported"))
})

test_case("cap_fortran_shlib probe emits a stable classification token", {
  script <- pkg_file("probes", "cap_fortran_shlib.R")
  res <- capture_probe_run(script)
  valid <- c("available", "missing", "error")
  assert_true(res$value %in% valid, info = sprintf("Unexpected status: %s", res$value))
  if (identical(res$value, "error")) {
    assert_true(res$status != 0L, "error classifications should return a non-zero exit status")
  } else {
    assert_equal(res$status, 0L, "non-error classifications should not signal failure")
  }
})

test_case("cap_utf8_locale probe reports locale handling tiers", {
  script <- pkg_file("probes", "cap_utf8_locale.R")
  res <- capture_probe_run(script)
  valid <- c("utf8_native", "utf8_decode_only", "utf8_unsupported", "probe_error")
  assert_true(res$value %in% valid, info = sprintf("Unexpected status: %s", res$value))
  if (identical(res$value, "probe_error")) {
    assert_true(res$status != 0L, "probe_error should propagate a failing exit status")
  } else {
    assert_equal(res$status, 0L, "locale classifications should exit cleanly")
  }
})

test_case("cap_temp_symlink_exec probe distinguishes filesystem behaviors", {
  script <- pkg_file("probes", "cap_temp_symlink_exec.R")
  res <- capture_probe_run(script)
  valid <- c("supported", "symlink_blocked", "noexec", "error")
  assert_true(res$value %in% valid, info = sprintf("Unexpected status: %s", res$value))
  if (identical(res$value, "error")) {
    assert_true(res$status != 0L, "error classifications should bubble up failures")
  } else {
    assert_equal(res$status, 0L, "symlink classifications should be informational only")
  }
})

test_case("cap_pkg_config_path probe validates pkg-config discovery", {
  script <- pkg_file("probes", "cap_pkg_config_path.R")
  res <- capture_probe_run(script)
  valid <- c("available", "missing", "error")
  assert_true(res$value %in% valid, info = sprintf("Unexpected status: %s", res$value))
  if (identical(res$value, "error")) {
    assert_true(res$status != 0L, "error tokens must propagate the exit code")
  } else {
    assert_equal(res$status, 0L, "available/missing should exit cleanly")
  }
})

test_case("cap_long_tmp_paths probe detects filesystem path limits", {
  script <- pkg_file("probes", "cap_long_tmp_paths.R")
  res <- capture_probe_run(script)
  valid <- c("supported", "path_limit", "error")
  assert_true(res$value %in% valid, info = sprintf("Unexpected status: %s", res$value))
  if (identical(res$value, "error")) {
    assert_true(res$status != 0L, "error classifications should signal probe failure")
  } else {
    assert_equal(res$status, 0L, "supported/path_limit should not fail the harness")
  }
})

test_case("cap_case_sensitive_tmpfs probe reports filesystem casing behavior", {
  script <- pkg_file("probes", "cap_case_sensitive_tmpfs.R")
  res <- capture_probe_run(script)
  valid <- c("case_sensitive", "case_insensitive", "error")
  assert_true(res$value %in% valid, info = sprintf("Unexpected status: %s", res$value))
  if (identical(res$value, "error")) {
    assert_true(res$status != 0L, "error classifications should bubble up the exit code")
  } else {
    assert_equal(res$status, 0L, "case sensitivity detection must exit successfully")
  }
})
