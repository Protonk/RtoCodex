# tests/unit/test_capabilities.R documents how capability probes gate work that
# depends on host toolchains or sandbox permissions.

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

test_case("cap_locale_utf8 probe emits locale classification token", {
  script <- pkg_file("probes", "cap_locale_utf8.R")
  skip_if(!file.exists(script), "cap_locale_utf8 script not available")
  err_file <- tempfile("cap_locale_utf8_test_err_")
  on.exit(unlink(err_file), add = TRUE)
  output <- tryCatch(system2("Rscript", script, stdout = TRUE, stderr = err_file), error = function(e) e)
  if (inherits(output, "error")) {
    fail_test(sprintf("cap_locale_utf8 probe failed to launch: %s", conditionMessage(output)))
  }
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  stderr_lines <- if (file.exists(err_file)) paste(readLines(err_file, warn = FALSE), collapse = " ") else ""
  assert_equal(status, 0L, info = sprintf("stderr: %s", stderr_lines))
  assert_true(length(output) >= 1L, "cap_locale_utf8 produced no output")
  token <- trimws(output[[1L]])
  assert_true(token %in% c("utf8", "latin1_only", "mismatch"), info = sprintf("unexpected token: %s", token))
})

test_case("cap_fs_case_sensitivity identifies filesystem behavior", {
  script <- pkg_file("probes", "cap_fs_case_sensitivity.R")
  skip_if(!file.exists(script), "cap_fs_case_sensitivity script not available")
  err_file <- tempfile("cap_fs_case_test_err_")
  on.exit(unlink(err_file), add = TRUE)
  output <- tryCatch(system2("Rscript", script, stdout = TRUE, stderr = err_file), error = function(e) e)
  if (inherits(output, "error")) {
    fail_test(sprintf("cap_fs_case_sensitivity probe failed to launch: %s", conditionMessage(output)))
  }
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  stderr_lines <- if (file.exists(err_file)) paste(readLines(err_file, warn = FALSE), collapse = " ") else ""
  assert_equal(status, 0L, info = sprintf("stderr: %s", stderr_lines))
  assert_true(length(output) >= 1L, "cap_fs_case_sensitivity produced no output")
  token <- trimws(output[[1L]])
  assert_true(token %in% c("case_sensitive", "case_insensitive"), info = sprintf("unexpected token: %s", token))
})

test_case("cap_fortran_config reports compiler availability", {
  script <- pkg_file("probes", "cap_fortran_config.R")
  skip_if(!file.exists(script), "cap_fortran_config script not available")
  err_file <- tempfile("cap_fortran_config_test_err_")
  on.exit(unlink(err_file), add = TRUE)
  output <- tryCatch(system2("Rscript", script, stdout = TRUE, stderr = err_file), error = function(e) e)
  if (inherits(output, "error")) {
    fail_test(sprintf("cap_fortran_config probe failed to launch: %s", conditionMessage(output)))
  }
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  stderr_lines <- if (file.exists(err_file)) paste(readLines(err_file, warn = FALSE), collapse = " ") else ""
  assert_true(status %in% c(0L, 1L), info = sprintf("unexpected exit status: %s stderr: %s", status, stderr_lines))
  assert_true(length(output) >= 1L, "cap_fortran_config produced no output")
  token <- trimws(output[[1L]])
  assert_true(token %in% c("available", "missing", "error"), info = sprintf("unexpected token: %s", token))
})
