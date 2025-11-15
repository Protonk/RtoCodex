# Matrix harness shared library: common logging, matrix definitions, and probe-
# aware helpers so both deterministic and fuzz runners can share behavior and
# emit comparable artifacts without duplicating base-R plumbing.

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

build_matrix <- function() {
  expand.grid(
    USE_CPP20 = c(0, 1),
    USE_OPENMP = c(0, 1),
    USE_DEVTOOLS = c(0, 1),
    PKGBUILD_ASSUME_TOOLS = c(0, 1),
    stringsAsFactors = FALSE
  )
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

detect_kern_boottime_block <- function() {
  if (!requireNamespace("ps", quietly = TRUE)) {
    return(list(blocked = NA, detected = FALSE, message = "ps package not available."))
  }
  err <- tryCatch({
    ps::ps_boot_time()
    NULL
  }, error = function(e) e)
  if (is.null(err)) {
    return(list(blocked = FALSE, detected = TRUE, message = "ps::ps_boot_time() succeeded."))
  }
  msg <- conditionMessage(err)
  blocked <- grepl("kern\\.boottime", msg, ignore.case = TRUE) &&
    grepl("Operation not permitted|permission denied|EPERM|EACCES", msg, ignore.case = TRUE)
  list(blocked = blocked, detected = TRUE, message = msg)
}

build_env_profile <- function() {
  sys <- Sys.info()
  config_keys <- c("CC", "CXX", "CXX17", "CXX20", "SHLIB_OPENMP_CFLAGS", "SHLIB_OPENMP_CXXFLAGS", "SHLIB_OPENMP_LDFLAGS")
  probes <- lapply(config_keys, probe_r_config)
  names(probes) <- config_keys
  cpp20 <- detect_cpp20_support(probes[["CXX20"]])
  openmp <- detect_openmp_support(probes[c("SHLIB_OPENMP_CFLAGS", "SHLIB_OPENMP_CXXFLAGS", "SHLIB_OPENMP_LDFLAGS")])
  sandbox <- list(
    kern_boottime = detect_kern_boottime_block()
  )
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
    sandbox = sandbox,
    label = label
  )
}

summarize_profile <- function(profile) {
  log_line("INFO", "Detected host: %s %s (%s)", profile$sys[["sysname"]], profile$sys[["release"]], profile$label)
  log_line("INFO", "  CXX20 probe: %s", profile$cpp20$reason)
  log_line("INFO", "  OpenMP probe: %s", profile$openmp$reason)
  sandbox <- profile$sandbox$kern_boottime
  status <- if (!isTRUE(sandbox$detected)) {
    "not evaluated"
  } else if (isTRUE(sandbox$blocked)) {
    "blocked"
  } else {
    "ok"
  }
  log_line("INFO", "  kern.boottime probe: %s (%s)", status, sandbox$message)
}

derive_expectations <- function(combo_row, profile, skip_tests = FALSE) {
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
  tests_notes <- character()
  sandbox_blocked <- isTRUE(profile$sandbox$kern_boottime$blocked)
  if (skip_tests) {
    tests_notes <- c(tests_notes, "User requested --skip-tests.")
  } else if (tests_label == "skip") {
    tests_notes <- c(tests_notes, "Tests depend on a successful build.")
  } else if (combo_row[["USE_DEVTOOLS"]] == 1) {
    tests_notes <- c(tests_notes, "Tests run via subprocess harness (Rscript).")
    if (sandbox_blocked && combo_row[["PKGBUILD_ASSUME_TOOLS"]] == 0) {
      tests_label <- "fail"
      tests_notes <- c(tests_notes, "Sandbox blocks kern.boottime for subprocess driver.")
    } else if (combo_row[["PKGBUILD_ASSUME_TOOLS"]] == 1) {
      tests_notes <- c(tests_notes, "PKGBUILD_ASSUME_TOOLS=1 disables the sandbox guard.")
    }
  } else {
    tests_notes <- c(tests_notes, "Tests run via embedded harness.")
  }
  list(
    install = list(label = install_label, notes = if (length(install_notes) == 0) "None" else paste(install_notes, collapse = " | ")),
    tests = list(label = tests_label, notes = if (length(tests_notes) == 0) "None" else paste(tests_notes, collapse = " | "))
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

set_env_vars <- function(vars) {
  if (length(vars) == 0) {
    return(function() invisible(NULL))
  }
  vars <- lapply(vars, as.character)
  old <- Sys.getenv(names(vars), unset = NA_character_)
  do.call(Sys.setenv, vars)
  function() {
    restore <- old[!is.na(old)]
    if (length(restore) > 0) {
      restore_list <- as.list(restore)
      names(restore_list) <- names(restore)
      do.call(Sys.setenv, restore_list)
    }
    to_unset <- names(old)[is.na(old)]
    if (length(to_unset) > 0) {
      Sys.unsetenv(to_unset)
    }
  }
}

run_tests <- function(lib_path, log_path, combo_row) {
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  env_vars <- c(
    sprintf("USE_CPP20=%s", combo_row[["USE_CPP20"]]),
    sprintf("USE_OPENMP=%s", combo_row[["USE_OPENMP"]]),
    sprintf("USE_DEVTOOLS=%s", combo_row[["USE_DEVTOOLS"]]),
    sprintf("PKGBUILD_ASSUME_TOOLS=%s", combo_row[["PKGBUILD_ASSUME_TOOLS"]])
  )
  use_devtools <- isTRUE(combo_row[["USE_DEVTOOLS"]] == 1)
  assume_pkgbuild <- isTRUE(combo_row[["PKGBUILD_ASSUME_TOOLS"]] == 1)
  sandbox_blocked <- isTRUE(getOption("RtoCodex.sandbox_blocks_kern_boottime", FALSE))

  if (!use_devtools) {
    con <- file(log_path, open = "wt")
    baseline_output <- sink.number()
    baseline_message <- sink.number(type = "message")
    sink(con)
    sink(con, type = "message")
    on.exit({
      while (sink.number(type = "message") > baseline_message) {
        sink(NULL, type = "message")
      }
      while (sink.number() > baseline_output) {
        sink(NULL)
      }
    })
    on.exit({
      if (isOpen(con)) {
        close(con)
      }
    }, add = TRUE)
    env_reset <- set_env_vars(lapply(combo_row, as.character))
    on.exit(env_reset(), add = TRUE)
    exit_code <- tryCatch({
      runner_path <- file.path(repo_root, "tests", "unit", "unit_runner.R")
      if (!file.exists(runner_path)) {
        stop(sprintf("Unit test runner not found at %s", runner_path))
      }
      runner_env <- new.env(parent = baseenv())
      sys.source(runner_path, envir = runner_env)
      result <- runner_env$rtocodex_run_unit_tests(config = list(driver = "embedded"), lib_path = lib_path)
      if (isTRUE(result$failed)) 1L else 0L
    }, error = function(e) {
      message("[embedded] Test run failed: ", conditionMessage(e))
      1L
    })
    return(exit_code)
  }

  if (sandbox_blocked && !assume_pkgbuild) {
    writeLines(
      c(
        "Sandbox blocked kern.boottime; subprocess harness intentionally aborts.",
        "Set PKGBUILD_ASSUME_TOOLS=1 to bypass the guard in this environment."
      ),
      con = log_path
    )
    return(5L)
  }

  args <- c(
    "tests/run_unit_tests.R",
    sprintf("--driver=%s", "subprocess"),
    sprintf("--lib=%s", lib_path)
  )
  status <- system2("Rscript", args = args, stdout = log_path, stderr = log_path, env = env_vars)
  if (is.null(status)) 0L else status
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
