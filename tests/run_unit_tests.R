#!/usr/bin/env Rscript
# tests/run_unit_tests.R bootstraps the lightweight harness so R CMD check and
# local agents can execute the suite via Rscript without extra packages.

parse_unit_test_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  cfg <- list(lib = NULL, driver = "cli")
  for (arg in args) {
    if (startsWith(arg, "--lib=")) {
      cfg$lib <- sub("^--lib=", "", arg)
    } else if (startsWith(arg, "--driver=")) {
      cfg$driver <- sub("^--driver=", "", arg)
    } else {
      warning(sprintf("Ignoring unrecognized argument: %s", arg))
    }
  }
  cfg
}

locate_tests_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  matches <- args[grepl(file_arg, args)]
  if (length(matches) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", matches[length(matches)]))))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

main <- function() {
  opts <- parse_unit_test_args()
  tests_dir <- locate_tests_dir()
  runner_path <- file.path(tests_dir, "unit", "unit_runner.R")
  if (!file.exists(runner_path)) {
    stop("Unable to find unit runner at ", runner_path)
  }
  runner_env <- new.env(parent = baseenv())
  sys.source(runner_path, envir = runner_env)
  result <- runner_env$rtocodex_run_unit_tests(config = list(driver = opts$driver), lib_path = opts$lib)
  if (isTRUE(result$failed)) {
    quit(status = 1, save = "no")
  }
  quit(status = 0, save = "no")
}

main()
