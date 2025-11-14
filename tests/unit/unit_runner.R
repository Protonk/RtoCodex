# tests/unit/unit_runner.R centralizes the dependency-free unit test harness so
# both the CLI scripts and R CMD check can execute the same suite without
# pulling in devtools or testthat.

rtocodex_find_package_root <- local({
  cached <- NULL
  target <- "^Package:\\s*RtoCodex\\s*$"
  function(start = NULL) {
    if (!is.null(cached) && file.exists(file.path(cached, "DESCRIPTION"))) {
      return(cached)
    }
    path <- if (is.null(start)) getwd() else start
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    repeat {
      desc <- file.path(path, "DESCRIPTION")
      if (file.exists(desc)) {
        lines <- readLines(desc, warn = FALSE)
        if (any(grepl(target, lines))) {
          cached <<- path
          return(cached)
        }
      }
      candidate <- file.path(path, "RtoCodex")
      candidate_desc <- file.path(candidate, "DESCRIPTION")
      if (file.exists(candidate_desc)) {
        lines <- readLines(candidate_desc, warn = FALSE)
        if (any(grepl(target, lines))) {
          cached <<- candidate
          return(cached)
        }
      }
      parent <- dirname(path)
      if (identical(parent, path)) {
        stop("Unable to locate DESCRIPTION for package RtoCodex starting from ", start %||% getwd())
      }
      path <- parent
    }
  }
})

`%||%` <- function(x, y) {
  if (is.null(x) || (is.character(x) && length(x) == 0)) y else x
}

rtocodex_init_test_env <- function(config, root) {
  env <- new.env(parent = baseenv())
  env$.test_config <- config
  env$pkg_root <- function() root
  env$pkg_file <- function(...) file.path(root, ...)
  env$get_test_config <- function() env$.test_config
  env$set_test_config <- function(value) {
    env$.test_config <- value
  }
  env
}

rtocodex_load_helpers <- function(env, root) {
  helper_dir <- file.path(root, "tests", "unit", "helpers")
  if (!dir.exists(helper_dir)) {
    return(invisible(NULL))
  }
  helper_files <- sort(list.files(helper_dir, pattern = "\\.R$", full.names = TRUE))
  for (helper in helper_files) {
    sys.source(helper, envir = env)
  }
  invisible(NULL)
}

rtocodex_load_test_files <- function(env, root) {
  test_dir <- file.path(root, "tests", "unit")
  if (!dir.exists(test_dir)) {
    stop("Missing tests/unit directory at ", test_dir)
  }
  pattern <- "^test_.*\\.R$"
  test_files <- sort(list.files(test_dir, pattern = pattern, full.names = TRUE))
  for (tf in test_files) {
    sys.source(tf, envir = env)
  }
  invisible(NULL)
}

rtocodex_run_unit_tests <- function(config = list(), lib_path = NULL) {
  root <- normalizePath(config$root %||% rtocodex_find_package_root(), winslash = "/", mustWork = TRUE)
  driver <- config$driver %||% "embedded"
  config$root <- root
  config$driver <- driver

  original_libpaths <- .libPaths()
  on.exit(.libPaths(original_libpaths), add = TRUE)
  if (!is.null(lib_path) && nzchar(lib_path)) {
    lib_path <- normalizePath(lib_path, winslash = "/", mustWork = FALSE)
    .libPaths(unique(c(lib_path, original_libpaths)))
  }

  if ("package:RtoCodex" %in% search()) {
    detach("package:RtoCodex", unload = TRUE, character.only = TRUE)
  }
  suppressPackageStartupMessages(library("RtoCodex", character.only = TRUE))
  on.exit({
    if ("package:RtoCodex" %in% search()) {
      detach("package:RtoCodex", unload = TRUE, character.only = TRUE)
    }
  }, add = TRUE)

  env <- rtocodex_init_test_env(config, root)
  harness_path <- file.path(root, "tests", "unit", "test_harness.R")
  if (!file.exists(harness_path)) {
    stop("Missing harness definition at ", harness_path)
  }
  sys.source(harness_path, envir = env)
  rtocodex_load_helpers(env, root)
  rtocodex_load_test_files(env, root)

  results <- env$run_registered_tests()
  env$print_test_summary(results)
  failed <- any(vapply(results, function(entry) identical(entry$status, "fail"), logical(1)))
  list(results = results, failed = failed, config = config)
}
