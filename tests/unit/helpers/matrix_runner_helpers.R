# tests/unit/helpers/matrix_runner_helpers.R loads scripts/run_matrix.R into an
# isolated environment so unit tests can exercise its helpers without invoking
# the CLI entrypoint.

locate_run_matrix_script <- function() {
  root <- pkg_root()
  candidates <- c(
    file.path(root, "scripts", "run_matrix.R"),
    file.path(root, "RtoCodex", "scripts", "run_matrix.R")
  )
  for (candidate in candidates) {
    candidate_abs <- tryCatch(normalizePath(candidate, mustWork = FALSE), error = function(e) candidate)
    if (file.exists(candidate_abs)) {
      return(candidate_abs)
    }
  }
  stop("Unable to locate scripts/run_matrix.R from tests directory.")
}

load_run_matrix_env <- function() {
  script_path <- locate_run_matrix_script()
  expressions <- parse(script_path)
  keep <- vapply(
    expressions,
    function(expr) {
      !(is.call(expr) && identical(expr[[1L]], as.name("main")) && length(expr) == 1L)
    },
    logical(1)
  )
  env <- new.env(parent = baseenv())
  repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(repo_root)
  stub_cmd_args <- function(trailingOnly = FALSE) {
    if (isTRUE(trailingOnly)) {
      return(character())
    }
    c(sprintf("--file=%s", script_path))
  }
  assign("commandArgs", stub_cmd_args, envir = env)
  assign(
    "source",
    function(..., local = FALSE) {
      base::source(..., local = env)
    },
    envir = env
  )
  for (expr in expressions[keep]) {
    eval(expr, envir = env)
  }
  if (exists("source", envir = env, inherits = FALSE)) {
    rm("source", envir = env)
  }
  env
}

with_mocked_command_args <- function(env, args, code) {
  stub <- function(trailingOnly = FALSE) {
    if (isTRUE(trailingOnly)) {
      return(args)
    }
    c("Rscript", "scripts/run_matrix.R", args)
  }
  has_original <- exists("commandArgs", envir = env, inherits = FALSE)
  if (has_original) {
    original <- get("commandArgs", envir = env, inherits = FALSE)
  }
  assign("commandArgs", stub, envir = env)
  on.exit({
    if (has_original) {
      assign("commandArgs", original, envir = env)
    } else {
      rm("commandArgs", envir = env)
    }
  }, add = TRUE)
  force(code)
}
