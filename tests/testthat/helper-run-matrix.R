# Matrix runner test helper: loads scripts/run_matrix.R into a fresh environment
# without executing its CLI entrypoint and provides small stubbing utilities.

load_run_matrix_env <- function() {
  script_path <- normalizePath(
    testthat::test_path("..", "..", "scripts", "run_matrix.R"),
    mustWork = TRUE
  )
  expressions <- parse(script_path)
  keep <- vapply(
    expressions,
    function(expr) {
      !(is.call(expr) &&
        identical(expr[[1L]], as.name("main")) &&
        length(expr) == 1L)
    },
    logical(1)
  )
  expressions <- expressions[keep]
  env <- new.env(parent = baseenv())
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  for (expr in expressions) {
    eval(expr, envir = env)
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
  result <- force(code)
  result
}
