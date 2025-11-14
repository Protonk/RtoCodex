# Vendored probe runtime loader: sources sandbox-capabilities runtime helpers so
# harness scripts (matrix, fuzz, probes) can stamp runtime metadata into
# artifacts without adding new package dependencies.

if (!exists("find_repo_root", mode = "function")) {
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
}

load_probe_runtime <- function(runtime_dir = NULL) {
  if (is.null(runtime_dir) || !nzchar(runtime_dir)) {
    repo_root <- find_repo_root()
    runtime_dir <- file.path(repo_root, "vendor", "probe-runtime")
  }
  runtime_dir <- normalizePath(runtime_dir, winslash = "/", mustWork = FALSE)

  locate <- function(path_candidates) {
    for (candidate in path_candidates) {
      candidate_path <- file.path(runtime_dir, candidate)
      if (file.exists(candidate_path)) {
        return(candidate_path)
      }
    }
    NULL
  }

  runtime_file <- locate(c("probe_runtime.R", file.path("runtime", "r", "probe_runtime.r")))
  if (is.null(runtime_file)) {
    stop(sprintf("Unable to locate probe runtime entrypoint under %s.", runtime_dir), call. = FALSE)
  }
  source(runtime_file, chdir = TRUE)

  version_file <- locate(c("VERSION", file.path("runtime", "r", "VERSION")))
  runtime_version <- NULL
  if (!is.null(version_file)) {
    version_line <- readLines(version_file, n = 1, warn = FALSE)
    runtime_version <- trimws(version_line[1])
  } else if (exists("RUNTIME_VERSION", inherits = TRUE)) {
    runtime_version <- get("RUNTIME_VERSION", inherits = TRUE)
  }

  info <- list(
    runtime_dir = runtime_dir,
    runtime_file = runtime_file,
    runtime_version = runtime_version
  )
  options(RtoCodex.probe_runtime = info)
  invisible(info)
}
