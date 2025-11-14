#!/usr/bin/env Rscript

# scripts/run_probes.R executes every probes/cap_*.R helper (or a selected cap),
# stores the single-line status in artifacts/caps, and prints JSON summarizing
# the capability map so harnesses can branch on sandbox quirks.

trim_first_line <- function(lines) {
  if (!length(lines)) {
    return("")
  }
  trimmed <- trimws(lines)
  trimmed <- trimmed[nzchar(trimmed)]
  if (!length(trimmed)) "" else trimmed[[1L]]
}

escape_json <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  x
}

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- list(probes = "probes", artifacts = "artifacts/caps", cap = NULL)
  for (arg in args) {
    if (startsWith(arg, "--probes=")) {
      opts$probes <- sub("^--probes=", "", arg)
    } else if (startsWith(arg, "--artifacts=")) {
      opts$artifacts <- sub("^--artifacts=", "", arg)
    } else if (startsWith(arg, "--cap=")) {
      opts$cap <- sub("^--cap=", "", arg)
    } else if (arg %in% c("-h", "--help")) {
      cat("Usage: Rscript scripts/run_probes.R [--cap=name] [--probes=dir] [--artifacts=dir]\n")
      quit(status = 0L)
    }
  }
  opts
}

list_capabilities <- function(dir) {
  files <- Sys.glob(file.path(dir, "cap_*.R"))
  if (!length(files)) {
    return(character())
  }
  sort(sub("\\.R$", "", basename(files)))
}

run_probe <- function(cap, dirs) {
  script <- file.path(dirs$probes, paste0(cap, ".R"))
  if (!file.exists(script)) {
    stop(sprintf("Probe script %s not found", script))
  }
  err_file <- tempfile(paste0("probe_", cap, "_stderr_"))
  on.exit(unlink(err_file), add = TRUE)
  output <- tryCatch(
    system2("Rscript", c(script), stdout = TRUE, stderr = err_file),
    error = function(e) {
      structure(character(), status = 127L, stderr = conditionMessage(e))
    }
  )
  exit_code <- attr(output, "status")
  if (is.null(exit_code)) exit_code <- 0L
  stderr_text <- if (!is.null(attr(output, "stderr"))) {
    attr(output, "stderr")
  } else if (file.exists(err_file)) {
    trim_first_line(readLines(err_file, warn = FALSE))
  } else {
    ""
  }
  value <- trim_first_line(output)
  if (!nzchar(value)) {
    value <- if (exit_code == 0L) "unknown" else "error"
  }
  list(value = value, exit_code = exit_code, stderr = stderr_text)
}

write_cap_file <- function(cap, value, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, paste0(cap, ".txt"))
  writeLines(value, con = path)
}

write_status_json <- function(results, path) {
  if (!length(results)) {
    json <- '{\n}\n'
    writeLines(json, path)
    cat(json)
    return(invisible(NULL))
  }
  items <- vapply(names(results), function(name) {
    value <- results[[name]]$value
    sprintf('  "%s": "%s"', escape_json(name), escape_json(value))
  }, character(1))
  json <- paste0('{\n', paste(items, collapse = ',\n'), '\n}\n')
  writeLines(json, path)
  cat(json)
}

opts <- parse_args()
if (!dir.exists(opts$probes)) {
  stop(sprintf("Probe directory %s not found", opts$probes))
}
caps <- if (!is.null(opts$cap)) {
  opts$cap
} else {
  list_capabilities(opts$probes)
}
if (!length(caps)) {
  stop("No capability probes found.")
}
results <- list()
any_failure <- FALSE
for (cap in caps) {
  res <- run_probe(cap, opts)
  write_cap_file(cap, res$value, opts$artifacts)
  results[[cap]] <- res
  if (res$exit_code != 0) {
    any_failure <- TRUE
    msg <- sprintf("Probe %s exited with status %s", cap, res$exit_code)
    if (nzchar(res$stderr)) {
      msg <- paste(msg, res$stderr, sep = ": ")
    }
    message(msg)
  }
}
write_status_json(results, file.path(opts$artifacts, "caps.json"))
quit(status = if (any_failure) 1L else 0L)
