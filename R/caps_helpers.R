# R/caps_helpers.R centralizes reading capability probe artifacts so tests can
# branch on sandbox quirks without rerunning the probes themselves.

.capabilities_pkg_root <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) {
      return(cached)
    }
    target <- "RtoCodex"
    path <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    repeat {
      desc <- file.path(path, "DESCRIPTION")
      if (file.exists(desc)) {
        desc_lines <- readLines(desc, warn = FALSE)
        if (any(grepl(sprintf("^Package:\\s*%s\\s*$", target), desc_lines))) {
          cached <<- path
          return(cached)
        }
      }
      parent <- dirname(path)
      if (identical(parent, path)) {
        cached <<- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
        return(cached)
      }
      path <- parent
    }
  }
})

caps_dir <- function() {
  file.path(.capabilities_pkg_root(), "artifacts", "caps")
}

read_cap <- function(name) {
  stopifnot(is.character(name), length(name) == 1)
  cap_file <- file.path(caps_dir(), sprintf("%s.txt", name))
  if (!file.exists(cap_file)) {
    return(NA_character_)
  }
  line <- readLines(cap_file, warn = FALSE, n = 1)
  if (length(line) == 0) {
    return(NA_character_)
  }
  value <- trimws(line[[1]])
  if (!nzchar(value)) NA_character_ else value
}

needs_cap <- function(name, expected = "supported") {
  identical(read_cap(name), expected)
}
