# tests/unit/test_harness.R implements a tiny assertion DSL so agents can write
# deterministic regression tests without depending on external helper packages.

.test_registry <- list()

register_test <- function(name, fn) {
  stopifnot(is.character(name), length(name) == 1, nzchar(name))
  stopifnot(is.function(fn))
  .test_registry[[length(.test_registry) + 1L]] <<- list(name = name, fn = fn)
  invisible(NULL)
}

format_value <- function(x) {
  if (length(x) <= 4 && (is.numeric(x) || is.character(x) || is.logical(x))) {
    return(paste(c(x), collapse = ", "))
  }
  paste(utils::capture.output(str(x)), collapse = " ")
}

fail_test <- function(message) {
  stop(message, call. = FALSE)
}

assert_equal <- function(actual, expected, info = NULL) {
  if (!identical(actual, expected)) {
    msg <- sprintf("Expected %s but observed %s.", format_value(expected), format_value(actual))
    if (!is.null(info)) {
      msg <- paste(msg, info)
    }
    fail_test(msg)
  }
  invisible(TRUE)
}

assert_true <- function(value, info = NULL) {
  if (!isTRUE(value)) {
    msg <- "Assertion failed: value is not TRUE."
    if (!is.null(info)) {
      msg <- paste(msg, info)
    }
    fail_test(msg)
  }
  invisible(TRUE)
}

assert_false <- function(value, info = NULL) {
  if (!identical(value, FALSE)) {
    msg <- "Assertion failed: value is not FALSE."
    if (!is.null(info)) {
      msg <- paste(msg, info)
    }
    fail_test(msg)
  }
  invisible(TRUE)
}

assert_match <- function(value, pattern, info = NULL) {
  if (!is.character(value) || length(value) != 1L || !grepl(pattern, value)) {
    msg <- sprintf("Assertion failed: '%s' does not match /%s/.", format_value(value), pattern)
    if (!is.null(info)) {
      msg <- paste(msg, info)
    }
    fail_test(msg)
  }
  invisible(TRUE)
}

assert_length <- function(object, expected, info = NULL) {
  actual <- length(object)
  if (!identical(actual, expected)) {
    msg <- sprintf("Assertion failed: expected length %s but saw %s.", expected, actual)
    if (!is.null(info)) {
      msg <- paste(msg, info)
    }
    fail_test(msg)
  }
  invisible(TRUE)
}

skip_test <- function(message = "Test skipped.") {
  cond <- structure(list(message = message, call = NULL), class = c("rtocodex_skip_condition", "condition"))
  stop(cond)
}

skip_if <- function(condition, message = "Condition triggered skip.") {
  if (isTRUE(condition)) {
    skip_test(message)
  }
  invisible(NULL)
}

test_case <- function(name, code) {
  block <- substitute(code)
  caller <- parent.frame()
  register_test(name, function() eval(block, envir = caller))
}

run_registered_tests <- function() {
  results <- vector("list", length(.test_registry))
  for (i in seq_along(.test_registry)) {
    entry <- .test_registry[[i]]
    status <- "pass"
    message <- ""
    start <- proc.time()[[3L]]
    tryCatch(
      {
        entry$fn()
      },
      rtocodex_skip_condition = function(e) {
        status <<- "skip"
        message <<- conditionMessage(e)
      },
      error = function(e) {
        status <<- "fail"
        message <<- conditionMessage(e)
      }
    )
    duration <- proc.time()[[3L]] - start
    results[[i]] <- list(name = entry$name, status = status, message = message, duration = duration)
  }
  results
}

print_test_summary <- function(results) {
  total <- length(results)
  counts <- table(factor(vapply(results, `[[`, character(1), "status"), levels = c("pass", "skip", "fail")))
  cat(sprintf("Tests: %d total | %d passed | %d skipped | %d failed\n", total, counts[["pass"]], counts[["skip"]], counts[["fail"]]))
  failures <- Filter(function(entry) identical(entry$status, "fail"), results)
  if (length(failures) > 0) {
    cat("Failures:\n")
    for (entry in failures) {
      cat(sprintf(" - %s: %s\n", entry$name, entry$message))
    }
  }
  invisible(NULL)
}
