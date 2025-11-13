# RtoCodex package-level documentation and native registration hints.
# This file teaches roxygen2 how to document the package and ensures
# the compiled shared object is loaded via useDynLib.

#' RtoCodex: environment-sensitive native code experiments
#'
#' Provides a single user-facing helper (`add1tester()`) whose compiled
#' implementations are intentionally sensitive to toolchain toggles so
#' we can observe differences between local macOS and the universal
#' Ubuntu container.
#'
#' @name RtoCodex-package
#' @keywords internal
#' @useDynLib RtoCodex, .registration = TRUE
"_PACKAGE"
