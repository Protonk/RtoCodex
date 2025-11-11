#' Increment by one using compiled C
#' @param x Integer vector
#' @return Integer vector incremented by one
#' @export
add1 <- function(x) {
  x <- as.integer(x)
  n <- as.integer(length(x))
  .C("C_add1", x, n, PACKAGE = "RtoCodex")[[1]]
}
