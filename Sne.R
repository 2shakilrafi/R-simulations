source("R/Tay.R")

#' The Sne function
#'
#' @param n the number of Taylor iterations
#' @param q a real number between (2,\infty)
#' @param eps a real number between (0,\infty)
#'
#' @return a neural network that approximates Sin.

Sne <- function(n, q, eps) {
  return(Tay("sin", n, q, eps))
}
