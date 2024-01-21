source("Tay.R")

#' The Csn function
#'
#' @param n The number of Taylor iterations
#' @param q a real number in (2,\infty)
#' @param eps a real number in (0,\infty)
#'
#' @return a neural network that approximates cos

Csn <- function(n, q, eps) {
  return(Tay("cos", n, q, eps))
}
