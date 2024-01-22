source("R/Tay.R")

#' The Csn function
#'
#' @param n The number of Taylor iterations
#' @param q a real number in \eqn{(2,\infty)}
#' @param eps a real number in \eqn{(0,\infty)}
#'
#' @return a neural network that approximates cos

Csn <- function(n, q, eps) {
  Tay("cos", n, q, eps) -> return_network
  return(return_network)
}
