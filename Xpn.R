source("R/Tay.R")

#' The Xpn function
#'
#' @param n number of Taylor iterations
#' @param q real number in (2,\infty)
#' @param eps real number in (0, \infty)
#'
#' @return a neural network that approximates e^x for real x

Xpn <- function(n, q, eps) {
  return(Tay("exp", n, q, eps))
}
