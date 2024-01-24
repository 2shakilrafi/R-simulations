source("R/Tay.R")

#' The Xpn function
#'
#' @param n Number of Taylor iterations.
#' @param q Real number in \eqn{(2,\infty)}.
#' @param eps Real number in \eqn{(0, \infty)}.
#'
#' @return A neural network that approximates \eqn{e^x} for real \eqn{x} when
#' given appropriate \eqn{n,q,\varepsilon} and instnatiated with ReLU
#' activation at point\eqn{x}.
#'
#' @export

Xpn <- function(n, q, eps) {
  if (q <= 2 || eps <= 0) {
    stop("q must be > 2 and eps must be > 0")
  } else if (n %% 1 != 0 || n < 0) {
    stop("The number of Taylor iterations must be a non negative integer")
  } else {
    return(Tay("exp", n, q, eps))
  }
}
