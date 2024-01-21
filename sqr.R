source("affn.R")
source("comp.R")
source("affn.R")
source("nn_sum.R")
source("Phi.R")
source("aux_fun.R")

#' The Sqr network
#'
#' @param q parameter for the Sqr network. \eqn{2 \in (2,\infty)}
#' @param eps parameter for the Sqr network. \eqn{eps \in (0,1]}. You may
#' choose epsilon to be greater than 1 but that leads to large errors
#'
#' @return a neural network that approximates the square of a real number.upon
#' instantiation with ReLU.
#' @example Sqr(2.1,0.1)
#' @example Sqr(0.05, 0.05)

Sqr <- function(q, eps) {
  2^(-2 / (q - 2)) * eps^(q / (q - 2)) -> delta
  (eps / 2)^(1 / (q - 2)) -> alpha

  (0.5 * log2(1 / eps) - 1) |> ceiling() -> M

  if (M <= 0) 1 else M -> M

  (aff(alpha^(-2), 0) |> comp(Phi(delta))) |>
    comp(aff(alpha, 0)) -> first_summand

  (aff(alpha^(-2), 0) |> comp(Phi(delta))) |>
    comp(aff(-alpha, 0)) -> second_summand

  first_summand |>
    nn_sum(second_summand) -> return_network

  return(return_network)
}
