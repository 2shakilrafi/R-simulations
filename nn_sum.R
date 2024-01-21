source("comp.R")
source("par.R")
source("aux_fun.R")
source("affn.R")

#' Function for calculating neural network sums
#'
#' @param nu_1 a neural network of the type from create_neural_network
#' @param nu_2 a neural network of the type from create_neural_network of the
#' same depth as nu_1
#'
#' @return a neural network that is the neural network sum of nu_1 and nu_2
#'
#' @remark Note we have two versions, an infix version \eqn{\oplus} and a prefix version
#' nn_sum

`%⊕%` <- function(nu_1, nu_2) {
  cpy(2, inn(nu_1)) -> first_third
  nu_1 |> stk(nu_2) -> mid_third
  sm(2, out(nu_1)) -> last_third

  last_third |>
    comp(mid_third) |>
    comp(first_third)
  return(return_network)
}

nn_sum <- function(nu_1, nu_2) {
  cpy(2, inn(nu_1)) -> first_third
  nu_1 |> stk(nu_2) -> mid_third
  sm(2, out(nu_1)) -> last_third

  last_third |>
    comp(mid_third) |>
    comp(first_third)
  return(return_network)
}
