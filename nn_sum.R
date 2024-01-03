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
#' @remark Note we have two versions, an infix version `%⊕%` and a prefix version
#' nn_sum

`%⊕%` <- function(nu_1, nu_2) {
  first_third <- cpy(2, inn(nu_1))
  mid_third <- nu_1 %⊟% nu_2
  last_third <- sm(2, out(nu_1))
  intermediate_network <- mid_third %•% first_third
  return_network <- last_third %•% intermediate_network

  return(return_network)
}

nn_sum <- function(nu_1, nu_2) {
  first_third <- cpy(2, inn(nu_1))
  mid_third <- nu_1 |> par(nu_2)
  last_third <- sm(2, out(nu_1))

  return_network <- last_third |>
    comp(mid_third) |>
    comp(first_third)

  return(return_network)
}
