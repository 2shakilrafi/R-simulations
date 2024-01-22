source("Prd.R")
source("Aff.R")
source("stacking.R")
source("Tun.R")
source("aux_fun.R")

#' The Pwr function
#'
#' @param q parameter for the Pwr network
#' @param eps parameter for the Pwr network
#' @param exponent the exponent which the Pwr network will approximate
#'
#' @return a neural network that approximates raising a number to exponent
#' under ReLU activation
#' @example Pwr(2.05, 0.05, 3)
#' @example Pwr(2.1,0.1, 3)

Pwr <- function(q, eps, exponent) {
  if (exponent == 0) {
    Aff(0, 1) -> return_network
    return(return_network)
  } else if (exponent >= 1) {
    Cpy(2, 1) -> first_third
    Pwr(q, eps, exponent - 1) |> stk(Pwr(q, eps, exponent - 1) |> dep() |> Tun()) -> mid_third
    Prd(q, eps) -> last_third
    last_third |>
      comp(mid_third) |>
      comp(first_third) -> return_network
  } else {
    return("Invalid exponent")
  }
  return(return_network)
}
