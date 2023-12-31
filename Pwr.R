source("Prd.R")
source("affn.R")
source("par.R")
source("Tun.R")
source("aux_fun.R")

Pwr <- function(q,eps, exponent) {
  if (exponent == 0) {
    return_network <- aff(0,1)
    return(return_network)
  }
  
  if (exponent >= 1) {
    first_third <- cpy(2, 1) 
    second_third <- par(Pwr(q, eps, exponent-1), Pwr(q,eps,exponent - 1) |> dep() |> Tun())
    last_third <- Prd(q,eps)
    return_network <- last_third |> comp(second_third) |> comp(first_third)
  }
  return(return_network)
}