source("affn.R")
source("comp.R")
source("affn.R")
source("nn_sum.R")
source("Phi.R")

Sqr <- function(q, eps) {
  delta <- 2^(-2 / (q - 2)) * eps^(q / (q - 2))
  alpha <- (eps / 2)^(1 / (q - 2))

  M <- (0.5 * log2(1 / eps) - 1) |>
    ceiling()
  
  M <- if (M <= 0) 1 else M

  first_summand <- (aff(alpha^(-2), 0) %•% Phi(eps)) |>
    comp(aff(alpha, 0))
  
  second_summand <- (aff(alpha^(-2), 0) %•% Phi(eps)) |>
    comp(aff(-alpha, 0))
  
  return_network <- first_summand |>
    nn_sum(second_summand)

  return(return_network)
}
