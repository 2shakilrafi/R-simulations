source("affn.R")
source("comp.R")
source("affn.R")
source("nn_sum.R")
source("Phi.R")
source("scalar_mult.R")

A_1 <- matrix(c(1, 1), nrow = 1, ncol = 2)
A_2 <- matrix(c(1, 0), nrow = 1, ncol = 2)
A_3 <- matrix(c(0, 1), nrow = 1, ncol = 2)

Prd <- function(q, eps) {
  first_summand <- (1 / 2) %|>% (Phi(eps) %•% aff(A_1, 0))
  second_summand <- (-1 / 2) %|>% (Phi(eps) %•% aff(A_2, 0))
  third_summand <- (-1 / 2) %|>% (Phi(eps) %•% aff(A_3, 0))
  return_network <- (first_summand %⊕% second_summand) %⊕% third_summand
  return(return_network)
}
