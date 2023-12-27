source("Phi_k.R")
source("i.R")
source("affn.R")


Phi <- function(eps) {
  M <- ceiling(0.5 * log2(1 / eps) - 1)

  if (M == 1) {
    return_network <- (aff(C_k(1), 0) %•% i(4)) %•% aff(A(), B())
    return(return_network)
  }

  if (M >= 2) {
    return_network <- aff(C_k(M), 0) %•% i(4)
    for (j in (M - 1):1) {
      intermediate_network <- aff(A_k(j), B()) %•% i(4)
      return_network <- return_network %•% intermediate_network
    }
    return_network <- return_network %•% aff(A(), B())
    return(return_network)
  }
}
