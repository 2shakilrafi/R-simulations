source("comp.R")
source("affn.R")
source("i.R")
source("aux_fun.R")
source("realization.R")
source("activations.R")

c_k <- function(k) {
  return <- 2^{
    1 - 2 * k
  }
  return(return)
}

B <- function() {
  contents <- c(0, -1 / 2, -1, 0)
  return <- matrix(contents, nrow = 4, ncol = 1)
  return(return)
}

C_k <- function(k) {
  contents <- c(-c_k(k), 2 * c_k(k), -c_k(k), 1)
  return <- matrix(contents, nrow = 1, ncol = 4)
  return(return)
}

A_k <- function(k) {
  row_1 <- c(2, -4, 2, 0)
  row_2 <- c(2, -4, 2, 0)
  row_3 <- c(2, -4, 2, 0)
  row_4 <- c(-c_k(k), 2 * c_k(k), -c_k(k), 1)
  return <- matrix(rbind(row_1, row_2, row_3, row_4), nrow = 4, ncol = 4)
  return(return)
}

A <- function() {
  contents <- c(1, 1, 1, 1)
  return <- matrix(contents, nrow = 4, ncol = 1)
  return(return)
}

Phi_k <- function(k) {
  if (k == 1) {
    return_network <- (aff(C_k(1), 0) %•% i(4)) %•% aff(A(), B())
    return(return_network)
  }
  if (k >= 2) {
    return_network <- aff(C_k(k), 0) %•% i(4)
    for (j in (k - 1):1) {
      intermediate_network <- aff(A_k(j), B()) %•% i(4)
      return_network <- return_network %•% intermediate_network
    }
    return_network <- return_network %•% aff(A(), B())
    return(return_network)
  }
}
