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
  return <- c(0, -1 / 2, -1, 0) |>
    matrix(4, 1)
  return(return)
}

C_k <- function(k) {
  return <- c(-c_k(k), 2 * c_k(k), -c_k(k), 1) |>
    matrix(1, 4)
  return(return)
}

A_k <- function(k) {
  return <- c(2, 2, 2, -c_k(k)) |>
    c(-4, -4, -4, 2 * c_k(k)) |>
    c(2, 2, 2, -c_k(k)) |>
    c(0, 0, 0, 1) |>
    matrix(4, 4)
  return(return)
}

A <- function() {
  return <- c(1, 1, 1, 1) |>
    matrix(4, 1)
  return(return)
}

Phi_k <- function(k) {
  if (k == 1) {
    return_network <- C_k(1) |>
      aff(0) |>
      comp(i(4)) |>
      comp(aff(A(), B()))
    return(return_network)
  }
  if (k >= 2) {
    return_network <- C_k(k) |>
      aff(0) |>
      comp(i(4))
    for (j in (k - 1):1) {
      intermediate_network <- A_k(j) |>
        aff(B()) |>
        comp(i(4))
      return_network <- return_network |>
        comp(intermediate_network)
    }
    return_network <- return_network |>
      comp(A() |> aff(B()))
    return(return_network)
  }
}

