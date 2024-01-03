source("comp.R")
source("affn.R")
source("i.R")
source("aux_fun.R")
source("realization.R")
source("activations.R")

#' The c_k function
#'
#' @param k an integer between (2,\inf)
#'
#' @return the real number 2^{1-2k}
#' @references Grohs, P., Hornung, F., Jentzen, A. et al.
#' Space-time error estimates for deep neural network approximations
#' for differential equations. Adv Comput Math 49, 4 (2023).
#' https://doi.org/10.1007/s10444-022-09970-2

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

#' The Phi_k function
#'
#' @param k an integer between (2,\inf)
#'
#' @return The Phi_k neural network
#' @references Grohs, P., Hornung, F., Jentzen, A. et al.
#' Space-time error estimates for deep neural network approximations
#' for differential equations. Adv Comput Math 49, 4 (2023).
#' https://doi.org/10.1007/s10444-022-09970-2
#'
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
