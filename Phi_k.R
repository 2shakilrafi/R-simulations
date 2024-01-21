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
  2^{
    1 - 2 * k
  } -> result
  return(result)
}

B <- function() {
  c(0, -1 / 2, -1, 0) |> matrix() -> result
  return(result)
}

C_k <- function(k) {
  c(-c_k(k), 2 * c_k(k), -c_k(k), 1) |> matrix(1, 4) -> result
  return(result)
}

A_k <- function(k) {
  return <- c(2, 2, 2, -c_k(k)) |>
    c(-4, -4, -4, 2 * c_k(k)) |>
    c(2, 2, 2, -c_k(k)) |>
    c(0, 0, 0, 1) |>
    matrix(4, 4) -> result
  return(result)
}

A <- function() {
  c(1, 1, 1, 1) |> matrix(4, 1) -> result
  return(result)
}

#' The Phi_k function
#'
#' @param k an integer \eqn{k \in (2,\infty)}
#'
#' @return The Phi_k neural network
#' @references Grohs, P., Hornung, F., Jentzen, A. et al.
#' Space-time error estimates for deep neural network approximations
#' for differential equations. Adv Comput Math 49, 4 (2023).
#' https://doi.org/10.1007/s10444-022-09970-2
#'
Phi_k <- function(k) {
  if (k == 1) {
    C_k(1) |>
      aff(0) |>
      comp(i(4)) |>
      comp(aff(A(), B())) -> return_network
    return(return_network)
  }
  if (k >= 2) {
    C_k(k) |>
      aff(0) |>
      comp(i(4)) -> return_network
    for (j in (k - 1):1) {
      A_k(j) |>
        aff(B()) |>
        comp(i(4)) -> intermediate_network
      return_network |> comp(intermediate_network) -> return_network
    }
    return_network |> comp(A() |> aff(B())) -> return_network
    return(return_network)
  }
}
