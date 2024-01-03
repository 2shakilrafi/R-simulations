source("Phi_k.R")
source("i.R")
source("affn.R")


#' The Phi function
#'
#' @param eps parameter for Phi
#' @references Grohs, P., Hornung, F., Jentzen, A. et al.
#' Space-time error estimates for deep neural network approximations
#' for differential equations. Adv Comput Math 49, 4 (2023).
#' https://doi.org/10.1007/s10444-022-09970-2
#'
#' @return neural network Phi that approximately squares a number between
#' 0 and 1.

Phi <- function(eps) {
  M <- (0.5 * log2(1 / eps) - 1) |>
    ceiling()

  M <- if (M <= 0) 1 else M

  if (M == 1) {
    return_network <- C_k(1) |>
      aff(0) |>
      comp(i(4)) |>
      comp(aff(A(), B()))
    return(return_network)
  }

  if (M >= 2) {
    return_network <- C_k(M) |>
      aff(0) |>
      comp(i(4))
    for (j in (M - 1):1) {
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
