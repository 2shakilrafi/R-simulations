source("Phi_k.R")
source("i.R")
source("affn.R")


Phi <- function(eps) {
  M <- 0.5 * ((1 / eps) |> log2() - 1) |>
    ceiling()

  if (M == 1) {
    return_network <- aff(C_k(1), 0) |>
      comp(i(4)) |>
      comp(aff(A(), B()))
    return(return_network)
  }

  if (M >= 2) {
    return_network <- aff(C_k(M), 0) |> comp(i(4))
    for (j in (M - 1):1) {
      intermediate_network <- aff(A_k(j), B()) |> comp(i(4))
      return_network <- return_network |> comp(intermediate_network)
    }
    return_network <- return_network |> comp(aff(A(), B()))
    return(return_network)
  }
}
