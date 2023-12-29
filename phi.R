source("Phi_k.R")
source("i.R")
source("affn.R")


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

Phi_v <- Vectorize(Phi)
