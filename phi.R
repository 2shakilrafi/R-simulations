source("Phi_k.R")
source("i.R")
source("affn.R")


Phi <- function(eps) {
  M <- 0.5 * ((1 / eps) |> log2() - 1) |>
    ceiling()

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
