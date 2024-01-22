source("Aff.R")
source("comp.R")
source("nn_sum.R")
source("Phi.R")
source("scalar_mult.R")

c(1, 1) |> matrix(1, 2) -> A_1
c(1, 0) |> matrix(1, 2) -> A_2
c(0, 1) |> matrix(1, 2) -> A_3

#' The Prd network
#'
#' @param q parameter for Prd
#' @param eps parameter for Prd
#'
#' @return A neural network that takes in x and y and approximately
#' returns xy under ReLU activation

Prd <- function(q, eps) {
  0.5 |>
    slm(Sqr(q, eps)) |>
    comp(aff(A_1, 0)) |>
    nn_sum(-0.5 |> slm(Sqr(q, eps)) |> comp(Aff(A_2, 0))) |>
    nn_sum(-0.5 |> slm(Sqr(q, eps)) |> comp(Aff(A_3, 0))) -> return_network
  return(return_network)
}
