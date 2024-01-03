source("affn.R")
source("comp.R")
source("affn.R")
source("nn_sum.R")
source("Phi.R")
source("scalar_mult.R")

A_1 <- c(1, 1) |> matrix(1, 2)
A_2 <- c(1, 0) |> matrix(1, 2)
A_3 <- c(0, 1) |> matrix(1, 2)

#' The Prd network
#'
#' @param q parameter for Prd
#' @param eps parameter for Prd
#'
#' @return A neural network that takes in x and y and approximately
#' returns xy under ReLU activation

Prd <- function(q, eps) {
  return_network <- ((1 / 2) %|>% (Sqr(q, eps) %•% aff(A_1, 0))) |>
    nn_sum((-1 / 2) %|>% (Sqr(q, eps) %•% aff(A_2, 0))) |>
    nn_sum((-1 / 2) %|>% (Sqr(q, eps) %•% aff(A_3, 0)))
  return(return_network)
}
