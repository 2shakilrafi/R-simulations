
source("comp.R")
source("aux_fun.R")

#' The left and right scalar multiplication fuctions
#'
#' @param a a real number
#' @param nu a neural network
#'
#' @return returns a neural network that realizes as a.f(x) and f(a.x) 
#' under ReLU activation respectively

`%|>%` <- function(a, nu) {
  constant_matrix_size <- nu |> out()
  multiplier_network <- list()
  W <- a |> diag(constant_matrix_size)
  b <- 0 |> matrix(constant_matrix_size)
  multiplier_network[[1]] <- list(W = W, b = b)
  return_network <- multiplier_network |> comp(nu)
  return(return_network)
}

`%<|%` <- function(nu, a) {
  constant_matrix_size <- nu |> inn()
  multiplier_network <- list()
  W <- a |> diag(constant_matrix_size)
  b <- 0 |> matrix(constant_matrix_size)
  multiplier_network[[1]] <- list(W = W, b = b)
  return_network <- multiplier_network |> comp(nu)
  return(return_network)
}
