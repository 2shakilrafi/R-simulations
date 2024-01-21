source("comp.R")
source("aux_fun.R")

#' The left and right scalar multiplication fuctions
#'
#' @param a a real number
#' @param nu a neural network
#'
#' @return returns a neural network that realizes as a.f(x) and f(a.x)
#' under ReLU activation respectively
#'
#' @remark Note we will have two kinds of operations, an infix and
#' a prefix version.

`%|>%` <- function(a, nu) {
  nu |> out() -> constant_matrix_size
  list() -> multiplier_network
  a |> diag(constant_matrix_size) -> W
  0 |> matrix(constant_matrix_size) -> b
  list(W = W, b = b) -> multiplier_network[[1]]
  multiplier_network |> comp(nu) -> return_network
  return(return_network)
}

`%<|%` <- function(nu, a) {
  nu |> inn() -> constant_matrix_size
  list() -> multiplier_network
  a |> diag(constant_matrix_size) -> W
  0 |> matrix(constant_matrix_size) -> b
  list(W = W, b = b) -> multiplier_network[[1]]
  nu |> comp(multiplier_network) -> return_network
  return(return_network)
}

slm <- function(a, nu) {
  nu |> out() -> constant_matrix_size
  list() -> multiplier_network
  a |> diag(constant_matrix_size) -> W
  0 |> matrix(constant_matrix_size) -> b
  list(W = W, b = b) -> multiplier_network[[1]]
  multiplier_network |> comp(nu) -> return_network
  return(return_network)
}

srm <- function(nu, a) {
  nu |> inn() -> constant_matrix_size
  list() -> multiplier_network
  a |> diag(constant_matrix_size) -> W
  0 |> matrix(constant_matrix_size) -> b
  list(W = W, b = b) -> multiplier_network[[1]]
  nu |> comp(multiplier_network) -> return_network
  return(return_network)
}
