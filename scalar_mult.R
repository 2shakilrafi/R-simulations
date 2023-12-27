#' ---
#' title: "Scalar Multiplication of Neural Networks"
#' author: "Shakil Rafi"
#' output: pdf_document
#' ---


source("comp.R")
source("aux_fun.R")

`%|>%` <- function(a, nu) {
  constant_matrix_size <- out(nu)
  multiplier_network <- list()
  W <- diag(a, constant_matrix_size, constant_matrix_size)
  b <- matrix(0, nrow = constant_matrix_size, ncol = 1)
  multiplier_network[[1]] <- list(W = W, b = b)
  return_network <- multiplier_network %•% nu
  return(return_network)
}

`%<|%` <- function(nu, a) {
  constant_matrix_size <- inn(nu)
  multiplier_network <- list()
  W <- diag(a, constant_matrix_size, constant_matrix_size)
  b <- matrix(0, nrow = constant_matrix_size, ncol = 1)
  multiplier_network[[1]] <- list(W = W, b = b)
  return_network <- multiplier_network %•% nu
  return(return_network)
}
