source("R/comp.R")
source("R/aux_fun.R")

#' The left  scalar multiplication fuction, slm.
#'
#' @param a a real number
#' @param nu a neural network
#'
#' @return returns a neural network that is \eqn{a \triangleright \nu}. This
#' instantiates as \eqn{a \cdot f(x)}.under continuous function activation .
#'
#' \emph{Note:} We will have two kinds of operations, an infix \code{\link{%slm%}} and
#' a prefix version, \code{\link{slm}}

slm <- function(a, nu) {
  nu |> out() -> constant_matrix_size
  list() -> multiplier_network
  a |> diag(constant_matrix_size) -> W
  0 |> matrix(constant_matrix_size) -> b
  list(W = W, b = b) -> multiplier_network[[1]]
  multiplier_network |> comp(nu) -> return_network
  return(return_network)
}

#' The right scalar multiplication function
#'
#' @param nu A neural network
#' @param a A real number.
#'
#' @return returns a neural network that is \eqn{\nu \triangleleft a}. This
#' instantiates as \eqn{f(a \cdot x)}.under continuous function activation .

srm <- function(nu, a) {
  nu |> inn() -> constant_matrix_size
  list() -> multiplier_network
  a |> diag(constant_matrix_size) -> W
  0 |> matrix(constant_matrix_size) -> b
  list(W = W, b = b) -> multiplier_network[[1]]
  nu |> comp(multiplier_network) -> return_network
  return(return_network)
}

#' The left  scalar multiplication infix fuction, `%slm%`.
#'
#' @param a a real number
#' @param nu a neural network
#'
#' @return returns a neural network that is \eqn{a \triangleright \nu}. This
#' instantiates as \eqn{a \cdot f(x)}.under continuous function activation .
#'
#' \emph{Note:} This is the infix version of \code{\link{slm}}
#'
`%slm%` <- function(a, nu) {
  nu |> out() -> constant_matrix_size
  list() -> multiplier_network
  a |> diag(constant_matrix_size) -> W
  0 |> matrix(constant_matrix_size) -> b
  list(W = W, b = b) -> multiplier_network[[1]]
  multiplier_network |> comp(nu) -> return_network
  return(return_network)
}

#' The right scalar multiplication function
#'
#' @param nu A neural network
#' @param a A real number.
#'
#' @return Returns a neural network that is \eqn{\nu \triangleleft a}. This
#' instantiates as \eqn{f(a \cdot x)}.under continuous function activation . This
#' is the infix version of \code{\link{srm}}

`%srm%` <- function(nu, a) {
  nu |> inn() -> constant_matrix_size
  list() -> multiplier_network
  a |> diag(constant_matrix_size) -> W
  0 |> matrix(constant_matrix_size) -> b
  list(W = W, b = b) -> multiplier_network[[1]]
  nu |> comp(multiplier_network) -> return_network
  return(return_network)
}
