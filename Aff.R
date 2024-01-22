#' The Aff function
#'
#' @param W An \eqn{m \times n} matrix representing the weight of the affine neural network
#' @param b An \eqn{m \times 1} vector representing the bias of the affine neural network
#'
#' @return returns the network \eqn{((W,b))} representing an affine neural network.
#' See also \code{\link{Cpy}} and \code{\link{Sum}}.

Aff <- function(W, b) {
  if (W |> is.matrix() == FALSE) (W |> matrix() -> W)
  if (b |> is.matrix() == FALSE) (b |> matrix() -> b)
  list(list(W = W, b = b)) -> return_network
  return(return_network)
}

#' The Cpy network,
#'
#' @param n number of copies to make.
#' @param k the size of the input vector.
#'
#' @return Returns an affine network that makes a concatenated vector that is \eqn{n}
#' copies of the input vector of size \eqn{k}. See \code{\link{Aff}} and \code{\link{Sum}}.

Cpy <- function(n, k) {
  k |> diag() -> W
  for (i in 2:n) {
    W |> rbind(k |> diag()) -> W
  }
  0 |> matrix(n * k) -> b
  list(list(W = W, b = b)) -> return_network
  return(return_network)
}

#' The Sum neural network
#'
#' @param n number of copies of a certain vector to be summed.
#' @param k the size of the summation vector.
#'
#' @return An affine neural network that will take a vector of size \eqn{n \times k} and return
#' the summation vector that is of length \eqn{k}. See also \code{\link{Aff}} and \code{\link{Cpy}}.

Sum <- function(n, k) {
  k |> diag() -> W
  for (i in 2:n) {
    W |> cbind(k |> diag()) -> W
  }
  0 |> matrix(k) -> b
  list(list(W = W, b = b)) -> return_network

  return(return_network)
}
