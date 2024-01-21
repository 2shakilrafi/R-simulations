#' The aff function
#'
#' @param W an m by n matrix representing the weight of the affine neural network
#' @param b a m by 1 vector representing the bias of the affine neural network
#'
#' @return returns the network \eqn{((W,b))} representing an affine neural network

aff <- function(W, b) {
  if (W |> is.matrix() == FALSE) (W |> matrix() -> W)
  if (b |> is.matrix() == FALSE) (b |> matrix() -> b)

  return(list(list(W = W, b = b)))
}

#' The cpy network, a network that takes a vector of length k and returns a concatenated
#' vector consisting of n copies of said vector
#'
#' @param n number of copies to make
#' @param k the size of the input vector
#'
#' @return returns an affine network that makes a concatenated vector that is n
#' copies of the input vector

cpy <- function(n, k) {
  k |> diag() -> W
  for (i in 2:n) {
    W |> rbind(k |> diag()) -> W
  }
  0 |> matrix(n * k) -> b

  return(list(list(W = W, b = b)))
}

#' The sum neural network
#'
#' @param n number of copies of a certain vector to be summed
#' @param k the size of the summation vector
#'
#' @return an affine neural network that will take a vector of size n*k and return
#' the summation vector that is of length k

sm <- function(n, k) {
  k |> diag() -> W
  for (i in 2:n) {
    W |> cbind(k |> diag()) -> W
  }
  0 |> matrix(k) -> b
  return(list(list(W = W, b = b)))
}
