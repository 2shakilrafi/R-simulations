#' The aff function
#'
#' @param W an m by n matrix representing the weight of the affine neural network
#' @param b a m by 1 vector representing the bias of the affine neural network
#'
#' @return returns the network ((W,b)) representing an affine neural network

aff <- function(W, b) {
  if (W |> is.matrix() == FALSE) W |> matrix()
  if (b |> is.matrix() == FALSE) b |> matrix()
  
  return(list(list(W = W, b = b)))
}

aff_v <- Vectorize(aff)

#' The cpy network, a network that takes a vector of length k and returns a concatenated
#' vector consisting of n copies of said vector
#'
#' @param n number of copies to make
#' @param k the size of the input vector
#'
#' @return returns an affine network that makes a concatenated vector that is n
#' copies of the input vector

cpy <- function(n, k) {
  W <- k |> diag()
  for (i in 2:n) {
    W <- W |> rbind(k |> diag())
  }
  b <- 0 |> matrix(n * k)

  return(list(list(W = W, b = b)))
}

cpy_v <- Vectorize(cpy)

#' The sum neural network
#'
#' @param n number of copies of a certain vector to be summed
#' @param k the size of the summation vector
#'
#' @return an affine neural network that will take a vector of size n*k and return
#' the summation vector that is of length k

sm <- function(n, k) {
  W <- k |> diag()
  for (i in 2:n) {
    W <- W |> cbind(k |> diag())
  }
  b <- 0 |> matrix(k)
  return(list(list(W = W, b = b)))
}

sm_v <- Vectorize(sm)
