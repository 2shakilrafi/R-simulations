#' The ReLU activation function
#'
#' @param x A real number that is the input to our ReLU function.
#'
#' @return The output of the standard ReLU function. See also \code{\link{Sigmoid}}.
#' and \code{\link{Tanh}}

ReLU <- function(x) {
  return(x |> max(0))
}

#' The Sigmoid activation function.
#'
#' @param x a real number that is the input to our Sigmoid function
#'
#' @return the output of a standard Sigmoid function. See also \code{\link{Tanh}}.
#' and \code{\link{ReLU}}

Sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}

#' The tanh activation function
#'
#' @param x a real number
#'
#' @return the tanh of x. See also \code{\link{Sigmoid}} and \code{\link{ReLU}}.

Tanh <- function(x) {
  return(x |> tanh())
}
