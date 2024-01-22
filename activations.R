#' ---
#' title: "Activation Functions ReLU and Sigmoid"
#' author: "Shakil Rafi"
#' date: "May 3rd, 2014"
#' ---


#' The ReLU activation function
#'
#' @param x a real number that is the input to our ReLU function
#'
#' @return the output of the standard ReLU function

ReLU <- function(x) {
  return(x |> max(0))
}

#' The Sigmoid activation function
#'
#' @param x a real number that is the input to our Sigmoid function
#'
#' @return the output of a standard sigmoid function

Sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}

#' The tanh activation function
#'
#' @param x a real number
#'
#' @return the tanh of x

Tanh <- function(x) {
  return(x |> tanh())
}
