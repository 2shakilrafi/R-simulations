source("R/aux_fun.R")

#' The inst or instantiation function
#'
#' @param neural_network. An ordered list of lists, where each element in the list of
#' lists is a pair (W,b) representing the weights and biases of that layer. For
#' detials see:
#'
#' Grohs, P., Hornung, F., Jentzen, A. et al. Space-time error estimates for deep
#' neural network approximations for differential equations. Adv Comput Math 49, 4 (2023).
#' https://doi.org/10.1007/s10444-022-09970-2
#'
#' @param activation_function a continuous function applied to the output of each layer
#' @param x our input to the continuous function formed from activation. Our input will
#' be an element
#'
#' @return a continuous function that is the instantiation of the input neural network
#' with the activation function specified.


inst <- function(neural_network, activation_function, x) {
  if (dep(neural_network) == 1) {
    output <- neural_network[[1]]$W %*% x + neural_network[[1]]$b
    return(output)
  }

  output <- matrix(x, nrow = length(x), ncol = 1)

  for (i in 1:(length(neural_network) - 1)) {
    linear_transform <- neural_network[[i]]$W %*% output + neural_network[[i]]$b
    output <- apply(linear_transform,
      MARGIN = 1,
      FUN = activation_function
    )
  }
  output <- neural_network[[length(neural_network)]]$W %*% output +
    neural_network[[length(neural_network)]]$b
  return(output)
}
