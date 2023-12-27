source("comp.R")

#' The Tunneling Neural Network
#'
#' @param n: The depth of the tunnel network
#'
#' @return a tunnel neural network of depth n.

tun <- function(n) {
  
  W_1 <- matrix(c(1, -1), nrow = 2, ncol = 1)
  b_1 <- matrix(c(0, 0), nrow = 2, ncol = 1)
  layer_1 <- list(W_1, b_1)

  W_2 <- matrix(c(1, -1), nrow = 1, ncol = 2)
  b_2 <- matrix(c(0), nrow = 1, ncol = 1)
  layer_2 <- list(W_2, b_2)

  id <- list(layer_1, layer_2)
  if (n == 1) {
    W <- matrix(1, nrow = 1, ncol = 1)
    b <- matrix(0, nrow = 1, ncol = 1)
    return_network <- list(W, b)
    return(return_network)
  }

  if (n == 2) {
    return(id)
  }

  if (n > 2) {
    return_network <- id
    for (i in 3:n) {
      return_network <- return_network %•% id
    }
    return(return_network)
  }
}
