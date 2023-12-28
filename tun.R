source("comp.R")
source("Id.R")

#' The Tunneling Neural Network
#'
#' @param n: The depth of the tunnel network
#'
#' @return a tunnel neural network of depth n.

Tun <- function(n) {
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
