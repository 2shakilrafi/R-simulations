source("comp.R")
source("Id.R")

#' The Tunneling Neural Network
#'
#' @param n: The depth of the tunnel network
#'
#' @return a tunnel neural network of depth n.

Tun <- function(n) {
  if (n == 1) {
    W <- 1 |> matrix()
    b <- 0 |> matrix()
    return_network <- list(W, b)
    return(return_network)
  }

  if (n == 2) {
    return(Id())
  }

  if (n > 2) {
    return_network <- Id()
    for (i in 3:n) {
      return_network <- return_network |> comp(Id())
    }
    return(return_network)
  }
}
