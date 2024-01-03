source("comp.R")
source("Id.R")

#' The Tunneling Neural Network
#'
#' @param n: The depth of the tunnel network where n >= 1
#'
#' @return a tunnel neural network of depth n.

Tun <- function(n) {
  if (n == 0) {
    return("Error")
  }
  if (n == 1) {
    return(aff(1, 0))
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
