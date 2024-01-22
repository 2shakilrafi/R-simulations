source("comp.R")
source("Id.R")

#' The Tunneling Neural Network
#'
#' @param n: The depth of the tunnel network where n >= 1
#'
#' @return a tunnel neural network of depth n.

Tun <- function(n) {
  if (n == 1) {
    return(aff(1, 0))
  } else if (n == 2) {
    return(Id())
  } else if (n > 2) {
    Id() -> return_network
    for (i in 3:n) {
      return_network |> comp(Id()) -> return_network
    }
    return(return_network)
  } else {
    return("Error, n not in range")
  }
}
