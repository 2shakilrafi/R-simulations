source("R/comp.R")
source("R/Id.R")

#' The Tunneling Neural Network
#'
#' @param n: The depth of the tunnel network where \eqn{n \in \mathbb{N} \cap [1,\infty)}.
#'
#' @return A tunnel neural network of depth n. A tunneling neural
#' network is defined as the neural network \eqn{\mathsf{Aff}_{1,0}} for \eqn{n=1},
#' the neural network \eqn{\mathsf{Id}_1} for \eqn{n=1} and the neural network
#' \eqn{\bullet^{n-2}\mathsf{Id}_1} for \eqn{n >2}
#'
#'

Tun <- function(n) {
  if (n == 1) {
    return(Aff(1, 0))
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
