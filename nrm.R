source("affn.R")
source("par.R")
source("comp.R")
source("nn_sum.R")

#' The Nrm network
#'
#' @param d the dimensions of the vector being normed
#'
#' @return a neural network that takes the 1-norm of a vector of
#' size d.under ReLU activation
#' @remark note that this function is split into two cases
#' much like the definition itself.
#'
nrm_1 <- function(d) {
  if (d == 1) {
    W_1 <- c(1, -1) |> matrix()
    b_1 <- c(0, 0) |> matrix()
    W_2 <- c(1, 1) |> matrix(1, 2)
    b_2 <- 0 |> matrix()

    layer_1 <- list(W = W_1, b = b_1)
    layer_2 <- list(W = W_2, b = b_2)
    return_network <- list(layer_1, layer_2)
    return(return_network)
  }
  if (d > 1) {
    first_compose <- nrm_1(1)
    for (i in 1:(d - 1)) {
      first_compose |> par(nrm_1(1)) -> first_compose
    }
    sm(d, 1) |> comp(first_compose) -> return_network
    return(return_network)
  }
}
