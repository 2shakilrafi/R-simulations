#' The i function
#'
#' @param d the size of the i network
#'
#' @return returns the i_d network

i <- function(d) {
  return_network <- list()
  W <- d |> diag()
  b <- 0 |> matrix(d, 1)
  return_network[[1]] <- list(W = W, b = b)
  return_network[[2]] <- list(W = W, b = b)
  return(return_network)
}
