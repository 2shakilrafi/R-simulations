#' Title
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
i <- function(d) {
  return_network <- list()
  W <- d |> diag()
  b <- 0 |> matrix(d, 1)
  return_network[[1]] <- list(W = W, b = b)
  return_network[[2]] <- list(W = W, b = b)
  return(return_network)
}
