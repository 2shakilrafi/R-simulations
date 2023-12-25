

i_network <- function(d) {
  return_network <- list()
  W <- diag(d)
  b <- matrix(0, 
              nrow = d,
              ncol = 1)
  return_network[[1]] <- list(W = W, b = b)
  return_network[[2]] <- list(W = W, b = b)
}