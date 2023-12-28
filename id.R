Id <- function() {
  W_1 <- c(1, -1) |> matrix()

  b_1 <- c(0, 0) |> matrix()

  layer_1 <- list(W = W_1, b = b_1)

  W_2 <- c(1, -1) |> matrix(1, 2)

  b_2 <- 0 |> matrix()

  layer_2 <- list(W = W_2, b = b_2)

  return <- list(layer_1, layer_2)

  return(return)
}
