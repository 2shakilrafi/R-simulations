trp <- function(h) {
  c(h / 2, h / 2) |> matrix(1, 2) -> W
  0 |> matrix() -> b
  list(list(W = W, b = b)) -> return_network
  return(return_network)
}

etr <- function(n, h) {
  c(h / 2, rep(h, n - 1), h / 2) |>
    matrix() |>
    t() -> W
  0 |> matrix() -> b
  list(list(W = W, b = b)) -> return_network
  return(return_network)
}
