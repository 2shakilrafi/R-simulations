#' The Trp network that calculates the trapezoidal area 
#' between two mesh points
#'
#' @param h the horizontal distance between two mesh points
#'
#' @return the area when activated with ReLU and two meshpoints
#' x_1 and x_2
#' 
#' @export
#'
#' @examples
Trp <- function(h) {
  c(h / 2, h / 2) |> matrix(1, 2) -> W
  0 |> matrix() -> b
  list(list(W = W, b = b)) -> return_network
  return(return_network)
}

#' The Etr network
#'
#' @param n number of trapezoids to make. Note this will result in a set of trapezoids
#' Note that this will result in n+1 meshpoints including the starting a and
#' ending b
#' 
#' @param h width of trapezoids 
#'
#' @return an approximation for area of the integral 
#' @export
#'
#' @examples
Etr <- function(n, h) {
  c(h / 2, rep(h, n - 1), h / 2) |>
    matrix() |>
    t() -> W
  0 |> matrix() -> b
  list(list(W = W, b = b)) -> return_network
  return(return_network)
}
