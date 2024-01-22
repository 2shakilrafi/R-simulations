source("Aff.R")
source("stacking.R")
source("comp.R")
source("nn_sum.R")
source("Id.R")

#' The Mxm network
#'
#' @param d the dimension of the input
#'
#' @return the neural network that will ouput the maximum when activated
#' with the ReLU function

#' @remark Note that because of certain quirks of R we will have split
#' into five cases. We add an extra case for d == 3. Unlike the paper
#' we will simply reverse engineer the apropriate d.
Mxm <- function(d) {
  if (d == 1) {
    return(Aff(1, 0))
  } else if (d == 2) {
    c(1, 0, 0, -1, 1, -1) |> matrix(3, 2) -> W_1
    c(0, 0, 0) |> matrix() -> b_1
    c(1, 1, -1) |> matrix(1, 3) -> W_2
    0 |> matrix() -> b_2
    list(W = W_1, b = b_1) -> layer_1
    list(W = W_2, b = b_2) -> layer_2
    list(layer_1, layer_2) -> return_network
    return(return_network)
  } else if (d == 3) {
    Mxm(2) |> stk(Id()) -> first_compose
    Mxm(2) |> comp(first_compose) -> return_network
    return(return_network)
  } else if ((d %% 2 == 0) & (d > 3)) {
    d / 2 -> d
    Mxm(2) -> first_compose
    for (i in 1:(d - 1)) {
      first_compose |> stk(Mxm(2)) -> first_compose
    }
    Mxm(d) |> comp(first_compose) -> return_network
    return(return_network)
  } else if ((d %% 2 != 0) & (d > 3)) {
    (d - 1) / 2 -> d

    Mxm(2) -> first_compose
    for (i in 1:(d - 1)) {
      first_compose |> stk(Mxm(2)) -> first_compose
    }
    first_compose |> stk(Id()) -> first_compose
    Mxm(d + 1) |> comp(first_compose) -> return_network
    return(return_network)
  } else {
    return("Error: possily taking max of vector of length 0")
  }
}
