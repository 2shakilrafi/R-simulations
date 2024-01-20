source("affn.R")
source("par.R")
source("comp.R")
source("nn_sum.R")
source("Id.R")

#' The mxm network
#'
#' @param d the dimension of the input
#'
#' @return the neural network that will ouput the maximum when activated
#' with the ReLU function

#' @remark Note that because of certain quirks of R we will have split
#' into five cases. We add an extra case for d == 3. Unlike the paper
#' we will simply reverse engineer the apropriate d.
mxm <- function(d) {
  if (d == 1) {
    return(aff(1, 0))
  }
  if (d == 2) {
    W_1 <- c(1, 0, 0, -1, 1, -1) |> matrix(3, 2)
    b_1 <- c(0, 0, 0) |> matrix()
    W_2 <- c(1, 1, -1) |> matrix(1, 3)
    b_2 <- 0 |> matrix()
    layer_1 <- list(W = W_1, b = b_1)
    layer_2 <- list(W = W_2, b = b_2)
    return_network <- list(layer_1, layer_2)
    return(return_network)
  }

  if (d == 3) {
    mxm(2) |> par(Id()) -> first_compose
    mxm(2) |> comp(first_compose) -> return_network
    return(return_network)
  }

  if ((d %% 2 == 0) & (d > 3)) {
    d <- d / 2
    first_compose <- mxm(2)
    for (i in 1:(d - 1)) {
      first_compose |> par(mxm(2)) -> first_compose
    }
    mxm(d) |> comp(first_compose) -> return_network
    return(return_network)
  }



  if ((d %% 2 != 0) & (d > 3)) {
    d <- (d - 1) / 2

    first_compose <- mxm(2)
    for (i in 1:(d - 1)) {
      first_compose |> par(mxm(2)) -> first_compose
    }
    first_compose |> par(Id()) -> first_compose
    # print(first_compose)
    # print(d_new)
    mxm(d + 1) |> comp(first_compose) -> return_network
    return(return_network)
  }
}

for (i in 1:0) {
  print(i)
}

for (i in 1:100) {
  print(mxm(i))
}
