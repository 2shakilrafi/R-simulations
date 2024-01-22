source("R/aux_fun.R")
source("R/Tun.R")


#' Function for creating a block diagonal given two matrices.
#'
#' @param matrix1 A matrix.
#' @param matrix2 A matrix
#'
#' @return A block diagonal matrix with matrix1 on top left
#' and matrix2 on bottom right.

create_block_diagonal <- function(matrix1, matrix2) {
  nrow(matrix1) -> m1
  nrow(matrix2) -> m2
  ncol(matrix1) -> n1
  ncol(matrix2) -> n2


  # Create a block diagonal matrix
  0 |> matrix(m1 + m2, n1 + n2) -> block_diagonal_matrix
  block_diagonal_matrix[1:m1, 1:n1] <- matrix1
  block_diagonal_matrix[(m1 + 1):(m1 + m2), (n1 + 1):(n1 + n2)] <-
    matrix2

  return(block_diagonal_matrix)
}

#' The stk function.
#'
#' @param nu neural network.
#' @param mu neural network.
#'
#' @return A stacked neural network of nu and mu.
#'
#' \strong{NOTE} This is different than the one given in Grohs, et. al. 2023.
#' While we use padding to equalize neural networks being parallelized our
#' padding is via the Tun network whereas Grohs et. al. uses repetitive
#' composition of the i network. We use repetitive composition of the \eqn{\mathsf{Id_1}}
#' network. See \code{\link{Id}} \code{\link{comp}}
#'
#' \strong{NOTE} The terminology is also different from Grohs et. al. 2023.
#' We call stacking what they call parallelization. This terminology change was
#' inspired by the fact that parallelization implies commutativity but this
#' operation is not quite commutative. It is commutative up to transposition
#' of our input x under instantiation with a continuous activation function.
#'
#' Also the work parallelization has a lot of baggage when it comes to
#' artificial neural networks in that it often means many different CPUs working
#' together.
#'
#' \emph{Remark:} We will use only one symbol for stacking equal and unequal depth
#' neural networks, namely "stk". This is for usability but also that
#' for all practical purposes only the general stacking of neural networks
#' of different sizes is what is needed.

stk <- function(nu, mu) {
  if (dep(nu) == dep(mu)) {
    list() -> parallelized_network
    for (i in 1:length(nu)) {
      create_block_diagonal(nu[[i]]$W, mu[[i]]$W) -> parallelized_W
      rbind(nu[[i]]$b, mu[[i]]$b) -> parallelized_b
      list(W = parallelized_W, b = parallelized_b) -> parallelized_network[[i]]
    }
    return(parallelized_network)
  }

  if (dep(nu) > dep(mu)) {
    (dep(nu) - dep(mu) + 1) |> Tun() -> padding
    padding |> comp(mu) -> padded_network
    nu |> stk(padded_network) -> parallelized_network
    return(parallelized_network)
  }

  if (dep(nu) < dep(mu)) {
    (dep(mu) - dep(nu) + 1) |> Tun() -> padding
    padding |> comp(nu) -> padded_network
    padded_network |> stk(mu) -> parallelized_network
    return(parallelized_network)
  }
}

#' The stk function.
#'
#' @param nu neural network.
#' @param mu neural network.
#'
#' @return A stacked neural network of nu and mu.
#'
#' \strong{NOTE} This is different than the one given in Grohs, et. al. 2023.
#' While we use padding to equalize neural networks being parallelized our
#' padding is via the Tun network whereas Grohs et. al. uses repetitive
#' composition of the i network. We use repetitive composition of the \eqn{\mathsf{Id_1}}
#' network. See \code{\link{Id}} \code{\link{comp}}
#'
#' \strong{NOTE} The terminology is also different from Grohs et. al. 2023.
#' We call stacking what they call parallelization. This terminology change was
#' inspired by the fact that parallelization implies commutativity but this
#' operation is not quite commutative. It is commutative up to transposition
#' of our input x under instantiation with a continuous activation function.
#'
#' Also the work parallelization has a lot of baggage when it comes to
#' artificial neural networks in that it often means many different CPUs working
#' together.
#'
#' \emph{Remark:} We will use only one symbol for stacking equal and unequal depth
#' neural networks, namely "stk". This is for usability but also that
#' for all practical purposes only the general stacking of neural networks
#' of different sizes is what is needed.

`%stk%` <- function(nu, mu) {
  if (dep(nu) == dep(mu)) {
    list() -> parallelized_network
    for (i in 1:length(nu)) {
      create_block_diagonal(nu[[i]]$W, mu[[i]]$W) -> parallelized_W
      rbind(nu[[i]]$b, mu[[i]]$b) -> parallelized_b
      list(W = parallelized_W, b = parallelized_b) -> parallelized_network[[i]]
    }
    return(parallelized_network)
  }

  if (dep(nu) > dep(mu)) {
    (dep(nu) - dep(mu) + 1) |> Tun() -> padding
    padding |> comp(mu) -> padded_network
    nu |> stk(padded_network) -> parallelized_network
    return(parallelized_network)
  }

  if (dep(nu) < dep(mu)) {
    (dep(mu) - dep(nu) + 1) |> Tun() -> padding
    padding |> comp(nu) -> padded_network
    padded_network |> stk(mu) -> parallelized_network
    return(parallelized_network)
  }
}
