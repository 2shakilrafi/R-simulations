source("aux_fun.R")
source("Tun.R")

#' Function for creating a block diagonal given two matrices
#'
#' @param matrix1
#' @param matrix2
#'
#' @return a block diagonal matrix with matrix1 on top left
#' and matrix2 on bottom right

create_block_diagonal <- function(matrix1, matrix2) {
  n1 <- nrow(matrix1)
  n2 <- nrow(matrix2)
  m1 <- ncol(matrix1)
  m2 <- ncol(matrix2)


  # Create a block diagonal matrix
  block_diagonal_matrix <- 0 |> matrix(n1 + n2, m1 + m2)
  block_diagonal_matrix[1:n1, 1:m1] <- matrix1
  block_diagonal_matrix[(n1 + 1):(n1 + n2), (m1 + 1):(m1 + m2)] <-
    matrix2

  return(block_diagonal_matrix)
}

#' The stacking function
#'
#' @param nu neural network
#' @param mu neural network
#'
#' @return stacked neural network of nu and mu
#' @remark: NOTE!! This is different than the one given in Grohs, et. al. 2023.
#' While we use padding to equalize neural networks being parallelized our
#' padding is via the Tun network whereas Grohs et. al. uses repetitive
#' composition of the i network.
#'
#' @remark NOTE!! The terminology is also different from Grohs et. al. 2023.
#' We call stacking what they call parallelization. This terminology change was
#' inspired by the fact that parallelization implies commutativity but this
#' operation is not quite commutative. It is commutative up to transposition
#' of our input x under instantiation with a continuous activation function.
#'
#' Also the work parallelization has a lot of baggage when it comes to
#' artificial neural networks in that it often means many different CPUs working
#' together.
#'
#' @remark We will use only one symbol for stacking equal and unequal depth
#' neural networks, namely \eqn{\boxminus}. This is for usability but also that
#' for all practical purposes only the general stacking of neural networks
#' of different sizes is what is needed.

stk <- function(nu, mu) {
  if (dep(nu) == dep(mu)) {
    parallelized_network <- list()
    for (i in 1:length(nu)) {
      parallelized_W <- create_block_diagonal(nu[[i]]$W, mu[[i]]$W)
      parallelized_b <- rbind(nu[[i]]$b, mu[[i]]$b)
      parallelized_network[[i]] <-
        list(W = parallelized_W, b = parallelized_b)
    }
    return(parallelized_network)
  }

  if (dep(nu) > dep(mu)) {
    (dep(nu) - dep(mu) + 1) |> Tun() -> padding
    padding |> comp(mu) -> padded_network
    nu |> par(padded_network) -> parallelized_network
    return(parallelized_network)
  }

  if (dep(nu) < dep(mu)) {
    (dep(mu) - dep(nu) + 1) |> Tun() -> padding
    padding |> comp(nu) -> padded_network
    padded_network |> par(mu) -> parallelized_network
    return(parallelized_network)
  }
}

`%⊟%` <- function(nu, mu) {
  if (dep(nu) == dep(mu)) {
    parallelized_network <- list()
    for (i in 1:length(nu)) {
      parallelized_W <- create_block_diagonal(nu[[i]]$W, mu[[i]]$W)
      parallelized_b <- rbind(nu[[i]]$b, mu[[i]]$b)
      parallelized_network[[i]] <-
        list(W = parallelized_W, b = parallelized_b)
    }
    return(parallelized_network)
  }

  if (dep(nu) > dep(mu)) {
    (dep(nu) - dep(mu) + 1) |> Tun() -> padding
    padding |> comp(mu) -> padded_network
    nu |> par(padded_network) -> parallelized_network
    return(parallelized_network)
  }

  if (dep(nu) < dep(mu)) {
    (dep(mu) - dep(nu) + 1) |> Tun() -> padding
    padding |> comp(nu) -> padded_network
    padded_network |> par(mu) -> parallelized_network
    return(parallelized_network)
  }
}
