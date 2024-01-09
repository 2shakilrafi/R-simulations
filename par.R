#' Function for creating a block diagonal
#'
#' @param matrix1 
#' @param matrix2 
#'
#' @return a block diagonal matrix with matrix1 on top left
#' and matrix2 on bottom right
#'
#'@remark: NOTE!! This is different than the one given in Grohs, et. al. 2023.
#'While we use padding to equalize neural networks being parallelized our
#'padding is via the Tun network whereas Grohs et. al. uses repetitive 
#'composition of the i network.
#'
source("aux_fun.R")
source("Tun.R")





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

#' The parallelization function
#'
#' @param nu neural network
#' @param mu neural network
#'
#' @return parallelized neural network

par <- function(nu, mu) {
 
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

par <- function(nu, mu) {
  
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
