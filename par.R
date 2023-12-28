create_block_diagonal <- function(matrix1, matrix2) {
  n1 <- nrow(matrix1)
  n2 <- nrow(matrix2)
  m1 <- ncol(matrix1)
  m2 <- ncol(matrix2)

  # Create a block diagonal matrix
  block_diagonal_matrix <- 0 |> matrix(0, n1 + n2, m1 + m2)
  block_diagonal_matrix[1:n1, 1:m1] <- matrix1
  block_diagonal_matrix[(n1 + 1):(n1 + n2), (m1 + 1):(m1 + m2)] <-
    matrix2

  return(block_diagonal_matrix)
}

par <- function(nu, mu) {
  parallelized_network <- list()
  for (i in 1:length(nu)) {
    parallelized_W <- create_block_diagonal(nu[[i]]$W, mu[[i]]$W)
    parallelized_b <- rbind(nu[[i]]$b, mu[[i]]$b)
    parallelized_network[[i]] <-
      list(W = parallelized_W, b = parallelized_b)
  }
  return(parallelized_network)
}
