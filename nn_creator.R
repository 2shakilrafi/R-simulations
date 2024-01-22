#' Function to generate a random matrix with specified dimensions.
#'
#' @param rows number of rows.
#' @param cols number of columns.
#'
#' @return a random matrix of dimension rows times columns with elements from
#' a standard normal distribution

generate_random_matrix <- function(rows, cols) {
  (rows * cols) |>
    rnorm() |>
    matrix(rows, cols) -> result
  return(result)
}

#' Function to create a list of lists for neural network layers
#'
#' @param layer_architecture a tuple specifying the width of each layer
#'
#' @return an ordered list of ordered pairs of \eqn{(W,d)}. We will use the
#' definition of neural networks as found in:
#'
#' @references Grohs, P., Hornung, F., Jentzen, A. et al.
#' Space-time error estimates for deep neural network approximations
#' for differential equations. Adv Comput Math 49, 4 (2023).
#' \url{https://doi.org/10.1007/s10444-022-09970-2}.

create_neural_network <- function(layer_architecture) {
  layer_architecture |> length() -> L

  # Initialize the list of lists
  neural_network <- list()

  # Generate matrices W and vectors b for each layer
  for (i in 1:(L - 1)) {
    # Set dimensions for W and b
    layer_architecture[i] -> input_size
    layer_architecture[i + 1] -> output_size

    # Create matrix W
    generate_random_matrix(output_size, input_size) -> W

    # Create vector b
    output_size |>
      rnorm() |>
      matrix(output_size, 1) -> b

    # Add W and b to the list
    list(W = W, b = b) -> neural_network[[i]]
  }

  return(neural_network)
}
