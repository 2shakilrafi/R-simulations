#' Function to generate a random matrix with specified dimensions
#'
#' @param rows
#' @param cols
#'
#' @return
#' @export
#'
#' @examples
generate_random_matrix <- function(rows, cols) {
  return <- (rows * cols) |>
    matrix(rows, cols)
  return(return)
}

# Function to create a list of lists for neural network layers
#' Title
#'
#' @param layer_architecture
#'
#' @return
#' @export
#'
#' @examples
create_neural_network <- function(layer_architecture) {
  L <- layer_architecture |> length()

  # Initialize the list of lists
  neural_network <- list()

  # Generate matrices W and vectors b for each layer
  for (i in 1:(L - 1)) {
    # Set dimensions for W and b
    input_size <- layer_architecture[i]
    output_size <- layer_architecture[i + 1]

    # Create matrix W
    W <- generate_random_matrix(output_size, input_size)

    # Create vector b
    b <- output_size |>
      rnorm() |>
      matrix(output_size, 1)

    # Add W and b to the list
    neural_network[[i]] <- list(W = W, b = b)
  }

  return(neural_network)
}
