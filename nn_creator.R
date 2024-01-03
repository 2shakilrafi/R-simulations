#' Function to generate a random matrix with specified dimensions
#'
#' @param rows number of rows
#' @param cols number of columns
#'
#' @return a random matrix of dimension rows times columns with elements from
#' a standdard normal distribution
#' @references

generate_random_matrix <- function(rows, cols) {
  (rows * cols) |>
    rnorm() |>
    matrix(rows, cols) -> result
  return(result)
}

# Function to create a list of lists for neural network layers
#'
#' @param layer_architecture a tuple specifying the width of each layer
#'
#' @return an ordered list of ordered pairs of (W,d)

create_neural_network <- function(layer_architecture) {
  layer_architecture |> length() -> L

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
