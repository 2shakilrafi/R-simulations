# Function to generate a random matrix with specified dimensions
generate_random_matrix <- function(rows, cols) {
  matrix(rnorm(rows * cols), nrow = rows, ncol = cols)
}

# Function to create a list of lists for neural network layers
create_neural_network <- function(neuron_counts) {
  num_layers <- length(neuron_counts)
  
  # Initialize the list of lists
  list_of_lists <- list()
  
  # Generate matrices W and vectors b for each layer
  for (i in 1:(num_layers - 1)) {
    # Set dimensions for W and b
    input_size <- neuron_counts[i]
    output_size <- neuron_counts[i + 1]
    
    # Create matrix W
    W <- generate_random_matrix(output_size, input_size)
    
    # Create vector b
    b <- matrix(rnorm(output_size),
                nrow = output_size,
                ncol = 1)
    
    # Add W and b to the list
    list_of_lists[[i]] <- list(W = W, b = b)
  }
  
  return(list_of_lists)
}
