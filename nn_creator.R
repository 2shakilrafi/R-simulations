



nn_creator   <- function(neurons_per_layer) {
  num_layers <- length(neurons_per_layer)
  network    <- list()
  
  
  for (i in 1:(num_layers - 1)) {
    weight       <-
      matrix(
        runif(
          neurons_per_layer[i] * neurons_per_layer[i + 1],
          min = -1,
          max = 1
        ),
        nrow = neurons_per_layer[i + 1],
        ncol = neurons_per_layer[i]
      )
    bias         <-
      runif(neurons_per_layer[i + 1], min = -1, max = 1)
    layer_data   <- list(weights = weights, bias = bias)
    network[[i]] <- layer_data
  }
  return(network)
}

layer_config = c(3, 4, 5, 6)
test_2_nn <- nn_creator(layer_config)
