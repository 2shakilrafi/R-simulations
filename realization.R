source("aux_fun.R")

rlz <- function(neural_network, activation_function, x) {
  if(dep(neural_network) == 1) {
    output <- neural_network[[1]]$W %*% x + neural_network[[1]]$b
    return(output)
  }

  output <- matrix(x, nrow = length(x), ncol = 1)
  
  for (i in 1:(length(neural_network)-1)) {
    linear_transform <- neural_network[[i]]$W %*% output + neural_network[[i]]$b
    output <- apply(linear_transform, 
                    MARGIN = 1, 
                    FUN = activation_function)
  }
  output <- neural_network[[length(neural_network)]]$W %*% output + neural_network[[length(neural_network)]]$b
  return(output)
}