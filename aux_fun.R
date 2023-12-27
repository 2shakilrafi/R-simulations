hid <- function(nu) {
  return(length(nu) - 1)
}

dep <- function(nu) {
  return(length(nu))
}

inn <- function(nu) {
  return(dim(nu[[1]][[1]])[2])
}

out <- function(nu) {
  return(dim(nu[[length(nu)]][[1]])[1])
}

lay <- function(nu) {
  layer_architecture <- list()
  for (i in 1:length(nu)) {
    layer_architecture <-
      append(layer_architecture, dim(nu[[i]][[1]])[1])
  }
  layer_architecture <- append(inn(nu), layer_architecture)
  return(layer_architecture)
}

param <- function(nu) {
  param_count <- 0
  for (i in 1:length(nu)) {
    param_count <-
      param_count + length(nu[[i]][[1]]) + length(nu[[i]][[2]])
  }
  return(param_count)
}
