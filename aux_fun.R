hid <- function(nu) {
  return(length(nu) - 1)
}

hid_v <- Vectorize(hid)

dep <- function(nu) {
  return(length(nu))
}

dep_v <- Vectorize(dep)

inn <- function(nu) {
  return(dim(nu[[1]]$W)[2])
}

inn_v <- Vectorize(inn)

out <- function(nu) {
  return(dim(nu[[length(nu)]]$W)[1])
}

out_v <- Vectorize(out)

lay <- function(nu) {
  layer_architecture <- list()
  for (i in 1:length(nu)) {
    layer_architecture <-
      append(layer_architecture, dim(nu[[i]]$W)[1])
  }
  layer_architecture <- append(inn(nu), layer_architecture)
  return(layer_architecture)
}

lay_v <- Vectorize(lay)

param <- function(nu) {
  param_count <- 0
  for (i in 1:length(nu)) {
    param_count <-
      param_count + length(nu[[i]]$W) + length(nu[[i]]$b)
  }
  return(param_count)
}

param_v <- Vectorize(param)
