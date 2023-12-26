

id <- function() {
  W_1 <- matrix(c(1, -1),
                nrow = 2,
                ncol = 1)
  b_1 <- matrix(c(0, 0),
                nrow = 2,
                ncol = 1)
  layer_1 <- list(W_1,
                  b_1)
  
  W_2 <- matrix(c(1, -1),
                nrow = 1,
                ncol = 2)
  b_2 <- matrix(c(0),
                nrow = 1,
                ncol = 1)
  layer_2 <- list(W_2,
                  b_2)
  
  id_network <- list(list(layer_1,
                          layer_2))
  return(id_network)
}