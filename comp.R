source("aux_fun.R")

# The composition script that takes two nn and reutrns their composition

`%•%` <- function (nu, mu) {
  if (dep(nu) == 1 & dep(mu) == 1) {
    W <- nu[[1]]$W %*% mu[[1]]$W
    b <- nu[[1]]$W %*% mu[[1]]$b + nu[[1]]$b
    composed_network <- (list(W = W,
                              b = b))
    return(composed_network)
  }
  
  if (dep(nu) == 1 & dep(mu) > 1) {
    beginning <- mu[-length(mu)]
    end_W <- nu[[1]]$W %*% mu[[length(mu)]]$W
    end_b <- nu[[1]]$W %*% mu[[length(mu)]]$b + nu[[1]]$b
    end <- list(W = end_W,
                b = end_b)
    composed_network <- c(beginning,
                          list(end))
    return(composed_network)
  }
  
  if (dep(nu) > 1 & dep(mu) == 1) {
    beginning_W <- nu[[1]]$W %*% mu[[1]]$W
    beginning_b <- nu[[1]]$W %*% mu[[1]]$b + nu[[1]]$b
    beginning <- list(W = beginning_W,
                      b = beginning_b)
    end <- nu[-1]
    composed_network <- c(list(beginning),
                          end)
    return (composed_network)
  }
  
  if (dep(mu) > 1 & dep(nu) > 1) {
    beginning <- mu[-length(mu)]
    end <- nu[-1]
    mid_W <- mu[[1]]$W %*% mu[[length(mu)]]$W
    mid_b <- nu[[1]]$W %*% mu[[length(mu)]]$b + nu[[1]]$b
    mid <- list(W = mid_W, b = mid_b)
    composed_network <- c(beginning,
                          list(mid),
                          end)
    return(composed_network)
  }
}

`%comp%` <- function (nu, mu) {
  if (dep(nu) == 1 & dep(mu) == 1) {
    W <- nu[[1]]$W %*% mu[[1]]$W
    b <- nu[[1]]$W %*% mu[[1]]$b + nu[[1]]$b
    composed_network <- (list(W = W,
                              b = b))
    return(composed_network)
  }
  
  if (dep(nu) == 1 & dep(mu) > 1) {
    beginning <- mu[-length(mu)]
    end_W <- nu[[1]]$W %*% mu[[length(mu)]]$W
    end_b <- nu[[1]]$W %*% mu[[length(mu)]]$b + nu[[1]]$b
    end <- list(W = end_W,
                b = end_b)
    composed_network <- c(beginning,
                          list(end))
    return(composed_network)
  }
  
  if (dep(nu) > 1 & dep(mu) == 1) {
    beginning_W <- nu[[1]]$W %*% mu[[1]]$W
    beginning_b <- nu[[1]]$W %*% mu[[1]]$b + nu[[1]]$b
    beginning <- list(W = beginning_W,
                      b = beginning_b)
    end <- nu[-1]
    composed_network <- c(list(beginning),
                          end)
    return (composed_network)
  }
  
  if (dep(mu) > 1 & dep(nu) > 1) {
    beginning <- mu[-length(mu)]
    end <- nu[-1]
    mid_W <- mu[[1]]$W %*% mu[[length(mu)]]$W
    mid_b <- nu[[1]]$W %*% mu[[length(mu)]]$b + nu[[1]]$b
    mid <- list(W = mid_W,
                b = mid_b)
    composed_network <- c(beginning,
                          list(mid),
                          end)
    return(composed_network)
  }
}