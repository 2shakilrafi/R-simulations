source("aux_fun.R")

#' The %•% function
#'
#' @param phi_1 
#' @param phi_2 
#'
#' @return the composed neural network
#' 
#' @references Grohs, P., Hornung, F., Jentzen, A. et al. 
#' Space-time error estimates for deep neural network approximations 
#' for differential equations. Adv Comput Math 49, 4 (2023). 
#' https://doi.org/10.1007/s10444-022-09970-2
#' 
#' @remark Note that we have two versions of this function, a unicode
#' infix version for close resemblance to mathematical notation and 
#' prefix version comp()
#' 
#' @encoding utf8

`%•%` <- function(phi_1, phi_2) {
  L <- dep(phi_1)
  L_ <- dep(phi_2)

  if (L > 1 & L_ > 1) {
    beginning <- phi_2[-L_]
    end <- phi_1[-1]
    mid_W <- phi_1[[1]]$W %*% phi_2[[L_]]$W
    mid_b <- phi_1[[1]]$W %*% phi_2[[L_]]$b + phi_1[[1]]$b
    mid <- list(W = mid_W, b = mid_b)
    composed_network <- c(
      beginning,
      list(mid),
      end
    )
    return(composed_network)
  }

  if (L > 1 & L_ == 1) {
    beginning_W <- phi_1[[1]]$W %*% phi_2[[1]]$W
    beginning_b <- phi_1[[1]]$W %*% phi_2[[1]]$b + phi_1[[1]]$b
    beginning <- list(
      W = beginning_W,
      b = beginning_b
    )
    end <- phi_1[-1]
    composed_network <- c(
      list(beginning),
      end
    )
    return(composed_network)
  }

  if (L == 1 & L_ > 1) {
    beginning <- phi_2[-L_]
    end_W <- phi_1[[1]]$W %*% phi_2[[L_]]$W
    end_b <- phi_1[[1]]$W %*% phi_2[[L_]]$b + phi_1[[1]]$b
    end <- list(
      W = end_W,
      b = end_b
    )
    composed_network <- c(
      beginning,
      list(end)
    )
    return(composed_network)
  }


  if (L == 1 & L_ == 1) {
    composed_network <- list()
    W <- phi_1[[1]]$W %*% phi_2[[1]]$W
    b <- phi_1[[1]]$W %*% phi_2[[1]]$b + phi_1[[1]]$b
    composed_network[[1]] <- list(
      W = W,
      b = b
    )
    return(composed_network)
  }
}

comp <- function(phi_1, phi_2) {
  L <- dep(phi_1)
  L_ <- dep(phi_2)

  if (L > 1 & L_ > 1) {
    beginning <- phi_2[-L_]
    end <- phi_1[-1]
    mid_W <- phi_1[[1]]$W %*% phi_2[[L_]]$W
    mid_b <- phi_1[[1]]$W %*% phi_2[[L_]]$b + phi_1[[1]]$b
    mid <- list(W = mid_W, b = mid_b)
    composed_network <- c(
      beginning,
      list(mid),
      end
    )
    return(composed_network)
  }

  if (L > 1 & L_ == 1) {
    beginning_W <- phi_1[[1]]$W %*% phi_2[[1]]$W
    beginning_b <- phi_1[[1]]$W %*% phi_2[[1]]$b + phi_1[[1]]$b
    beginning <- list(
      W = beginning_W,
      b = beginning_b
    )
    end <- phi_1[-1]
    composed_network <- c(
      list(beginning),
      end
    )
    return(composed_network)
  }

  if (L == 1 & L_ > 1) {
    beginning <- phi_2[-L_]
    end_W <- phi_1[[1]]$W %*% phi_2[[L_]]$W
    end_b <- phi_1[[1]]$W %*% phi_2[[L_]]$b + phi_1[[1]]$b
    end <- list(
      W = end_W,
      b = end_b
    )
    composed_network <- c(
      beginning,
      list(end)
    )
    return(composed_network)
  }


  if (L == 1 & L_ == 1) {
    # first
    composed_network <- list()
    W <- phi_1[[1]]$W %*% phi_2[[1]]$W
    b <- phi_1[[1]]$W %*% phi_2[[1]]$b + phi_1[[1]]$b
    composed_network[[1]] <- list(
      W = W,
      b = b
    )
    return(composed_network)
  }
}
