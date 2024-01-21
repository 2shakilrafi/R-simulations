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
  dep(phi_1) -> L
  dep(phi_2) -> L_

  if (L > 1 & L_ > 1) {
    phi_2[-L_] -> beginning
    phi_1[-1] -> end
    phi_1[[1]]$W %*% phi_2[[L_]]$W -> mid_W
    phi_1[[1]]$W %*% phi_2[[L_]]$b + phi_1[[1]]$b -> mid_b
    list(W = mid_W, b = mid_b) -> mid
    composed_network <- c(
      beginning,
      list(mid),
      end
    ) -> mid
    return(composed_network)
  }

  if (L > 1 & L_ == 1) {
    phi_1[[1]]$W %*% phi_2[[1]]$W -> beginning_W
    phi_1[[1]]$W %*% phi_2[[1]]$b + phi_1[[1]]$b -> beginning_b
    list(
      W = beginning_W,
      b = beginning_b
    ) -> beginning
    phi_1[-1] -> end
    c(
      list(beginning),
      end
    ) -> composed_network
    return(composed_network)
  }

  if (L == 1 & L_ > 1) {
    phi_2[-L_] -> beginning
    phi_1[[1]]$W %*% phi_2[[L_]]$W -> end_W
    phi_1[[1]]$W %*% phi_2[[L_]]$b + phi_1[[1]]$b -> end_b
    list(
      W = end_W,
      b = end_b
    ) -> end
    c(
      beginning,
      list(end)
    ) -> composed_network
    return(composed_network)
  }


  if (L == 1 & L_ == 1) {
    list() -> composed_network
    phi_1[[1]]$W %*% phi_2[[1]]$W -> W
    phi_1[[1]]$W %*% phi_2[[1]]$b + phi_1[[1]]$b -> b
    list(
      W = W,
      b = b
    ) -> composed_network[[1]]
    return(composed_network)
  }
}

comp <- function(phi_1, phi_2) {
  dep(phi_1) -> L
  dep(phi_2) -> L_

  if (L > 1 & L_ > 1) {
    phi_2[-L_] -> beginning
    phi_1[-1] -> end
    phi_1[[1]]$W %*% phi_2[[L_]]$W -> mid_W
    phi_1[[1]]$W %*% phi_2[[L_]]$b + phi_1[[1]]$b -> mid_b
    list(W = mid_W, b = mid_b) -> mid
    composed_network <- c(
      beginning,
      list(mid),
      end
    ) -> mid
    return(composed_network)
  }

  if (L > 1 & L_ == 1) {
    phi_1[[1]]$W %*% phi_2[[1]]$W -> beginning_W
    phi_1[[1]]$W %*% phi_2[[1]]$b + phi_1[[1]]$b -> beginning_b
    list(
      W = beginning_W,
      b = beginning_b
    ) -> beginning
    phi_1[-1] -> end
    c(
      list(beginning),
      end
    ) -> composed_network
    return(composed_network)
  }

  if (L == 1 & L_ > 1) {
    phi_2[-L_] -> beginning
    phi_1[[1]]$W %*% phi_2[[L_]]$W -> end_W
    phi_1[[1]]$W %*% phi_2[[L_]]$b + phi_1[[1]]$b -> end_b
    list(
      W = end_W,
      b = end_b
    ) -> end
    c(
      beginning,
      list(end)
    ) -> composed_network
    return(composed_network)
  }


  if (L == 1 & L_ == 1) {
    list() -> composed_network
    phi_1[[1]]$W %*% phi_2[[1]]$W -> W
    phi_1[[1]]$W %*% phi_2[[1]]$b + phi_1[[1]]$b -> b
    list(
      W = W,
      b = b
    ) -> composed_network[[1]]
    return(composed_network)
  }
}
