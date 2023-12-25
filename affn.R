

aff <- function (W, b) {
  return(list(list(W = W, b = b)))
}

cpy <- function (n, k) {
  W <- diag(k)
  for (i in 2:n) {
    W <- rbind(W, diag(k))
  }
  b <- matrix(0, n * k, 1)
  return(list(list(W = W, b = b)))
}

sm <- function (n, k) {
  W <- diag(k)
  for (i in 2:n) {
    W <- cbind(W, diag(k))
  }
  b <- matrix(0, k)
  return(list(list(W = W, b = b)))
}