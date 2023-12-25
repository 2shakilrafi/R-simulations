

ReLU <- function(x) {
  return (max(x, 0))
}

Sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}

Cont <- function(x) {
  return(x)
}