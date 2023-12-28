source("aux_fun.R")
source("Sqr.R")
source("realization.R")
source("activations.R")

diff <- function(q, eps, x) {
  return <- abs(x^2 - rlz(Sqr, ReLU, x))
  return(return)
}

Sqr_data <- expand.grid(
  q = 1:9,
  eps = seq(0.1, 1, length.out = 5),
  x = seq(0, 10, length.out = 50)
)

Sqr_data$y <- NA

for (i in seq(nrow(Sqr_data))) {
  Sqr_data$y[i] <- diff(Sqr_data$q[i], Sqr_data$eps[i], Sqr_data$x[i])
}
