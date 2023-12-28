source("aux_fun.R")
source("Sqr.R")
source("realization.R")
source("activations.R")

diff <- function(q, eps, x) {
  return <- abs(x^2 - rlz(Sqr(q,eps), ReLU, x))
  return(return)
}

Sqr_data <- expand.grid(
  q = seq(2,10,length.out = 50),
  eps = seq(0.1, 2, length.out = 50)
)

for (i in 1:2500) {
  diff(Sqr_data$q[i], Sqr_data$eps[i], 2)
}

Sqr_data$x <- 2

Sqr_data$diff_values <- apply(Sqr_data, 1, diff, 2)


for (i in seq(nrow(Sqr_data))) {
  Sqr_data$y[i] <- diff(Sqr_data$q[i], Sqr_data$eps[i], Sqr_data$x[i])
}
