source("phi.R")
source("aux_fun")
source("realization.R")
source("activations.R")

library(ggplot2)

diff <- function(x,k) {
  abs(x^2 - rlz(phi(k), ReLU, x))
}

k_values <- c(2, 5, 10, 15, 20, 25)
x_values <- seq(-2, 2, length.out = 200)
vectorized_diff <- Vectorize(diff)

diff_data <- expand.grid(k = k_values, x = x_values)
diff_data$y <- vectorized_diff(diff_data$x, diff_data$k)

ggplot(diff_data, aes(x = x, y = y, color = factor(k))) +
  scale_y_log10() +
  geom_line() +
  geom_point(aes(y = 2^(-2*k - 2)), color = "black") +
  labs(
    x = "x",
    y = "log10 of the 1-norm distance over entire domain"
  )

param_data <- data.frame(x = 1:100, y = param(phi(1:100)))















