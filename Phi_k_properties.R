source("Phi_k.R")
source("aux_fun")
source("realization.R")
source("activations.R")

library(ggplot2)

diff <- function(x, k) {
  return <- (k |> Phi_k() |> rlz(ReLU, x) - x^2) |>
    abs()
  return(return)
}

k_values <- c(1, 2, 5, 10, 15, 20, 25)
x_values <- seq(-2, 2, length.out = 200)
vectorized_diff <- Vectorize(diff)

diff_data <- expand.grid(k = k_values, x = x_values)
diff_data$y <- vectorized_diff(diff_data$x, diff_data$k)

ggplot(diff_data, aes(x = x, y = y, color = factor(k))) +
  scale_y_log10() +
  geom_line() +
  geom_point(aes(y = 2^(-2 * k - 2)), color = "") +
  labs(
    x = "x",
    y = "log10 of the 1-norm distance over entire domain"
  )

vectorized_Phi_k <- Vectorize(Phi_k)
vectorized_param <- Vectorize(param)

param_data <- data.frame(x = 1:100, y = vectorized_param(vectorized_Phi_k(1:100)))

ggplot(param_data, aes(x = x, y = y)) +
  geom_line() +
  theme_minimal() +
  xlab("Size of k") +
  ylab("Number of parameters") +
  ggtitle("Plot of the number of parameters of ϕ(k) against k") +
  geom_smooth(method = "lm", se = FALSE, color = "blue")

vectorized_dep <- Vectorize(dep)

dep_data <- data.frame(x = 1:100, y = vectorized_dep(vectorized_Phi_k(1:100)))

ggplot(dep_data, aes(x = x, y = y)) +
  geom_line() +
  theme_minimal() +
  xlab("Size of k") +
  ylab("Depth of network") +
  ggtitle("Plot of the depth of ϕ(k) against k") +
  geom_smooth(method = "lm", se = FALSE, color = "blue")
