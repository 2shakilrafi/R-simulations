source("Phi_k.R")
source("aux_fun")
source("realization.R")
source("activations.R")

library(ggplot2)

#' The Phi_k_diff function
#'
#' @param x the number to be squared in [0,1]
#' @param k a parameter for Phi_k in [0, \infty)]
#'
#' @return the 1-norm error between x^2 and Phi_k approximation

Phi_k_diff <- function(x, k) {
  return <- (k |> Phi_k() |> rlz(ReLU, x) - x^2) |>
    abs() -> result
  return(result)
}

k_values <- c(1, 2, 5, 10, 15, 20)
x_values <- seq(-2, 2, length.out = 200)
Phi_k_diff_v <- Vectorize(Phi_k_diff)

Phi_k_diff_data <- expand.grid(k = k_values, x = x_values)
Phi_k_diff_data$diff <- Phi_k_diff_v(Phi_k_diff_data$x, Phi_k_diff_data$k)

library(ggplot2)
ggplot(Phi_k_diff_data, aes(x = x, y = diff, color = factor(k))) +
  scale_y_log10() +
  geom_line() +
  geom_line(aes(y = 2^(-2 * k - 2))) +
  labs(
    x = "x",
    y = "log10 of the 1-norm error over domain [0,1]"
  ) -> Phi_k_diff_plot
ggsave("Phi_k_properties/diff.png", plot = Phi_k_diff_plot, width = 6, height = 5, units = "in")

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
