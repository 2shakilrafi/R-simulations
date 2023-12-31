source("Phi.R")
source("aux_fun.R")
source("realization.R")
source("activations.R")

diff <- function(eps, x) {
  (x^2 - eps |> Phi() |> rlz(ReLU, x)) |>
    abs()
}

eps_values <- c(1, 0.5, 0.1, 0.01, 0.001, 0.0001)
x_values <- seq(0, 1, length.out = 100)
vectorized_diff <- Vectorize(diff)

diff_data <- expand.grid(eps = eps_values, x = x_values)
diff_data$Phi_diff <- vectorized_diff(diff_data$eps, diff_data$x)

library(ggplot2)

ggplot(diff_data, aes(x = x, y = eps, z = Phi_diff)) +
  geom_contour_filled() +
  ggtitle("Contour plot of the 1-norm difference for values of x and eps") +
  theme_minimal() -> Phi_diff_contour_plot

ggsave("Phi_properties/Phi_diff_contour.png", plot = Phi_diff_contour_plot, width = 6, height = 5, units = "in")

vectorized_Phi_k <- Vectorize(Phi_k)
vectorized_param <- Vectorize(param)

param_data <- data.frame(x = 1:100, y = vectorized_param(vectorized_Phi_k(1:100)))

ggplot(param_data, aes(x, y)) +
  geom_line() +
  theme_minimal()



vectorized_dep <- Vectorize(dep)

dep_data <- data.frame(x = 1:100, y = vectorized_dep(vectorized_Phi_k(1:100)))

ggplot(dep_data, aes(x = x, y = y)) +
  geom_line() +
  theme_minimal() +
  xlab("Size of k") +
  ylab("Depth of network") +
  ggtitle("Plot of the depth of ϕ(k) against k") +
  geom_smooth(method = "lm", se = FALSE, color = "blue")
