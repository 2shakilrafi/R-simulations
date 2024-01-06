source("aux_fun.R")
source("Prd.R")
source("realization.R")
source("activations.R")
source("Prd.R")

#' The Prd_diff function
#'
#' @param q parameter for Prd function
#' @param eps parameter for Prd function
#' @param x one of the factors in the product
#' @param y one of the factors in the product
#'
#' @return The 1-norm error over \mathbb{R} between xy and the approximation
#' given by Prd.network
#
Prd_diff <- function(q, eps, x, y) {
  ((Prd(q, eps)) |> rlz(ReLU, c(x, y)) - x * y) |> abs() -> result
  return(result)
}

Prd_diff_v <- Vectorize(Prd_diff)

Prd_data <- expand.grid(
  q = seq(2.1, 4, length.out = 20),
  eps = seq(0.01, 2, length.out = 20),
  x = seq(-2, 2, length.out = 20),
  y = seq(-2, 2, length.out = 20)
)

Prd_data$xy <- Prd_data$x * Prd_data$y

Prd_data$diff <- Prd_diff_v(Prd_data$q, Prd_data$eps, Prd_data$x, Prd_data$y)


library(plotly)

fig <- plot_ly(
  type = "isosurface",
  x = Prd_data$xy,
  y = Prd_data$q,
  z = Prd_data$eps,
  value = Prd_data$diff,
  isomin = 0.0001,
  isomax = 50,
  colorscale = "RdBu"
) |>
  layout(scene = list(
    xaxis = list(title = "x"),
    yaxis = list(title = "q"),
    zaxis = list(title = "eps")
  )) |>
  layout(scene = list(legend = list(title = "Diff from x^2"))) |>
  layout(title = "Isosurface plot of 1-norm error vs parameters")

fig

ggplot(Prd_data, aes(diff)) +
  scale_x_log10() +
  geom_density(fill = "green", alpha = 0.6) +
  theme_minimal() +
  labs(
    x = "log10 error",
    y = "density distribution of error",
    title = "Distribution of log10 error"
  ) -> Prd_diff_hist_plot

ggsave("Prd_properties/Prd_diff_hist_plot.png", plot = Prd_diff_hist_plot, width = 6, height = 5, units = "in")

library(ggplot2)

Prd_data_aux <- expand.grid(
  q = seq(2, 10, length.out = 100),
  eps = seq(0.01, 4, length.out = 100)
)

Prd_data_aux$param <- 0

for (k in 1:10000) {
  Prd_data_aux$param[k] <- Prd(Prd_data_aux$q[k], Prd_data_aux$eps[k]) |> param()
}

experimental_params <- ggplot(Prd_data_aux, aes(x = q, y = eps, z = param)) +
  geom_contour_filled() +
  theme_minimal() +
  scale_y_log10() +
  labs(fill = "#Number of parameters")

Prd_data_aux$dep <- 0

for (k in 1:10000) {
  Prd_data_aux$dep[k] <- Prd(Prd_data_aux[k, ]$q, Prd_data_aux[k, ]$eps) |> dep()
}

experimental_deps <- ggplot(Prd_data_aux, aes(x = q, y = eps, z = dep)) +
  geom_contour_filled(alpha = 0.8, breaks = seq(0, 4, 1)) +
  scale_y_log10() +
  # scale_fill_continuous(breaks = seq(0, max(Prd_data_aux$dep), by = 1)) +
  theme_minimal() +
  labs(fill = "Depth")

#' The param_upper_limit function
#'
#' @param q parmeter for the Prd network
#' @param eps parameter for the Prd network
#'
#' @return the theoretical upper limit for the number of parameters

param_upper_limit <- function(q, eps) {
  (((40 * q) / (q - 2)) * ((1 / eps) |> log(2)) + 80 / (q - 2) - 28) |> max(52)
}

#' The dep_upper_limit function
#'
#' @param q parameter for the Prd network
#' @param eps parameter for the Prd network
#'
#' @return the theoretical upper limit for depth

dep_upper_limit <- function(q, eps) {
  ((q / (2 * q - 4)) * log2(1 / eps) + 1 / (q - 2) + 1 / (q - 2) + 1) |> max(2)
}

Prd_data_aux$param_upper_limit <- 0

for (k in 1:10000) {
  Prd_data_aux$param_upper_limit[k] <- param_upper_limit(Prd_data_aux[k, ]$q, Prd_data_aux[k, ]$eps) |>
    ceiling()
}

param_theoretical_upper_limits <- ggplot(Prd_data_aux, aes(x = q, y = eps, z = log10(param_upper_limit))) +
  geom_contour_filled() +
  theme_minimal() +
  scale_y_log10() +
  labs(fill = "Log10 upper limits of parameters")

Prd_data_aux$dep_upper_limit <- 0

for (k in 1:10000) {
  Prd_data_aux$dep_upper_limit[k] <- dep_upper_limit(Prd_data_aux[k, ]$q, Prd_data_aux[k, ]$eps) |>
    ceiling()
}

dep_theoretical_upper_limits <- ggplot(Prd_data_aux, aes(x = q, y = eps, z = log10(dep_upper_limit))) +
  geom_contour_filled() +
  theme_minimal() +
  scale_y_log10() +
  labs(fill = "Log10 upper limits of depth")

ggsave("Prd_properties/param_theoretical_upper_limits.png", plot = param_theoretical_upper_limits, width = 6, height = 5, units = "in")
ggsave("Prd_properties/dep_theoretical_upper_limits.png", plot = dep_theoretical_upper_limits, width = 6, height = 5, units = "in")
ggsave("Prd_properties/experimental_deps.png", plot = experimental_deps, width = 6, height = 5, units = "in")
ggsave("Prd_properties/experimental_params.png", plot = experimental_params, width = 6, height = 5, units = "in")
