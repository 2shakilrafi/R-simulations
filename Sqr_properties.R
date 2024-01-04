source("aux_fun.R")
source("Sqr.R")
source("realization.R")
source("activations.R")

#' Sqr_diff function
#'
#' @param q parameter for the Sqr network
#' @param eps parameter for the Sqr network
#' @param x the number to be squered
#'
#' @return a neural network that approximately squares x.

Sqr_diff <- function(q, eps, x) {
  return <- (Sqr(q, eps) |> rlz(ReLU, x) - x^2) |> abs()
  return(return)
}

Sqr_diff_v <- Vectorize(Sqr_diff)

Sqr_data <- expand.grid(
  q = seq(2.01, 4, length.out = 50),
  eps = seq(0.01, 2, length.out = 50),
  x = seq(-5, 5, length.out = 50)
)



Sqr_data$diff <- Sqr_diff_v(Sqr_data$q, Sqr_data$eps, Sqr_data$x)

#' Function to calculate the theoretical upper bounds of the 1-norm error
#' over \mathbb{R}
#'
#' @param q parameter for the Sqr network
#' @param eps parameter for the Sqr network
#' @param x the number to be squered
#'
#' @return the maximum 1-norm error over \mathbb{R}

diff_upper_limit <- function(q, eps, x) {
  eps * max(1, abs(x)^q)
}

diff_upper_limit_v <- Vectorize(diff_upper_limit)

Sqr_data$diff_upper_limit <- diff_upper_limit_v(Sqr_data$q, Sqr_data$eps, Sqr_data$x)

library(plotly)

fig <- plot_ly(
  type = "isosurface",
  x = Sqr_data$x,
  y = Sqr_data$q,
  z = Sqr_data$eps,
  value = Sqr_data$diff,
  isomin = 0.0001,
  isomax = 5,
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

library(ggplot2)

Sqr_data_aux <- expand.grid(
  q = seq(2, 10, length.out = 100),
  eps = seq(0.01, 4, length.out = 100)
)

Sqr_data_aux$param <- 0

for (k in 1:10000) {
  Sqr_data_aux$param[k] <- Sqr(Sqr_data_aux$q[k], Sqr_data_aux$eps[k]) |> param()
}

experimental_params <- ggplot(Sqr_data_aux, aes(x = q, y = eps, z = param)) +
  geom_contour_filled() +
  theme_minimal() +
  scale_y_log10() +
  labs(fill = "#Number of parameters")

Sqr_data_aux$dep <- 0

for (k in 1:10000) {
  Sqr_data_aux$dep[k] <- Sqr(Sqr_data_aux[k, ]$q, Sqr_data_aux[k, ]$eps) |> dep()
}

experimental_deps <- ggplot(Sqr_data_aux, aes(x = q, y = eps, z = dep)) +
  geom_contour_filled(alpha = 0.8, breaks = seq(0, 4, 1)) +
  scale_y_log10() +
  # scale_fill_continuous(breaks = seq(0, max(Sqr_data_aux$dep), by = 1)) +
  theme_minimal() +
  labs(fill = "Depth")

param_upper_limit <- function(q, eps) {
  (((40 * q) / (q - 2)) * ((1 / eps) |> log(2)) + 80 / (q - 2) - 28) |> max(52)
}

dep_upper_limit <- function(q, eps) {
  ((q / (2 * q - 4)) * log2(1 / eps) + 1 / (q - 2) + 1 / (q - 2) + 1) |> max(2)
}

Sqr_data_aux$param_upper_limit <- 0

for (k in 1:10000) {
  Sqr_data_aux$param_upper_limit[k] <- param_upper_limit(Sqr_data_aux[k, ]$q, Sqr_data_aux[k, ]$eps) |>
    ceiling()
}

param_theoretical_upper_limits <- ggplot(Sqr_data_aux, aes(x = q, y = eps, z = log10(param_upper_limit))) +
  geom_contour_filled() +
  theme_minimal() +
  scale_y_log10() +
  labs(fill = "Log10 upper limits of parameters")

Sqr_data_aux$dep_upper_limit <- 0

for (k in 1:10000) {
  Sqr_data_aux$dep_upper_limit[k] <- dep_upper_limit(Sqr_data_aux[k, ]$q, Sqr_data_aux[k, ]$eps) |>
    ceiling()
}

dep_theoretical_upper_limits <- ggplot(Sqr_data_aux, aes(x = q, y = eps, z = log10(dep_upper_limit))) +
  geom_contour_filled() +
  theme_minimal() +
  scale_y_log10() +
  labs(fill = "Log10 upper limits of depth")

ggsave("Sqr_properties/param_theoretical_upper_limits.png", plot = param_theoretical_upper_limits, width = 6, height = 5, units = "in")

ggsave("Sqr_properties/dep_theoretical_upper_limits.png", plot = dep_theoretical_upper_limits, width = 6, height = 5, units = "in")

ggsave("Sqr_properties/experimental_deps.png", plot = experimental_deps, width = 6, height = 5, units = "in")

ggsave("Sqr_properties/experimental_params.png", plot = experimental_params, width = 6, height = 5, units = "in")
