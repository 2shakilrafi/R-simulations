source("Pwr.R")
library(tidyverse)

#' Pwr_3_diff function
#'
#' @param q parameter for Pwr_3
#' @param eps parameter for Pwr_3
#' @param x the number to be cubed
#' @param exponent = 3, i.e. cubing a number

Pwr_3_diff <- function(q, eps, x, exponent = 3) {
  return <- (Pwr(q, eps, exponent = 3) |> rlz(ReLU, x) - x^3) |> abs()
  return(return)
}

Pwr_3_diff_v <- Vectorize(Pwr_3_diff)

Pwr_3_data <- expand.grid(
  q = seq(2.1, 4, length.out = 50),
  eps = seq(0.01, 2, length.out = 50),
  x = seq(-5, 5, length.out = 50)
)

Pwr_3_data$diff <- Pwr_3_diff_v(Pwr_3_data$q, Pwr_3_data$eps, Pwr_3_data$x)

library(ggplot2)

ggplot(Pwr_3_data, aes(diff)) +
  scale_x_log10() +
  geom_density() +
  theme_minimal()

library(plotly)

fig <- plot_ly(
  type = "isosurface",
  x = Pwr_3_data$x,
  y = Pwr_3_data$q,
  z = Pwr_3_data$eps,
  value = Pwr_3_data$diff,
  isomin = 0.0001,
  isomax = 5,
  colorscale = "RdBu"
) |>
  layout(scene = list(
    xaxis = list(title = "x"),
    yaxis = list(title = "q"),
    zaxis = list(title = "eps")
  )) |>
  layout(scene = list(legend = list(title = "Diff from x^2")))

fig

Pwr_3_data_aux <- expand.grid(
  q = seq(2.1, 10, length.out = 100),
  eps = seq(0.01, 4, length.out = 100)
)

Pwr_3_data_aux$param <- 0

for (k in 1:10000) {
  Pwr_3_data_aux$param[k] <- Pwr(Pwr_3_data_aux$q[k], Pwr_3_data_aux$eps[k], exponent = 3) |> param()
}

experimental_params <- ggplot(Pwr_3_data_aux, aes(x = q, y = eps, z = log10(param))) +
  geom_contour_filled() +
  theme_minimal() +
  labs(fill = "log 10 # of parameters")

Pwr_3_data_aux$dep <- 0

for (k in 1:10000) {
  Pwr_3_data_aux$dep[k] <- Pwr(Pwr_3_data_aux[k, ]$q, Pwr_3_data_aux[k, ]$eps, exponent = 3) |> dep()
}

experimental_deps <- ggplot(Pwr_3_data_aux, aes(x = q, y = eps, z = log10(dep))) +
  geom_contour_filled(alpha = 0.8, breaks = seq(0, 10, 1)) +
  # scale_fill_continuous(breaks = seq(0, max(Pwr_3_data_aux$dep), by = 1)) +
  theme_minimal() +
  labs(fill = "log10 depths")

#' The param_upper_limit funnction
#'
#' @param q parameter for the Pwr network
#' @param eps parameter for the Pwr network
#'
#' @return the theoretical upper limit for the number of parameters

param_upper_limit <- function(q, eps) {
  4^(4.5) -> first_summand
  (4^4-1)/3 -> second_summand_a
  ((360*q)/(q-2))*(log2(1/eps)+q+1)+372 -> second_summand_b
  first_summand + (second_summand_a * second_summand_b) -> result
  return(result)
}


#' The dep_upper_limit function
#'
#' @param q parameter for the Pwr_3 network
#' @param eps parameter for the Pwr_3 network
#'
#' @return the theoretical upper limit for the depth

dep_upper_limit <- function(q, eps) {
  ((q / (q - 2)) * (log2(1 / eps) + q) - 1) * 3 + 1
}

Pwr_3_data_aux$param_upper_limit <- 0

for (k in 1:10000) {
  Pwr_3_data_aux$param_upper_limit[k] <- param_upper_limit(Pwr_3_data_aux[k, ]$q, Pwr_3_data_aux[k, ]$eps) |>
    ceiling()
}

param_theoretical_upper_limits <- ggplot(Pwr_3_data_aux, aes(x = q, y = eps, z = log10(param_upper_limit))) +
  geom_contour_filled() +
  theme_minimal() +
  labs(fill = "Log10 upper limits of parameters")

Pwr_3_data_aux$dep_upper_limit <- 0

for (k in 1:10000) {
  Pwr_3_data_aux$dep_upper_limit[k] <- dep_upper_limit(Pwr_3_data_aux[k, ]$q, Pwr_3_data_aux[k, ]$eps)
}

dep_theoretical_upper_limits <- ggplot(Pwr_3_data_aux, aes(x = q, y = eps, z = log10(dep_upper_limit))) +
  geom_contour_filled() +
  theme_minimal() +
  labs(fill = "Log10 upper limits of depth")

ggsave("Pwr_3_properties/param_theoretical_upper_limits.png", plot = param_theoretical_upper_limits, width = 6, height = 5, units = "in")
ggsave("Pwr_3_properties/dep_theoretical_upper_limits.png", plot = dep_theoretical_upper_limits, width = 6, height = 5, units = "in")
ggsave("Pwr_3_properties/experimental_deps.png", plot = experimental_deps, width = 6, height = 5, units = "in")
ggsave("Pwr_3_properties/experimental_params.png", plot = experimental_params, width = 6, height = 5, units = "in")
