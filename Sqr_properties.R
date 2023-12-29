source("aux_fun.R")
source("Sqr.R")
source("realization.R")
source("activations.R")

diff <- function(q, eps, x) {
  return <- (Sqr(q,eps) |> rlz(ReLU,x) - x^2) |> abs()
  return(return)
}

Sqr_data <- expand.grid(
  q = seq(2, 4, length.out = 500),
  eps = seq(0.1, 2, length.out = 50),
  x = seq(-5,5, length.out = 50)
)

vectorized_diff <- diff |> Vectorize()

Sqr_data$diff <- vectorized_diff(Sqr_data$q, Sqr_data$eps, Sqr_data$x)



library(plotly)

fig <- plot_ly(
  type = 'isosurface',
  x = Sqr_data$x,
  y = Sqr_data$q,
  z = Sqr_data$eps,
  value = Sqr_data$diff,
  isomin = 0.0001,
  isomax = 5,
  colorscale = 'RdBu'
) |> 
  layout(scene = list(xaxis = list(title = "x"),
                          yaxis = list(title = "q"),
                          zaxis = list(title = "eps"))) |>
  layout(scene = list(legend = list(title = "Diff from x^2"))) |>
  layout(title = "Isosurface plot of 1-norm error vs parameters")


fig
