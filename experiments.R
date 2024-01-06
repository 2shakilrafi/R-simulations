source("nn_sum.R")
source("scalar_mult.R")
source("Pwr.R")

library(tidyverse)

Sqr_data |>
  ggplot(aes(diff)) +
  geom_density() +
  geom_density()

Pwr_3_data_aux |>
  select(param) |> 
  summary()
