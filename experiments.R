source("nn_sum.R")
source("scalar_mult.R")
source("Pwr.R")

library(tidyverse)


Pwr(2.1, 0.1, 0) |> nn_sum(Pwr(2.1, 0.1, 1))
