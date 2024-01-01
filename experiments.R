library(tidyverse)

Pwr_3_data <- read_csv("Pwr_3_properties/Pwr_3_data.csv")

values_of_interest <- seq(-5,5)
tolerance <- 0.1


Sqr_data |> 
  ggplot(aes(x = x)) + 
  geom_line(aes(y = diff), color = "red") +
  geom_line(aes(y = diff_upper_limit), color = "blue") + 
  scale_y_log10()


