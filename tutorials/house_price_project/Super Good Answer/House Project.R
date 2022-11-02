House <- read.csv("https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv")

library(tidyverse)

House %>%
  tidyr::pivot_wider(
    contains("Q7_"),
    names_to = ("Q7"),
    values_to = ("programming_language"))

plot()

boxplot() 