library(lubridate)
library(ggplot2)
library(dplyr)

load('../data/roenergyMoWater.rda')

data <- fulldata %>% as_tibble
