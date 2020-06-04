
#load library
library(tidyverse)
library(ggplot2)
library(lubridate)


#load data
load("/Users/jonathanyun/Desktop/ro-wave-energy/data/roenergyMoWater.rda")
View(fulldata)

data <- fulldata %>% as_tibble


data$time <- paste(data$date, data$time) %>%
  parse_date_time('%Y-%m-%d %H:%M:%S %p')

data <- data %>%
          select(-date)
data$experiment <- as.factor(data$experiment)
View(data)

#get rid of duplicate data rows
which(data$experiment == 6)
which(data$experiment == 11)

data <- data %>%
  filter(experiment != 6 & experiment != 11) %>%
  select(-iteration, -permeate_tank_level, -feed_cleaning_valve_state, -permeate_discharge_valve_state)

unique(data$experiment)

data %>%
  filter(experiment == 2) %>%
  ggplot(aes(x = time, y = permeate_conductivity_high_us)) +
  geom_line(color = 'steelblue') +
  geom_line(aes(x = time, y = permeate_conductivity_low_us), color = 'orangered3')


View(data)



