rm(list = ls())

## Load libraries.
library(lubridate)
library(viridis)
library(ggplot2)
library(tidyr)
library(dplyr)

## Load data and convert to tibble.
load('../data/roenergyMoWater.rda')
data <- fulldata %>% as_tibble
rm(fulldata)


## Convert date, time to time: POSIXct.
data$time <- paste(data$date, data$time) %>%
  parse_date_time('%Y-%m-%d %H:%M:%S %p')


## Remove some variables.
# 'permeate_tank_level', 'feed_cleaning_valve_state', 'temperature_valve_open_pct',
# and 'permeate_discharge_valve_state' have only one factor value.
del <- c('permeate_tank_level', 'feed_cleaning_valve_state',
         'permeate_discharge_valve_state', 'temperature_valve_open_pct')
for (i in del) { print(length(unique(data[[i]]))) }
data <- data %>%
  select(-date, -iteration, -all_of(del))
rm(del)


## Experiments 2, 11 and 1, 6 appear to be identical.
# Here's a demonstration.
data %>%
  filter(experiment == 2 | experiment == 11) %>%
  select(-time, -system_mode) %>%
  group_by(experiment) %>%
  summarise_all(mean) %>%
  as.data.frame
data %>%
  filter(experiment == 1 | experiment == 6) %>%
  select(-time, -system_mode) %>%
  group_by(experiment) %>%
  summarise_all(mean) %>%
  as.data.frame
data <- data %>%
  filter(experiment != 6 & experiment != 11)
# Re-number experiments so as to be 1-9.
for (i in 7:10) {
  data$experiment[data$experiment == i] <- i - 1
}
# Factor variables.
data <- data %>%
  mutate_at(c('system_mode', 'experiment'), as.factor)


## Save new data to data.Rdata.
save(data, file = '../data/data.Rdata')

