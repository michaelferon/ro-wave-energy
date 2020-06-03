rm(list = ls())

## Load libraries.
library(lubridate)
library(viridis)
library(ggplot2)
library(dplyr)

## Load data and convert to tibble.
load('../data/roenergyMoWater.rda')
data <- fulldata %>% as_tibble


## Convert date, time to time: POSIXct.
data$time <- paste(data$date, data$time) %>%
  parse_date_time('%Y-%m-%d %H:%M:%S %p')


## Clean up a bit.
data <- data %>%
  select(-date)
data$experiment <- as.factor(data$experiment)
rm(temp)


## Experiments 2, 11 and 1, 6 appear to be identical.
data %>%
  filter(experiment == 2 | experiment == 11) %>%
  select(-time, -permeate_tank_level, -feed_cleaning_valve_state,
         -permeate_discharge_valve_state, -system_mode) %>%
  group_by(experiment) %>%
  summarise_all(mean) %>%
  as.data.frame
data %>%
  filter(experiment == 1 | experiment == 6) %>%
  select(-time, -permeate_tank_level, -feed_cleaning_valve_state,
         -permeate_discharge_valve_state, -system_mode) %>%
  group_by(experiment) %>%
  summarise_all(mean) %>%
  as.data.frame


## Checking for constant frequency for each experiment.
for (i in 1:11) {
  print(i)
  print(unique(diff(data$iteration[data$experiment == i])))
  # print(unique(diff(data$time[data$experiment == i])))
}


## Feed Pressure distribution plot by experiment.
ggplot(data, aes(x = feed_pressure_psi, color = experiment)) +
  geom_density() +
  ggtitle('Feed Pressure Distribution') +
  labs(subtitle = 'by experiment', color = 'Exp.') +
  xlab('Feed Pressure (psi)') + ylab('Density') +
  theme_minimal()


## Some plots of conductivity.
data %>%
  filter(experiment == 2) %>%
  ggplot(aes(x = time, y = permeate_conductivity_high_us)) +
  geom_line(color = 'steelblue') +
  geom_line(aes(x = time, y = permeate_conductivity_low_us), color = 'orangered3')

data %>%
  ggplot(aes(x = time, y = permeate_conductivity_high_us)) +
  geom_line(color = 'steelblue') +
  facet_grid(rows = vars(experiment))










