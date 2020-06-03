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










