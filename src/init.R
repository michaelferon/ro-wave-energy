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
# 'permeate_tank_level', 'feed_cleaning_valve_state', and
# 'permeate_discharge_valve_state' have only one factor value.
del <- c('permeate_tank_level', 'feed_cleaning_valve_state',
         'permeate_discharge_valve_state')
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




## Feed Pressure distribution plot by experiment.
ggplot(data, aes(x = feed_pressure_psi, color = experiment)) +
  geom_density() +
  ggtitle('Feed Pressure Distribution') +
  labs(subtitle = 'by experiment', color = 'Exp.') +
  xlab('Feed Pressure (psi)') + ylab('Density') +
  theme_minimal()


## Some plots of conductivity.
for (i in 1:9) {
  df <- gather(data, key = measure, value = value,
               c('permeate_conductivity_high_us', 'permeate_conductivity_low_us'))
  pdf(file = paste('../plots/permeate_conductivity/permeate_conductivity', i,
                   '.pdf', sep = ''), height = 3.0, width = 6.5)
  g <- df %>%
    filter(experiment == i) %>%
    ggplot(aes(time, value, group = measure, color = measure)) +
    geom_line() +
    ggtitle('Permeate Conductivity') +
    labs(subtitle = paste('Experiment', i), color = 'Measure') +
    xlab('Time') + ylab('Permeate Conductivity') + ylim(0, 1000) +
    scale_color_manual(values = c('SteelBlue', 'Orange3')) +
    theme_minimal()
  print(g)
  dev.off()
}

data %>%
  ggplot(aes(x = time, y = permeate_conductivity_high_us)) +
  geom_line(color = 'steelblue') +
  facet_grid(rows = vars(experiment))










