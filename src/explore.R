rm(list = ls())

## Load libraries.
library(lubridate)
library(viridis)
library(ggplot2)
library(tidyr)
library(dplyr)

## Load data.
load('../data/data.Rdata')

## Number of experiments = 9.
N <- 9
OUTPUT <- FALSE


## Experiments summary.
tapply(data$experiment, data$experiment, length)
(data.mean <- data %>%
  select(-time, -runtime_h, -system_mode) %>%
  group_by(experiment) %>%
  summarise_all(mean, na.rm = TRUE))
(data.sd <- data %>%
    select(-time, -runtime_h, -system_mode) %>%
    group_by(experiment) %>%
    summarise_all(sd, na.rm = TRUE))
(data.iqr <- data %>%
    select(-time, -runtime_h, -system_mode) %>%
    group_by(experiment) %>%
    summarise_all(IQR, na.rm = TRUE))
(data.mad <- data %>%
    select(-time, -runtime_h, -system_mode) %>%
    group_by(experiment) %>%
    summarise_all(mad, na.rm = TRUE))




## Feed Pressure distribution by experiment.
ggplot(data, aes(x = feed_pressure_psi, color = experiment)) +
  geom_density() +
  ggtitle('Feed Pressure Distribution') +
  labs(subtitle = 'by experiment', color = 'Exp.') +
  xlab('Feed Pressure (psi)') + ylab('Density') +
  theme_minimal()


## Scatterplot matrices.
colors <- plasma(9)
for (i in 1:N) {
  if (OUTPUT) {
    pdf(file = paste('../plots/scatter_matrix/scatter_matrix', i,
                     '.pdf', sep = ''), height = 10.0, width = 10.0)
  }
  data %>%
    filter(experiment == i) %>%
    select(water_flux_lmh, feed_pressure_psi, feed_volume_l, feed_flowrate_l_min,
           feed_pump_power_pct, permeate_flowrate_l_min,
           permeate_conductivity_low_us, reject_flowrate_l_min,
           reject_conductivity_ms) %>%
    rename(
      flux = water_flux_lmh,
      feed_pressure = feed_pressure_psi,
      feed_volume = feed_volume_l,
      feed_flow = feed_flowrate_l_min,
      feed_power = feed_pump_power_pct,
      perm_flow = permeate_flowrate_l_min,
      perm_cond = permeate_conductivity_low_us,
      rej_flow = reject_flowrate_l_min,
      rej_cond = reject_conductivity_ms,
    ) %>%
    pairs(cex = 0.10) #, col = colors[i])
  if (OUTPUT) {
    dev.off()
  }
}


## Boxplot by experiment.
del <- c(1, 2, 3, 8, 13, 14, 16, 17, 18, 19)
del <- c('time', 'runtime_h', 'permeate_conductivity_high_us',
         'reject_valve_open_pct', 'system_mode', 'experiment', 'ac_current_a')
vars <- names(data)[!(names(data) %in% del)]
for (var in vars) {
  boxplot(data[[var]] ~ data$experiment, xlab = 'Experiment', ylab = var,
          main = 'Boxplots by experiment')
}


## Simple linear model.
model <- data %>%
  select(-time, -runtime_h, -permeate_conductivity_high_us, -reject_conductivity_ms,
         -experiment, -ac_current_a) %>%
  lm(permeate_conductivity_low_us ~ ., data = .)
summary(model)
plot(model)











