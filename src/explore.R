rm(list = ls())

## Load libraries.
library(lubridate)
library(viridis)
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)

## Load data.
load('../data/data.Rdata')

## Number of experiments = 9.
N <- 9
OUTPUT <- FALSE


## Experiments summarries.
tapply(data$experiment, data$experiment, length)
(data.mean <- data %>%
  select(-time, -system_mode) %>%
  group_by(experiment) %>%
  summarise_all(mean, na.rm = TRUE))
(data.sd <- data %>%
    select(-time, -system_mode) %>%
    group_by(experiment) %>%
    summarise_all(sd, na.rm = TRUE))
(data.iqr <- data %>%
    select(-time, -system_mode) %>%
    group_by(experiment) %>%
    summarise_all(IQR, na.rm = TRUE))
(data.mad <- data %>%
    select(-time, -system_mode) %>%
    group_by(experiment) %>%
    summarise_all(mad, na.rm = TRUE))

if (OUTPUT) { # Write mean summary to file.
  data %>%
    select(-time, -system_mode) %>%
    group_by(experiment) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    write_csv(path = '../data/summary.csv')
}




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
           permeate_conductivity_low_us, reject_conductivity_ms) %>%
    rename(
      flux = water_flux_lmh,
      feed_psi = feed_pressure_psi,
      feed_vol = feed_volume_l,
      feed_flow = feed_flowrate_l_min,
      feed_pow = feed_pump_power_pct,
      perm_flow = permeate_flowrate_l_min,
      perm_cond = permeate_conductivity_low_us,
      rej_cond = reject_conductivity_ms,
    ) %>%
    pairs(cex = 0.10) #, col = colors[i])
  if (OUTPUT) {
    dev.off()
  }
}


## Boxplot by experiment.
del <- c('time', 'permeate_conductivity_high_us', 'reject_valve_open_pct',
         'system_mode', 'experiment', 'ac_current_a')
vars <- names(data)[!(names(data) %in% del)]
for (var in vars) {
  boxplot(data[[var]] ~ data$experiment, xlab = 'Experiment', ylab = var,
          main = 'Boxplots by experiment')
}


## Simple linear model.
model <- data %>%
  select(-time, -permeate_conductivity_high_us, -reject_conductivity_ms,
         -experiment, -ac_current_a) %>%
  lm(permeate_conductivity_low_us ~ ., data = .)
summary(model)
plot(model)










