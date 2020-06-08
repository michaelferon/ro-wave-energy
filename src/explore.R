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
data %>%
  select(-time, -runtime_h, -system_mode) %>%
  group_by(experiment) %>%
  summarise_all(mean) %>%
  as.data.frame




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









