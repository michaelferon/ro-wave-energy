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



.
## Feed Pressure distribution by experiment.
ggplot(data, aes(x = feed_pressure_psi, color = experiment)) +
  geom_density() +
  ggtitle('Feed Pressure Distribution') +
  labs(subtitle = 'by experiment', color = 'Exp.') +
  xlab('Feed Pressure (psi)') + ylab('Density') +
  theme_minimal()


## Plots of conductivity for each experiment.
for (i in 1:N) {
  df <- gather(data, key = measure, value = value,
               c('permeate_conductivity_high_us', 'permeate_conductivity_low_us'))
  if (OUTPUT) {
    pdf(file = paste('../plots/permeate_conductivity/permeate_conductivity', i,
                     '.pdf', sep = ''), height = 4.0, width = 8.67)
  }
  g <- df %>%
    filter(experiment == i) %>%
    ggplot(aes(time, value, group = measure, color = measure)) +
    geom_line() +
    ggtitle('Permeate Conductivity') +
    labs(subtitle = paste('Experiment', i), color = 'Measure') +
    xlab('Time') + ylab('Permeate Conductivity') + ylim(0, 1000) +
    scale_color_manual(values = c('SteelBlue', 'Orange3'),
                       labels = c('High', 'Low')) +
    theme_minimal()
  print(g)
  if (OUTPUT) {
    dev.off()
  }
}
rm(df, g)


## Plots of feed pressure for each experiment.
for (i in 1:N) {
  if (OUTPUT) {
    pdf(file = paste('../plots/feed_pressure/feed_pressure', i,
                     '.pdf', sep = ''), height = 4.0, width = 8.67)
  }
  g <- data %>%
    filter(experiment == i) %>%
    ggplot(aes(time, feed_pressure_psi)) +
    geom_line(size = 0.25) +
    ggtitle('Feed Pressure') +
    labs(subtitle = paste('Experiment', i)) +
    xlab('Time') + ylab('Feed Pressure (psi)') + ylim(-5.61, 1114.9) +
    theme_minimal()
  print(g)
  if (OUTPUT) {
    dev.off()
  }
}


## Plots of feed flowrate for each experiment.
for (i in 1:N) {
  if (OUTPUT) {
    pdf(file = paste('../plots/feed_flowrate/feed_flowrate', i,
                     '.pdf', sep = ''), height = 4.0, width = 8.67)
  }
  g <- data %>%
    filter(experiment == i) %>%
    ggplot(aes(time, feed_flowrate_l_min)) +
    geom_line(size = 0.25) +
    ggtitle('Feed Flowrate') +
    labs(subtitle = paste('Experiment', i)) +
    xlab('Time') + ylab('Feed Flowrate') +
    theme_minimal()
  print(g)
  if (OUTPUT) {
    dev.off()
  }
}







