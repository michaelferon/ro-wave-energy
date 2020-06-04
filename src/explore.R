rm(list = ls())

## Load libraries.
library(lubridate)
library(viridis)
library(ggplot2)
library(tidyr)
library(dplyr)

## Load data.
load('../data/data.Rdata')


## Feed Pressure distribution by experiment.
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
    scale_color_manual(values = c('SteelBlue', 'Orange3'),
                       labels = c('High', 'Low')) +
    theme_minimal()
  print(g)
  dev.off()
}
rm(df, g)

data %>%
  ggplot(aes(x = time, y = permeate_conductivity_high_us)) +
  geom_line(color = 'steelblue') +
  facet_grid(rows = vars(experiment))




