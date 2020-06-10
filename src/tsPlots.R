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



exp_out <- '../plots/ts/experiments/exp'
## Plots of permeate conductivity.
for (i in 1:N) {
  df <- gather(data, key = measure, value = value,
               c('permeate_conductivity_high_us', 'permeate_conductivity_low_us'))
  g <- df %>%
    filter(experiment == i) %>%
    ggplot(aes(time, value, group = measure, color = measure)) +
    geom_line(size = 0.25) +
    ggtitle('Permeate Conductivity') +
    labs(subtitle = paste('Experiment', i), color = 'Measure') +
    xlab('Time') + ylab('Permeate Conductivity (uS)') + #ylim(0, 1100) +
    scale_color_manual(values = c('SteelBlue', 'Orange3'),
                       labels = c('High', 'Low')) +
    theme_minimal()
  if (OUTPUT) {
    pdf(file = paste('../plots/ts/permeate_conductivity/permeate_conductivity', i,
                     '.pdf', sep = ''), height = 4.0, width = 9.5)
    print(g + ylim(240, 1000))
    dev.off()
    
    pdf(file = paste(exp_out, i, '/permeate_conductivity', i, '.pdf', sep=''),
        height = 4.0, width = 9.5)
    print(g)
    dev.off()
  }
  
  #print(g + ylim(240, 1000))
}
rm(df)


## Plots of reject conductivity.
for (i in 1:N) {
  g <- data %>%
    filter(experiment == i) %>%
    ggplot(aes(time, reject_conductivity_ms)) +
    geom_line(size = 0.25) +
    ggtitle('Reject Conductivity') +
    labs(subtitle = paste('Experiment', i)) +
    xlab('Time') + ylab('Reject Conductivity (mS)') +
    theme_minimal()
  if (OUTPUT) {
    pdf(file = paste('../plots/ts/reject_conductivity/reject_conductivity', i,
                     '.pdf', sep = ''), height = 4.0, width = 8.67)
    print(g + ylim(50, 63))
    dev.off()
    
    pdf(file = paste(exp_out, i, '/reject_conductivity', i, '.pdf', sep=''),
        height = 4.0, width = 8.67)
    print(g)
    dev.off()
  }
  
  #print(g + ylim(50, 63))
}


## Plots of feed pressure.
for (i in 1:N) {
  g <- data %>%
    filter(experiment == i) %>%
    ggplot(aes(time, feed_pressure_psi)) +
    geom_line(size = 0.25) +
    ggtitle('Feed Pressure') +
    labs(subtitle = paste('Experiment', i)) +
    xlab('Time') + ylab('Feed Pressure (psi)') +
    theme_minimal()
  if (OUTPUT) {
    pdf(file = paste('../plots/ts/feed_pressure/feed_pressure', i,
                     '.pdf', sep = ''), height = 4.0, width = 8.67)
    print(g + ylim(range(data$feed_pressure_psi)))
    dev.off()
    
    pdf(file = paste(exp_out, i, '/feed_pressure', i, '.pdf', sep=''),
        height = 4.0, width = 8.67)
    print(g)
    dev.off()
  }
  
  #print(g + ylim(range(data$feed_pressure_psi)))
}


## Plots of feed volume.
for (i in 1:N) {
  g <- data %>%
    filter(experiment == i) %>%
    ggplot(aes(time, feed_volume_l)) +
    geom_line(size = 0.25) +
    ggtitle('Feed Volume') +
    labs(subtitle = paste('Experiment', i)) +
    xlab('Time') + ylab('Feed Volume') +
    theme_minimal()
  if (OUTPUT) {
    pdf(file = paste('../plots/ts/feed_volume/feed_volume', i,
                     '.pdf', sep = ''), height = 4.0, width = 8.67)
    print(g + ylim(range(data$feed_volume_l)))
    dev.off()
    
    pdf(file = paste(exp_out, i, '/feed_volume', i, '.pdf', sep=''),
        height = 4.0, width = 8.67)
    print(g)
    dev.off()
  }
  
  #print(g + ylim(range(data$feed_volume_l)))
}


## Plots of permeate flowrate.
for (i in 1:N) {
  g <- data %>%
    filter(experiment == i) %>%
    ggplot(aes(time, permeate_flowrate_l_min)) +
    geom_line(size = 0.25) +
    ggtitle('Permeate Flowrate') +
    labs(subtitle = paste('Experiment', i)) +
    xlab('Time') + ylab('Permeate Flowrate') +
    theme_minimal()
  if (OUTPUT) {
    pdf(file = paste('../plots/ts/permeate_flowrate/permeate_flowrate', i,
                     '.pdf', sep = ''), height = 4.0, width = 8.67)
    print(g + ylim(min(data$permeate_flowrate_l_min), 2.02))
    dev.off()
    
    pdf(file = paste(exp_out, i, '/permeate_flowrate', i, '.pdf', sep=''),
        height = 4.0, width = 8.67)
    print(g)
    dev.off()
  }
  
  #print(g + ylim(min(data$permeate_flowrate_l_min), 2.02))
}


## Plots of reject flowrate.
for (i in 1:N) {
  g <- data %>%
    filter(experiment == i) %>%
    ggplot(aes(time, reject_flowrate_l_min)) +
    geom_line(size = 0.25) +
    ggtitle('Reject Flowrate') +
    labs(subtitle = paste('Experiment', i)) +
    xlab('Time') + ylab('Reject Flowrate') +
    theme_minimal()
  if (OUTPUT) {
    pdf(file = paste('../plots/ts/reject_flowrate/reject_flowrate', i,
                     '.pdf', sep = ''), height = 4.0, width = 8.67)
    print(g + ylim(1, max(data$reject_flowrate_l_min)))
    dev.off()
    
    pdf(file = paste(exp_out, i, '/reject_flowrate', i, '.pdf', sep=''),
        height = 4.0, width = 8.67)
    print(g)
    dev.off()
  }
  
  #print(g + ylim(1, max(data$reject_flowrate_l_min)))
}


## Plots of feed flowrate.
for (i in 1:N) {
  g <- data %>%
    filter(experiment == i) %>%
    ggplot(aes(time, feed_flowrate_l_min)) +
    geom_line(size = 0.25) +
    ggtitle('Feed Flowrate') +
    labs(subtitle = paste('Experiment', i)) +
    xlab('Time') + ylab('Feed Flowrate') +
    theme_minimal()
  if (OUTPUT) {
    pdf(file = paste('../plots/ts/feed_flowrate/feed_flowrate', i,
                     '.pdf', sep = ''), height = 4.0, width = 8.67)
    print(g + ylim(1, 8.25))
    dev.off()
    
    pdf(file = paste(exp_out, i, '/feed_flowrate', i, '.pdf', sep=''),
        height = 4.0, width = 8.67)
    print(g)
    dev.off()
  }
  
  #print(g + ylim(1, 8.25))
}


## Plots of feed pump power.
for (i in 1:N) {
  g <- data %>%
    filter(experiment == i) %>%
    ggplot(aes(time, feed_pump_power_pct)) +
    geom_line(size = 0.25) +
    ggtitle('Feed Pump Power') +
    labs(subtitle = paste('Experiment', i)) +
    xlab('Time') + ylab('Feed Pump Power') +
    theme_minimal()
  if (OUTPUT) {
    pdf(file = paste('../plots/ts/feed_pump_power/feed_pump_power', i,
                     '.pdf', sep = ''), height = 4.0, width = 8.67)
    print(g + ylim(20, max(data$feed_pump_power_pct)))
    dev.off()
    
    pdf(file = paste(exp_out, i, '/feed_pump_power', i, '.pdf', sep=''),
        height = 4.0, width = 8.67)
    print(g)
    dev.off()
  }
  
  #print(g + ylim(20, max(data$feed_pump_power_pct)))
}


## Plots of water flux.
for (i in 1:N) {
  g <- data %>%
    filter(experiment == i) %>%
    ggplot(aes(time, water_flux_lmh)) +
    geom_line(size = 0.25) +
    ggtitle('Water Flux') +
    labs(subtitle = paste('Experiment', i)) +
    xlab('Time') + ylab('Water Flux (lmh)') +
    theme_minimal()
  if (OUTPUT) {
    pdf(file = paste('../plots/ts/water_flux/water_flux', i,
                     '.pdf', sep = ''), height = 4.0, width = 8.67)
    print(g + ylim(0, 45))
    dev.off()
    
    pdf(file = paste(exp_out, i, '/water_flux', i, '.pdf', sep=''),
        height = 4.0, width = 8.67)
    print(g)
    dev.off()
  }
  
  #print(g + ylim(0, 45))
}


## Plots of water permeability.
for (i in 1:N) {
  g <- data %>%
    filter(experiment == i) %>%
    ggplot(aes(time, water_perm)) +
    geom_line(size = 0.25) +
    ggtitle('Water Permeability') +
    labs(subtitle = paste('Experiment', i)) +
    xlab('Time') + ylab('Water Permeability') +
    theme_minimal()
  if (OUTPUT) {
    pdf(file = paste('../plots/ts/water_permeability/water_permeability', i,
                     '.pdf', sep = ''), height = 4.0, width = 8.67)
    print(g + ylim(0, 0.102))
    dev.off()
    
    pdf(file = paste(exp_out, i, '/water_permeability', i, '.pdf', sep=''),
        height = 4.0, width = 8.67)
    print(g)
    dev.off()
  }
  
  #print(g + ylim(0, 0.102))
}






