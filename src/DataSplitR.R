
#load library
library(tidyverse)
library(ggplot2)
library(lubridate)


#load data
load('../data/data.Rdata')
View(fulldata)

data <- fulldata %>% as_tibble


data$time <- paste(data$date, data$time) %>%
  parse_date_time('%Y-%m-%d %H:%M:%S %p')

data <- data %>%
          select(-date)
data$experiment <- as.factor(data$experiment)
View(data)

#get rid of duplicate data rows
which(data$experiment == 6)
which(data$experiment == 11)

data <- data %>%
  filter(experiment != 6 & experiment != 11) %>%
  select(-iteration, -permeate_tank_level, -feed_cleaning_valve_state, -permeate_discharge_valve_state)

pressureDataExp1 <- data %>%
                  filter(experiment == 1) %>%
                  select(feed_pressure_psi)

pressureDataExp2 <- data %>%
                  filter(experiment == 2) %>%
                  select(feed_pressure_psi)

pressureDataExp3 <- data %>%
    filter(experiment == 3) %>%
  select(feed_pressure_psi)

pressureDataExp4 <- data %>%
  filter(experiment == 4) %>%
  select(feed_pressure_psi)

unique(data$experiment)

#permeability graphs
data %>%
  filter(experiment == 1) %>%
  ggplot(aes(x = time, y = permeate_conductivity_high_us)) +
  geom_line(color = 'steelblue') +
  geom_line(aes(x = time, y = permeate_conductivity_low_us), color = 'orangered3')


data %>%
  filter(experiment == 2) %>%
  ggplot(aes(x = time, y = permeate_conductivity_high_us)) +
  geom_line(color = 'steelblue') +
  geom_line(aes(x = time, y = permeate_conductivity_low_us), color = 'orangered3')

data %>%
  filter(experiment == 3) %>%
  ggplot(aes(x = time, y = permeate_conductivity_high_us)) +
  geom_line(color = 'steelblue') +
  geom_line(aes(x = time, y = permeate_conductivity_low_us), color = 'orangered3')

data %>%
  filter(experiment == 4) %>%
  ggplot(aes(x = time, y = permeate_conductivity_high_us)) +
  geom_line(color = 'steelblue') +
  geom_line(aes(x = time, y = permeate_conductivity_low_us), color = 'orangered3')

data %>%
  filter(experiment == 5) %>%
  ggplot(aes(x = time, y = permeate_conductivity_high_us)) +
  geom_line(color = 'steelblue') +
  geom_line(aes(x = time, y = permeate_conductivity_low_us), color = 'orangered3')

data %>%
  filter(experiment == 7) %>%
  ggplot(aes(x = time, y = permeate_conductivity_high_us)) +
  geom_line(color = 'steelblue') +
  geom_line(aes(x = time, y = permeate_conductivity_low_us), color = 'orangered3')

data %>%
  filter(experiment == 8) %>%
  ggplot(aes(x = time, y = permeate_conductivity_high_us)) +
  geom_line(color = 'steelblue') +
  geom_line(aes(x = time, y = permeate_conductivity_low_us), color = 'orangered3')

data %>%
  filter(experiment == 9) %>%
  ggplot(aes(x = time, y = permeate_conductivity_high_us)) +
  geom_line(color = 'steelblue') +
  geom_line(aes(x = time, y = permeate_conductivity_low_us), color = 'orangered3')

data %>%
  filter(experiment == 10) %>%
  ggplot(aes(x = time, y = permeate_conductivity_high_us)) +
  geom_line(color = 'steelblue') +
  geom_line(aes(x = time, y = permeate_conductivity_low_us), color = 'orangered3')



 exp1 <- data %>%
  filter(experiment == 1)
 
 

 
 #conversion equation Luke version
 conversion <- function(pi_rej){
   ppm <- pi_rej / 2
   mole_per_liter <- ppm /35500
   return(mole_per_liter)
 }
 
 #temperature conversion equation
 kelvin_conversion <- function(temp){
   kel <- temp + 273.15
 }
 
 #pressure conversion equation
 atm_conversion <- function(press){
   atm <- press * 0.068046
   return(atm)
 }
 
 #pi_reject
 #units of the constant are in L atm mol^-1 K^-1
 pi_reject <- function(conversion, temp){
   pi_r <- conversion * 0.08205 * temp
 }
 
 
 #permeability coefficient function? 
 #water_flux = water flux 
 #feed_press = P_in (feed pressure)
 #press_feed_sol = pi_feed
 #press_reject = pi_rej
 #press_perm = pi_perm
 #R constant = L atm mol^-1 K^-1
 
 water_perm_coeff <- function(water_flux, feed_press, pi_rej, temp){
   #Luke conversion equation
   ppm <- pi_rej / 2
   mole_per_liter <- ppm /35500
   
   #temperature conversion
   kel <- temp + 273.15
   
   #pressure conversion
   atm <- feed_press * 0.068046
   
   #pi_reject calculation
   pi_r <- mole_per_liter * 0.08205 * kel
   
   coeff <- water_flux/ (atm - ((400 + pi_r) / 2))
   return(coeff)
 }
 
 
 #test of function

 

