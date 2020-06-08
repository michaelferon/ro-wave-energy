rm(list = ls())

## Load libraries.
library(lubridate)
library(viridis)
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)


files <- list.files('./raw', full.names = TRUE)

df <- read_csv(files[1], skip = 1)
