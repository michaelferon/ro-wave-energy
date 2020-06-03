library(tidyverse)
library(ggplot2)
library(lubridate)




load("/Users/jonathanyun/Downloads/roenergyMoWater.rda")
View(fulldata)


exp1 <- which(fulldata$experiment == 1)
exp2 <- which(fulldata$experiment == 2)
exp3 <- which(fulldata$experiment == 3)
exp4 <- which(fulldata$experiment == 4)
exp5 <- which(fulldata$experiment == 5)
exp6 <- which(fulldata$experiment == 6)
exp7 <- which(fulldata$experiment == 7)
exp8 <- which(fulldata$experiment == 8)
exp9 <- which(fulldata$experiment == 9)
exp10 <- which(fulldata$experiment == 10)
exp11 <- which(fulldata$experiment == 11)


ExperimentData1 <- fulldata[exp1,]
View(ExperimentData1)

ExperimentData2 <- fulldata[exp2,]
View(ExperimentData2)

ExperimentData3 <- fulldata[exp3,]
View(ExperimentData3)

ExperimentData4 <- fulldata[exp4,]
View(ExperimentData4)

ExperimentData5 <- fulldata[exp5,]
View(ExperimentData5)

ExperimentData6 <- fulldata[exp6,]
View(ExperimentData6)

ExperimentData7 <- fulldata[exp7,]
View(ExperimentData7)

ExperimentData8 <- fulldata[exp8,]
View(ExperimentData8)

ExperimentData9 <- fulldata[exp9,]
View(ExperimentData9)

ExperimentData10 <- fulldata[exp10,]
View(ExperimentData10)

ExperimentData11 <- fulldata[exp11,]
View(ExperimentData11)



