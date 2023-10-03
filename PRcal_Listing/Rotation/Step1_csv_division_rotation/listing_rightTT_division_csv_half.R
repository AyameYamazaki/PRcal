##set working directory **************************************************************************

rm(list = ls())

library(ggplot2)
library(tidyverse)
library(readxl)

rotation_direction <- "right"

stop_data <- read.csv(paste("turntable_9pt_", rotation_direction ,"_7.csv", sep=""))
stop_time <- min(stop_data$"Application.Time")

data2 <- read.csv(file=file.choose()) 

extra_time <- stop_time-60



#divide the data into groups of 10sec before rotation stops
#if stop time is less than 60sec, create per-rotation 9pt data that is slightly less than 10sec
#if stop time is more than 60sec, create per-rotation 9pt data that is slightly longer than 10sec

ninept_1.5 <- data2 %>% dplyr::filter(data2$Application.Time >=5 & data2$Application.Time <15)
ninept_2.5 <- data2 %>% dplyr::filter(data2$Application.Time >=15 & data2$Application.Time <25)
ninept_3.5 <- data2 %>% dplyr::filter(data2$Application.Time >=25 & data2$Application.Time <35)
ninept_4.5 <- data2 %>% dplyr::filter(data2$Application.Time >=35 & data2$Application.Time <45)
ninept_5.5 <- data2 %>% dplyr::filter(data2$Application.Time >=45 & data2$Application.Time <55)
ninept_7.5 <- data2 %>% dplyr::filter(data2$Application.Time >=(65+extra_time) & data2$Application.Time <(75+extra_time))
ninept_8.5 <- data2 %>% dplyr::filter(data2$Application.Time >(75+extra_time) & data2$Application.Time <(85+extra_time))
ninept_9.5 <- data2 %>% dplyr::filter(data2$Application.Time >=(85+extra_time) & data2$Application.Time <(95+extra_time))
ninept_10.5 <- data2 %>% dplyr::filter(data2$Application.Time >=(95+extra_time) & data2$Application.Time <(105+extra_time))
ninept_11.5 <- data2 %>% dplyr::filter(data2$Application.Time >=(105+extra_time) & data2$Application.Time <(115+extra_time))



write.csv(ninept_1.5, paste("turntable_9pt_", rotation_direction, "_1.5.csv", sep=""))
write.csv(ninept_2.5, paste("turntable_9pt_", rotation_direction, "_2.5.csv", sep=""))
write.csv(ninept_3.5, paste("turntable_9pt_", rotation_direction, "_3.5.csv", sep=""))
write.csv(ninept_4.5, paste("turntable_9pt_", rotation_direction, "_4.5.csv", sep=""))
write.csv(ninept_5.5, paste("turntable_9pt_", rotation_direction, "_5.5.csv", sep=""))
write.csv(ninept_7.5, paste("turntable_9pt_", rotation_direction, "_7.5.csv", sep=""))
write.csv(ninept_8.5, paste("turntable_9pt_", rotation_direction, "_8.5.csv", sep=""))
write.csv(ninept_9.5, paste("turntable_9pt_", rotation_direction, "_9.5.csv", sep=""))
write.csv(ninept_10.5, paste("turntable_9pt_", rotation_direction, "_10.5.csv", sep=""))
write.csv(ninept_11.5, paste("turntable_9pt_", rotation_direction, "_11.5.csv", sep=""))