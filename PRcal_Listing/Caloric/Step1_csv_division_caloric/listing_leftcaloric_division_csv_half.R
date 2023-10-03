##set working directory **************************************************************************

rm(list = ls())

library(ggplot2)
library(tidyverse)
library(readxl)

data2 <- read.csv(file=file.choose()) 

ear = "left"

time <- data2[,"Application.Time"]
lt_torsion <- data2[,"Eye.Torsion..degrees..left"]
rt_torsion <- data2[,"Eye.Torsion..degrees..right"]
df <- data.frame(time, lt_torsion, rt_torsion)

torsion_plot_left  <- ggplot(data=df)+
  geom_line(aes(x=time, y=lt_torsion), colour="blue")+
  ylim(-20,20)
torsion_plot_left

torsion_plot_right <- ggplot(data=df)+
  geom_line(aes(x=time, y=rt_torsion), colour="red")+
  ylim(-20,20)
torsion_plot_right

caloric_ninept_1.5 <- data2 %>% dplyr::filter(data2$Application.Time >=5 & data2$Application.Time <15)
caloric_ninept_2.5 <- data2 %>% dplyr::filter(data2$Application.Time >=15 & data2$Application.Time <25)
caloric_ninept_3.5 <- data2 %>% dplyr::filter(data2$Application.Time >=25 & data2$Application.Time <35)
caloric_ninept_4.5 <- data2 %>% dplyr::filter(data2$Application.Time >=35 & data2$Application.Time <45)
caloric_ninept_5.5 <- data2 %>% dplyr::filter(data2$Application.Time >=45 & data2$Application.Time <55)
caloric_ninept_6.5 <- data2 %>% dplyr::filter(data2$Application.Time >55 & data2$Application.Time <65)
caloric_ninept_7.5 <- data2 %>% dplyr::filter(data2$Application.Time >=65 & data2$Application.Time <75)
caloric_ninept_8.5 <- data2 %>% dplyr::filter(data2$Application.Time >=75 & data2$Application.Time <85)
caloric_ninept_9.5 <- data2 %>% dplyr::filter(data2$Application.Time >=85 & data2$Application.Time <95)
caloric_ninept_10.5 <- data2 %>% dplyr::filter(data2$Application.Time >=95 & data2$Application.Time <105)
caloric_ninept_11.5 <- data2 %>% dplyr::filter(data2$Application.Time >=105 & data2$Application.Time <115)
caloric_ninept_12.5 <- data2 %>% dplyr::filter(data2$Application.Time >=115 & data2$Application.Time <125)
caloric_ninept_13.5 <- data2 %>% dplyr::filter(data2$Application.Time >=125 & data2$Application.Time <135)
caloric_ninept_14.5 <- data2 %>% dplyr::filter(data2$Application.Time >=135 & data2$Application.Time <145)
caloric_ninept_15.5 <- data2 %>% dplyr::filter(data2$Application.Time >=145 & data2$Application.Time <155)
caloric_ninept_16.5 <- data2 %>% dplyr::filter(data2$Application.Time >=155 & data2$Application.Time <165)
caloric_ninept_17.5 <- data2 %>% dplyr::filter(data2$Application.Time >=165 & data2$Application.Time <175)
caloric_ninept_18.5 <- data2 %>% dplyr::filter(data2$Application.Time >=175 & data2$Application.Time <185)
caloric_ninept_19.5 <- data2 %>% dplyr::filter(data2$Application.Time >=185 & data2$Application.Time <195)
caloric_ninept_20.5 <- data2 %>% dplyr::filter(data2$Application.Time >=195 & data2$Application.Time <205)
caloric_ninept_21.5 <- data2 %>% dplyr::filter(data2$Application.Time >=205 & data2$Application.Time <215)
caloric_ninept_22.5 <- data2 %>% dplyr::filter(data2$Application.Time >=215 & data2$Application.Time <225)
caloric_ninept_23.5 <- data2 %>% dplyr::filter(data2$Application.Time >=225 & data2$Application.Time <235)
caloric_ninept_24.5 <- data2 %>% dplyr::filter(data2$Application.Time >=235 & data2$Application.Time <245)
caloric_ninept_25.5 <- data2 %>% dplyr::filter(data2$Application.Time >=245 & data2$Application.Time <255)
caloric_ninept_26.5 <- data2 %>% dplyr::filter(data2$Application.Time >=255 & data2$Application.Time <265)
caloric_ninept_27.5 <- data2 %>% dplyr::filter(data2$Application.Time >=265 & data2$Application.Time <275)
caloric_ninept_28.5 <- data2 %>% dplyr::filter(data2$Application.Time >=275 & data2$Application.Time <285)
caloric_ninept_29.5 <- data2 %>% dplyr::filter(data2$Application.Time >=285 & data2$Application.Time <295)

write.csv(caloric_ninept_1.5, paste("caloric_9pt_", ear, "_1.5.csv", sep=""))
write.csv(caloric_ninept_2.5, paste("caloric_9pt_", ear, "_2.5.csv", sep=""))
write.csv(caloric_ninept_3.5, paste("caloric_9pt_", ear, "_3.5.csv", sep=""))
write.csv(caloric_ninept_4.5, paste("caloric_9pt_", ear, "_4.5.csv", sep=""))
write.csv(caloric_ninept_5.5, paste("caloric_9pt_", ear, "_5.5.csv", sep=""))
write.csv(caloric_ninept_6.5, paste("caloric_9pt_", ear, "_6.5.csv", sep=""))
write.csv(caloric_ninept_7.5, paste("caloric_9pt_", ear, "_7.5.csv", sep=""))
write.csv(caloric_ninept_8.5, paste("caloric_9pt_", ear, "_8.5.csv", sep=""))
write.csv(caloric_ninept_9.5, paste("caloric_9pt_", ear, "_9.5.csv", sep=""))
write.csv(caloric_ninept_10.5, paste("caloric_9pt_", ear, "_10.5.csv", sep=""))
write.csv(caloric_ninept_11.5, paste("caloric_9pt_", ear, "_11.5.csv", sep=""))
write.csv(caloric_ninept_12.5, paste("caloric_9pt_", ear, "_12.5.csv", sep=""))
write.csv(caloric_ninept_13.5, paste("caloric_9pt_", ear, "_13.5.csv", sep=""))
write.csv(caloric_ninept_14.5, paste("caloric_9pt_", ear, "_14.5.csv", sep=""))
write.csv(caloric_ninept_15.5, paste("caloric_9pt_", ear, "_15.5.csv", sep=""))
write.csv(caloric_ninept_16.5, paste("caloric_9pt_", ear, "_16.5.csv", sep=""))
write.csv(caloric_ninept_17.5, paste("caloric_9pt_", ear, "_17.5.csv", sep=""))
write.csv(caloric_ninept_18.5, paste("caloric_9pt_", ear, "_18.5.csv", sep=""))
write.csv(caloric_ninept_19.5, paste("caloric_9pt_", ear, "_19.5.csv", sep=""))
write.csv(caloric_ninept_20.5, paste("caloric_9pt_", ear, "_20.5.csv", sep=""))
write.csv(caloric_ninept_21.5, paste("caloric_9pt_", ear, "_21.5.csv", sep=""))
write.csv(caloric_ninept_22.5, paste("caloric_9pt_", ear, "_22.5.csv", sep=""))
write.csv(caloric_ninept_23.5, paste("caloric_9pt_", ear, "_23.5.csv", sep=""))
write.csv(caloric_ninept_24.5, paste("caloric_9pt_", ear, "_24.5.csv", sep=""))
write.csv(caloric_ninept_25.5, paste("caloric_9pt_", ear, "_25.5.csv", sep=""))
write.csv(caloric_ninept_26.5, paste("caloric_9pt_", ear, "_26.5.csv", sep=""))
write.csv(caloric_ninept_27.5, paste("caloric_9pt_", ear, "_27.5.csv", sep=""))
write.csv(caloric_ninept_28.5, paste("caloric_9pt_", ear, "_28.5.csv", sep=""))
write.csv(caloric_ninept_29.5, paste("caloric_9pt_", ear, "_29.5.csv", sep=""))

