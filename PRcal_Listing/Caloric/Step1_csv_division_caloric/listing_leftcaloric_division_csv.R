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

caloric_ninept_1 <- data2 %>% dplyr::filter(data2$Application.Time <10)
caloric_ninept_2 <- data2 %>% dplyr::filter(data2$Application.Time >=10 & data2$Application.Time <20)
caloric_ninept_3 <- data2 %>% dplyr::filter(data2$Application.Time >=20 & data2$Application.Time <30)
caloric_ninept_4 <- data2 %>% dplyr::filter(data2$Application.Time >=30 & data2$Application.Time <40)
caloric_ninept_5 <- data2 %>% dplyr::filter(data2$Application.Time >=40 & data2$Application.Time <50)
caloric_ninept_6 <- data2 %>% dplyr::filter(data2$Application.Time >=50 & data2$Application.Time <60)
caloric_ninept_7 <- data2 %>% dplyr::filter(data2$Application.Time >=60 & data2$Application.Time <70)
caloric_ninept_8 <- data2 %>% dplyr::filter(data2$Application.Time >=70 & data2$Application.Time <80)
caloric_ninept_9 <- data2 %>% dplyr::filter(data2$Application.Time >=80 & data2$Application.Time <90)
caloric_ninept_10 <- data2 %>% dplyr::filter(data2$Application.Time >=90 & data2$Application.Time <100)
caloric_ninept_11 <- data2 %>% dplyr::filter(data2$Application.Time >=100 & data2$Application.Time <110)
caloric_ninept_12 <- data2 %>% dplyr::filter(data2$Application.Time >=110 & data2$Application.Time <120)
caloric_ninept_13 <- data2 %>% dplyr::filter(data2$Application.Time >=120 & data2$Application.Time <130)
caloric_ninept_14 <- data2 %>% dplyr::filter(data2$Application.Time >=130 & data2$Application.Time <140)
caloric_ninept_15 <- data2 %>% dplyr::filter(data2$Application.Time >=140 & data2$Application.Time <150)
caloric_ninept_16 <- data2 %>% dplyr::filter(data2$Application.Time >=150 & data2$Application.Time <160)
caloric_ninept_17 <- data2 %>% dplyr::filter(data2$Application.Time >=160 & data2$Application.Time <170)
caloric_ninept_18 <- data2 %>% dplyr::filter(data2$Application.Time >=170 & data2$Application.Time <180)
caloric_ninept_19 <- data2 %>% dplyr::filter(data2$Application.Time >=180 & data2$Application.Time <190)
caloric_ninept_20 <- data2 %>% dplyr::filter(data2$Application.Time >=190 & data2$Application.Time <200)
caloric_ninept_21 <- data2 %>% dplyr::filter(data2$Application.Time >=200 & data2$Application.Time <210)
caloric_ninept_22 <- data2 %>% dplyr::filter(data2$Application.Time >=210 & data2$Application.Time <220)
caloric_ninept_23 <- data2 %>% dplyr::filter(data2$Application.Time >=220 & data2$Application.Time <230)
caloric_ninept_24 <- data2 %>% dplyr::filter(data2$Application.Time >=230 & data2$Application.Time <240)
caloric_ninept_25 <- data2 %>% dplyr::filter(data2$Application.Time >=240 & data2$Application.Time <250)
caloric_ninept_26 <- data2 %>% dplyr::filter(data2$Application.Time >=250 & data2$Application.Time <260)
caloric_ninept_27 <- data2 %>% dplyr::filter(data2$Application.Time >=260 & data2$Application.Time <270)
caloric_ninept_28 <- data2 %>% dplyr::filter(data2$Application.Time >=270 & data2$Application.Time <280)
caloric_ninept_29 <- data2 %>% dplyr::filter(data2$Application.Time >=280 & data2$Application.Time <290)
caloric_ninept_30 <- data2 %>% dplyr::filter(data2$Application.Time >=290 & data2$Application.Time <300)


write.csv(caloric_ninept_1, paste("caloric_9pt_", ear, "_1.csv", sep=""))
write.csv(caloric_ninept_2, paste("caloric_9pt_", ear, "_2.csv", sep=""))
write.csv(caloric_ninept_3, paste("caloric_9pt_", ear, "_3.csv", sep=""))
write.csv(caloric_ninept_4, paste("caloric_9pt_", ear, "_4.csv", sep=""))
write.csv(caloric_ninept_5, paste("caloric_9pt_", ear, "_5.csv", sep=""))
write.csv(caloric_ninept_6, paste("caloric_9pt_", ear, "_6.csv", sep=""))
write.csv(caloric_ninept_7, paste("caloric_9pt_", ear, "_7.csv", sep=""))
write.csv(caloric_ninept_8, paste("caloric_9pt_", ear, "_8.csv", sep=""))
write.csv(caloric_ninept_9, paste("caloric_9pt_", ear, "_9.csv", sep=""))
write.csv(caloric_ninept_10, paste("caloric_9pt_", ear, "_10.csv", sep=""))
write.csv(caloric_ninept_11, paste("caloric_9pt_", ear, "_11.csv", sep=""))
write.csv(caloric_ninept_12, paste("caloric_9pt_", ear, "_12.csv", sep=""))
write.csv(caloric_ninept_13, paste("caloric_9pt_", ear, "_13.csv", sep=""))
write.csv(caloric_ninept_14, paste("caloric_9pt_", ear, "_14.csv", sep=""))
write.csv(caloric_ninept_15, paste("caloric_9pt_", ear, "_15.csv", sep=""))
write.csv(caloric_ninept_16, paste("caloric_9pt_", ear, "_16.csv", sep=""))
write.csv(caloric_ninept_17, paste("caloric_9pt_", ear, "_17.csv", sep=""))
write.csv(caloric_ninept_18, paste("caloric_9pt_", ear, "_18.csv", sep=""))
write.csv(caloric_ninept_19, paste("caloric_9pt_", ear, "_19.csv", sep=""))
write.csv(caloric_ninept_20, paste("caloric_9pt_", ear, "_20.csv", sep=""))
write.csv(caloric_ninept_21, paste("caloric_9pt_", ear, "_21.csv", sep=""))
write.csv(caloric_ninept_22, paste("caloric_9pt_", ear, "_22.csv", sep=""))
write.csv(caloric_ninept_23, paste("caloric_9pt_", ear, "_23.csv", sep=""))
write.csv(caloric_ninept_24, paste("caloric_9pt_", ear, "_24.csv", sep=""))
write.csv(caloric_ninept_25, paste("caloric_9pt_", ear, "_25.csv", sep=""))
write.csv(caloric_ninept_26, paste("caloric_9pt_", ear, "_26.csv", sep=""))
write.csv(caloric_ninept_27, paste("caloric_9pt_", ear, "_27.csv", sep=""))
write.csv(caloric_ninept_28, paste("caloric_9pt_", ear, "_28.csv", sep=""))
write.csv(caloric_ninept_29, paste("caloric_9pt_", ear, "_29.csv", sep=""))
write.csv(caloric_ninept_30, paste("caloric_9pt_", ear, "_30.csv", sep=""))

