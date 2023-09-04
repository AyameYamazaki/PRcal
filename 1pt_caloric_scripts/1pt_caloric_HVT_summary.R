
rm(list = ls())

library(ggplot2)
library(tidyverse)
library(readxl)
library(openxlsx)

id = "20220422HY"

right_control <- data.frame(0, 0)
colnames(right_control) <- c("time", "right_zero_origin")
left_control <- data.frame(0, 0)
colnames(left_control) <- c("time", "left_zero_origin")

right_tor_control_df <- read.xlsx("1ptcaloric_result_tor_control_right.xlsx", sheet="result")
right_tor_df <- read.xlsx("1ptcaloric_result_tor_right.xlsx", sheet="tor_data")
left_tor_control_df <- read.xlsx("1ptcaloric_result_tor_control_left.xlsx", sheet="result")
left_tor_df <- read.xlsx("1ptcaloric_result_tor_left.xlsx", sheet="tor_data")

right_Y_control_df <- read.xlsx("1ptcaloric_result_Y_control_right.xlsx", sheet="result")
right_Y_df <- read.xlsx("1ptcaloric_result_Y_right.xlsx", sheet="Y_data")
left_Y_control_df <- read.xlsx("1ptcaloric_result_Y_control_left.xlsx", sheet="result")
left_Y_df <- read.xlsx("1ptcaloric_result_Y_left.xlsx", sheet="Y_data")

right_Z_control_df <- read.xlsx("1ptcaloric_result_Z_control_right.xlsx", sheet="result")
right_Z_df <- read.xlsx("1ptcaloric_result_Z_right.xlsx", sheet="Z_data")
left_Z_control_df <- read.xlsx("1ptcaloric_result_Z_control_left.xlsx", sheet="result")
left_Z_df <- read.xlsx("1ptcaloric_result_Z_left.xlsx", sheet="Z_data")



#torsion
right_tor_control <- right_tor_control_df$average_tor
right_zero_origin <- right_tor_df$smoothedtor-right_tor_control
right_tor_zeroorigin <-  data.frame(right_tor_df$time, right_zero_origin)
colnames(right_tor_zeroorigin) <- c("time", "right_zero_origin")
right_tor_zeroorigin <- rbind(right_control, right_tor_zeroorigin)

right_tor_plot <- ggplot(data=right_tor_zeroorigin)+
  geom_line(aes(x=time, y=right_zero_origin), colour="red")
right_tor_plot

left_tor_control <- left_tor_control_df$average_tor
left_zero_origin <- left_tor_df$smoothedtor-left_tor_control
left_tor_zeroorigin <-  data.frame(left_tor_df$time, left_zero_origin)
colnames(left_tor_zeroorigin) <- c("time", "left_zero_origin")
left_tor_zeroorigin <- rbind(left_control, left_tor_zeroorigin)

left_tor_plot <- ggplot(data=left_tor_zeroorigin)+
  geom_line(aes(x=time, y=left_zero_origin), colour="blue")
left_tor_plot

torsion_plot <- ggplot()+
  geom_line(data=right_tor_zeroorigin, aes(x=time, y=right_zero_origin), colour="red")+
  geom_line(data=left_tor_zeroorigin, aes(x=time, y=left_zero_origin), colour="blue")+
  scale_x_continuous(name="time", breaks=seq(0, 300, 25)) +
  scale_y_continuous(name="Torsion(deg)",limits = c(-10, 10))+
  labs(title = "torsion", x = "time", y = "torsion(deg)")
torsion_plot


#Y
right_Y_control <- right_Y_control_df$average_Y
right_zero_origin <- right_Y_df$smoothedY-right_Y_control
right_Y_zeroorigin <-  data.frame(right_Y_df$time, right_zero_origin)
colnames(right_Y_zeroorigin) <- c("time", "right_zero_origin")
right_Y_zeroorigin <- rbind(right_control, right_Y_zeroorigin)

right_Y_plot <- ggplot(data=right_Y_zeroorigin)+
  geom_line(aes(x=time, y=right_zero_origin), colour="red")
right_Y_plot

left_Y_control <- left_Y_control_df$average_Y
left_zero_origin <- left_Y_df$smoothedY-left_Y_control
left_Y_zeroorigin <-  data.frame(left_Y_df$time, left_zero_origin)
colnames(left_Y_zeroorigin) <- c("time", "left_zero_origin")
left_Y_zeroorigin <- rbind(left_control, left_Y_zeroorigin)

left_Y_plot <- ggplot(data=left_Y_zeroorigin)+
  geom_line(aes(x=time, y=left_zero_origin), colour="blue")
left_Y_plot

Y_plot <- ggplot()+
  geom_line(data=right_Y_zeroorigin, aes(x=time, y=right_zero_origin), colour="red")+
  geom_line(data=left_Y_zeroorigin, aes(x=time, y=left_zero_origin), colour="blue")+
  scale_x_continuous(name="time", breaks=seq(0, 300, 25)) +
  scale_y_continuous(name="Y(deg)",limits = c(-10, 10))+
  labs(title = "Y", x = "time", y = "Y(deg)")

Y_plot



#Z
right_Z_control <- right_Z_control_df$average_Z
right_zero_origin <- right_Z_df$smoothedZ-right_Z_control
right_Z_zeroorigin <-  data.frame(right_Z_df$time, right_zero_origin)
colnames(right_Z_zeroorigin) <- c("time", "right_zero_origin")
right_Z_zeroorigin <- rbind(right_control, right_Z_zeroorigin)

right_Z_plot <- ggplot(data=right_Z_zeroorigin)+
  geom_line(aes(x=time, y=right_zero_origin), colour="red")
right_Z_plot

left_Z_control <- left_Z_control_df$average_Z
left_zero_origin <- left_Z_df$smoothedZ-left_Z_control
left_Z_zeroorigin <-  data.frame(left_Z_df$time, left_zero_origin)
colnames(left_Z_zeroorigin) <- c("time", "left_zero_origin")
left_Z_zeroorigin <- rbind(left_control, left_Z_zeroorigin)

left_Z_plot <- ggplot(data=left_Z_zeroorigin)+
  geom_line(aes(x=time, y=left_zero_origin), colour="blue")
left_Z_plot

Z_plot <- ggplot()+
  geom_line(data=right_Z_zeroorigin, aes(x=time, y=right_zero_origin), colour="red")+
  geom_line(data=left_Z_zeroorigin, aes(x=time, y=left_zero_origin), colour="blue")+
  scale_x_continuous(name="time", breaks=seq(0, 300, 25)) +
  scale_y_continuous(name="Z(deg)",limits = c(-10, 10))+
  labs(title = "Z", x = "time", y = "Z(deg)")

Z_plot

pdf("1ptcaloric_torsion_plot.pdf")
print(torsion_plot)
dev.off()

pdf("1ptcaloric_Y_plot.pdf")
print(Y_plot)
dev.off()

pdf("1ptcaloric_Z_plot.pdf")
print(Z_plot)
dev.off()

output_list <- c("left_torsion"=left_tor_zeroorigin, "right_torsion"=right_tor_zeroorigin, "left_Y"=left_Y_zeroorigin, "right_Y"=right_Y_zeroorigin, "left_Z"=left_Z_zeroorigin, "right_Z"=right_Z_zeroorigin)

write.xlsx(output_list, paste(id, "_1pt_caloric_result.xlsx", sep=""), overwrite=TRUE)