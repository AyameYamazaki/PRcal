
rm(list = ls())

library(ggplot2)
library(tidyverse)
library(readxl)
library(openxlsx)

id = "20230524AODR"

right_control <- data.frame(0, 0)
colnames(right_control) <- c("time", "right_zero_origin")
left_control <- data.frame(0, 0)
colnames(left_control) <- c("time", "left_zero_origin")


#torsion
right_tor_control_df <- read.xlsx("1ptturntable_result_tor_control_right.xlsx", sheet="result")
left_tor_control_df <- read.xlsx("1ptturntable_result_tor_control_left.xlsx", sheet="result")

right_tor_pre_df <- read.xlsx("1ptturntable_tor_result_right_pre.xlsx", sheet="tor_data")
left_tor_pre_df <- read.xlsx("1ptturntable_tor_result_left_pre.xlsx", sheet="tor_data")

right_tor_post_df <- read.xlsx("1ptturntable_tor_result_right_post.xlsx", sheet="tor_data")
left_tor_post_df <- read.xlsx("1ptturntable_tor_result_left_post.xlsx", sheet="tor_data")

right_tor_df <- rbind(right_tor_pre_df, right_tor_post_df)
left_tor_df <- rbind(left_tor_pre_df, left_tor_post_df)



#Y
right_Y_control_df <- read.xlsx("1ptturntable_result_Y_control_right.xlsx", sheet="result")
left_Y_control_df <- read.xlsx("1ptturntable_result_Y_control_left.xlsx", sheet="result")

right_Y_pre_df <- read.xlsx("1ptturntable_Y_result_right_pre.xlsx", sheet="Y_data")
left_Y_pre_df <- read.xlsx("1ptturntable_Y_result_left_pre.xlsx", sheet="Y_data")

right_Y_post_df <- read.xlsx("1ptturntable_Y_result_right_post.xlsx", sheet="Y_data")
left_Y_post_df <- read.xlsx("1ptturntable_Y_result_left_post.xlsx", sheet="Y_data")

right_Y_df <- rbind(right_Y_pre_df, right_Y_post_df)
left_Y_df <- rbind(left_Y_pre_df, left_Y_post_df)


#Z
right_Z_control_df <- read.xlsx("1ptturntable_result_Z_control_right.xlsx", sheet="result")
left_Z_control_df <- read.xlsx("1ptturntable_result_Z_control_left.xlsx", sheet="result")

right_Z_pre_df <- read.xlsx("1ptturntable_Z_result_right_pre.xlsx", sheet="Z_data")
left_Z_pre_df <- read.xlsx("1ptturntable_Z_result_left_pre.xlsx", sheet="Z_data")

right_Z_post_df <- read.xlsx("1ptturntable_Z_result_right_post.xlsx", sheet="Z_data")
left_Z_post_df <- read.xlsx("1ptturntable_Z_result_left_post.xlsx", sheet="Z_data")

right_Z_df <- rbind(right_Z_pre_df, right_Z_post_df)
left_Z_df <- rbind(left_Z_pre_df, left_Z_post_df)


rt_stop_time_1 = min(right_tor_post_df$time)-0.25
rt_stop_time_2 = min(right_tor_post_df$time)+0.25


lt_stop_time_1 = min(left_tor_post_df$time)-0.25
lt_stop_time_2 = min(left_tor_post_df$time)+0.25

#torsion
right_tor_control <- right_tor_control_df$average_tor
right_zero_origin <- right_tor_df$smoothedtor-right_tor_control
right_tor_zeroorigin <-  data.frame(right_tor_df$time, right_zero_origin)
colnames(right_tor_zeroorigin) <- c("time", "right_zero_origin")
right_tor_zeroorigin <- rbind(right_control, right_tor_zeroorigin)

right_tor_plot <- ggplot()+
  geom_line(data=right_tor_zeroorigin, aes(x=time, y=right_zero_origin), colour="red")+
  geom_vline(aes(xintercept=rt_stop_time_1), color="darkred", linetype="dashed") +
  geom_vline(aes(xintercept=rt_stop_time_2), color="darkred", linetype="dashed") +
  labs(title = "right_torsion", x = "time", y = "torsion(deg)")
right_tor_plot

left_tor_control <- left_tor_control_df$average_tor
left_zero_origin <- left_tor_df$smoothedtor-left_tor_control
left_tor_zeroorigin <-  data.frame(left_tor_df$time, left_zero_origin)
colnames(left_tor_zeroorigin) <- c("time", "left_zero_origin")
left_tor_zeroorigin <- rbind(left_control, left_tor_zeroorigin)

left_tor_plot <- ggplot()+
  geom_line(data=left_tor_zeroorigin, aes(x=time, y=left_zero_origin), colour="blue")+
  geom_vline(aes(xintercept=lt_stop_time_1), color="darkred", linetype="dashed") +
  geom_vline(aes(xintercept=lt_stop_time_2), color="darkred", linetype="dashed")+
  labs(title = "left_torsion", x = "time", y = "torsion(deg)")
left_tor_plot



#Y
right_Y_control <- right_Y_control_df$average_Y
right_zero_origin <- right_Y_df$smoothedY-right_Y_control
right_Y_zeroorigin <-  data.frame(right_Y_df$time, right_zero_origin)
colnames(right_Y_zeroorigin) <- c("time", "right_zero_origin")
right_Y_zeroorigin <- rbind(right_control, right_Y_zeroorigin)

right_Y_plot <- ggplot()+
  geom_line(data=right_Y_zeroorigin, aes(x=time, y=right_zero_origin), colour="red")+
  geom_vline(aes(xintercept=rt_stop_time_1), color="darkred", linetype="dashed") +
  geom_vline(aes(xintercept=rt_stop_time_2), color="darkred", linetype="dashed")+
  labs(title = "right_Y", x = "time", y = "Y(deg)")
right_Y_plot

left_Y_control <- left_Y_control_df$average_Y
left_zero_origin <- left_Y_df$smoothedY-left_Y_control
left_Y_zeroorigin <-  data.frame(left_Y_df$time, left_zero_origin)
colnames(left_Y_zeroorigin) <- c("time", "left_zero_origin")
left_Y_zeroorigin <- rbind(left_control, left_Y_zeroorigin)

left_Y_plot <- ggplot()+
  geom_line(data=left_Y_zeroorigin, aes(x=time, y=left_zero_origin), colour="blue")+
  geom_vline(aes(xintercept=lt_stop_time_1), color="darkred", linetype="dashed") +
  geom_vline(aes(xintercept=lt_stop_time_2), color="darkred", linetype="dashed")+
  labs(title = "left_Y", x = "time", y = "Y(deg)")
left_Y_plot

#Z
right_Z_control <- right_Z_control_df$average_Z
right_zero_origin <- right_Z_df$smoothedZ-right_Z_control
right_Z_zeroorigin <-  data.frame(right_Z_df$time, right_zero_origin)
colnames(right_Z_zeroorigin) <- c("time", "right_zero_origin")
right_Z_zeroorigin <- rbind(right_control, right_Z_zeroorigin)

right_Z_plot <- ggplot()+
  geom_line(data=right_Z_zeroorigin, aes(x=time, y=right_zero_origin), colour="red")+
  geom_vline(aes(xintercept=rt_stop_time_1), color="darkred", linetype="dashed") +
  geom_vline(aes(xintercept=rt_stop_time_2), color="darkred", linetype="dashed")+
  labs(title = "right_Z", x = "time", y = "Z(deg)")
right_Z_plot

left_Z_control <- left_Z_control_df$average_Z
left_zero_origin <- left_Z_df$smoothedZ-left_Z_control
left_Z_zeroorigin <-  data.frame(left_Z_df$time, left_zero_origin)
colnames(left_Z_zeroorigin) <- c("time", "left_zero_origin")
left_Z_zeroorigin <- rbind(left_control, left_Z_zeroorigin)

left_Z_plot <- ggplot()+
  geom_line(data=left_Z_zeroorigin, aes(x=time, y=left_zero_origin), colour="blue")+
  geom_vline(aes(xintercept=lt_stop_time_1), color="darkred", linetype="dashed") +
  geom_vline(aes(xintercept=lt_stop_time_2), color="darkred", linetype="dashed")+
  labs(title = "left_Z", x = "time", y = "Z(deg)")
left_Z_plot


pdf("1ptturntable_torsion_plot.pdf")
print(left_tor_plot)
print(right_tor_plot)
dev.off()

pdf("1ptturntable_Y_plot.pdf")
print(left_Y_plot)
print(right_Y_plot)
dev.off()

pdf("1ptturntable_Z_plot.pdf")
print(left_Z_plot)
print(right_Z_plot)
dev.off()

stoptime <- data.frame(lt_stop_time_1, lt_stop_time_2, rt_stop_time_1, rt_stop_time_2)

output_list <- list("left_torsion"=left_tor_zeroorigin, "right_torsion"=right_tor_zeroorigin, "left_Y"=left_Y_zeroorigin, "right_Y"=right_Y_zeroorigin, "left_Z"=left_Z_zeroorigin, "right_Z"=right_Z_zeroorigin, "stoptime"=stoptime)

write.xlsx(output_list, paste(id, "_1pt_turntable_result.xlsx", sep=""), overwrite=TRUE)