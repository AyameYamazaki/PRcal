



##set working directory **************************************************************************

rm(list = ls())

library(ggplot2)
library(tidyverse)
library(readxl)
library(openxlsx)
library(ggthemes)

#date and patient initials change accordingly!!!!!!!!!!!!!!!!!!!!!!!! *****************************************************!!!!!!!!!!!!!!!!
date = "20230511"
initials = "AY"
angle = "30"
head = "HD"

id = paste(date, initials, "_", angle, head, sep="")


#right side
rightLP_control <- read.xlsx("result_turntable_9pt_right_control.xlsx", sheet="Listing's Plane")
rightLP_1 <- read.xlsx("result_turntable_9pt_right_1.xlsx", sheet="Listing's Plane")
rightLP_1.5 <- read.xlsx("result_turntable_9pt_right_1.5.xlsx", sheet="Listing's Plane")
rightLP_2 <- read.xlsx("result_turntable_9pt_right_2.xlsx", sheet="Listing's Plane")
rightLP_2.5 <- read.xlsx("result_turntable_9pt_right_2.5.xlsx", sheet="Listing's Plane")
rightLP_3 <- read.xlsx("result_turntable_9pt_right_3.xlsx", sheet="Listing's Plane")
rightLP_3.5 <- read.xlsx("result_turntable_9pt_right_3.5.xlsx", sheet="Listing's Plane")
rightLP_4 <- read.xlsx("result_turntable_9pt_right_4.xlsx", sheet="Listing's Plane")
rightLP_4.5 <- read.xlsx("result_turntable_9pt_right_4.5.xlsx", sheet="Listing's Plane")
rightLP_5 <- read.xlsx("result_turntable_9pt_right_5.xlsx", sheet="Listing's Plane")
rightLP_5.5 <- read.xlsx("result_turntable_9pt_right_5.5.xlsx", sheet="Listing's Plane")
rightLP_6 <- read.xlsx("result_turntable_9pt_right_6.xlsx", sheet="Listing's Plane")
rightLP_7 <- read.xlsx("result_turntable_9pt_right_7.xlsx", sheet="Listing's Plane")
rightLP_7.5 <- read.xlsx("result_turntable_9pt_right_7.5.xlsx", sheet="Listing's Plane")
rightLP_8 <- read.xlsx("result_turntable_9pt_right_8.xlsx", sheet="Listing's Plane")
rightLP_8.5 <- read.xlsx("result_turntable_9pt_right_8.5.xlsx", sheet="Listing's Plane")
rightLP_9 <- read.xlsx("result_turntable_9pt_right_9.xlsx", sheet="Listing's Plane")
rightLP_9.5 <- read.xlsx("result_turntable_9pt_right_9.5.xlsx", sheet="Listing's Plane")
rightLP_10 <- read.xlsx("result_turntable_9pt_right_10.xlsx", sheet="Listing's Plane")
rightLP_10.5 <- read.xlsx("result_turntable_9pt_right_10.5.xlsx", sheet="Listing's Plane")
rightLP_11 <- read.xlsx("result_turntable_9pt_right_11.xlsx", sheet="Listing's Plane")
rightLP_11.5 <- read.xlsx("result_turntable_9pt_right_11.5.xlsx", sheet="Listing's Plane")
rightLP_12 <- read.xlsx("result_turntable_9pt_right_12.xlsx", sheet="Listing's Plane")

#average x
rightLP_averagex_control <- rightLP_control$LP_average_x
rightLP_averagex_1 <- rightLP_1$LP_average_x
rightLP_averagex_1.5 <- rightLP_1.5$LP_average_x
rightLP_averagex_2 <- rightLP_2$LP_average_x
rightLP_averagex_2.5 <- rightLP_2.5$LP_average_x
rightLP_averagex_3 <- rightLP_3$LP_average_x
rightLP_averagex_3.5 <- rightLP_3.5$LP_average_x
rightLP_averagex_4 <- rightLP_4$LP_average_x
rightLP_averagex_4.5 <- rightLP_4.5$LP_average_x
rightLP_averagex_5 <- rightLP_5$LP_average_x
rightLP_averagex_5.5 <- rightLP_5.5$LP_average_x
rightLP_averagex_6 <- rightLP_6$LP_average_x
rightLP_averagex_7 <- rightLP_7$LP_average_x
rightLP_averagex_7.5 <- rightLP_7.5$LP_average_x
rightLP_averagex_8 <- rightLP_8$LP_average_x
rightLP_averagex_8.5 <- rightLP_8.5$LP_average_x
rightLP_averagex_9 <- rightLP_9$LP_average_x
rightLP_averagex_9.5 <- rightLP_9.5$LP_average_x
rightLP_averagex_10 <- rightLP_10$LP_average_x
rightLP_averagex_10.5 <- rightLP_10.5$LP_average_x
rightLP_averagex_11 <- rightLP_11$LP_average_x
rightLP_averagex_11.5 <- rightLP_11.5$LP_average_x
rightLP_averagex_12 <- rightLP_12$LP_average_x

time_group <- c(0, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12)
rightLP_averagex <- c(rightLP_averagex_control, rightLP_averagex_1, rightLP_averagex_1.5, rightLP_averagex_2, rightLP_averagex_2.5, rightLP_averagex_3, rightLP_averagex_3.5,rightLP_averagex_4, rightLP_averagex_4.5, rightLP_averagex_5, rightLP_averagex_5.5, rightLP_averagex_6, rightLP_averagex_7, rightLP_averagex_7.5, rightLP_averagex_8,rightLP_averagex_8.5,  rightLP_averagex_9, rightLP_averagex_9.5, rightLP_averagex_10,rightLP_averagex_10.5,  rightLP_averagex_11, rightLP_averagex_11.5, rightLP_averagex_12)

#standard deviation
rightLP_xsd_control <- rightLP_control$LP_SD_x
rightLP_xsd_1 <- rightLP_1$LP_SD_x
rightLP_xsd_1.5 <- rightLP_1.5$LP_SD_x
rightLP_xsd_2 <- rightLP_2$LP_SD_x
rightLP_xsd_2.5 <- rightLP_2.5$LP_SD_x
rightLP_xsd_3 <- rightLP_3$LP_SD_x
rightLP_xsd_3.5 <- rightLP_3.5$LP_SD_x
rightLP_xsd_4 <- rightLP_4$LP_SD_x
rightLP_xsd_4.5 <- rightLP_4.5$LP_SD_x
rightLP_xsd_5 <- rightLP_5$LP_SD_x
rightLP_xsd_5.5 <- rightLP_5.5$LP_SD_x
rightLP_xsd_6 <- rightLP_6$LP_SD_x
rightLP_xsd_7 <- rightLP_7$LP_SD_x
rightLP_xsd_7.5 <- rightLP_7.5$LP_SD_x
rightLP_xsd_8 <- rightLP_8$LP_SD_x
rightLP_xsd_8.5 <- rightLP_8.5$LP_SD_x
rightLP_xsd_9 <- rightLP_9$LP_SD_x
rightLP_xsd_9.5 <- rightLP_9.5$LP_SD_x
rightLP_xsd_10 <- rightLP_10$LP_SD_x
rightLP_xsd_10.5 <- rightLP_10.5$LP_SD_x
rightLP_xsd_11 <- rightLP_11$LP_SD_x
rightLP_xsd_11.5 <- rightLP_11.5$LP_SD_x
rightLP_xsd_12 <- rightLP_12$LP_SD_x

rightLP_xsd <- c(rightLP_xsd_control, rightLP_xsd_1, rightLP_xsd_1.5, rightLP_xsd_2,rightLP_xsd_2.5, rightLP_xsd_3, rightLP_xsd_3.5, rightLP_xsd_4, rightLP_xsd_4.5, rightLP_xsd_5, rightLP_xsd_5.5, rightLP_xsd_6, rightLP_xsd_7, rightLP_xsd_7.5, rightLP_xsd_8, rightLP_xsd_8.5, rightLP_xsd_9, rightLP_xsd_9.5, rightLP_xsd_10, rightLP_xsd_10.5, rightLP_xsd_11, rightLP_xsd_11.5, rightLP_xsd_12)

rightLP_averagex_df <- data.frame(time_group, rightLP_averagex, rightLP_xsd)
#adjust by subtracting control value to all data
rightLP_averagex_df$right_adj_avex <- rightLP_averagex_df$rightLP_averagex - rightLP_averagex_control

plot_averagex_right <- ggplot(rightLP_averagex_df, aes(x=time_group, y=rightLP_averagex)) +
  geom_point(colour="red", size=3) +
  geom_errorbar(aes(ymin=rightLP_averagex-rightLP_xsd, ymax=rightLP_averagex+rightLP_xsd), colour="pink", width=0.2) +
  scale_x_continuous(name="Time Group", breaks=seq(0, 12,1)) +
  scale_y_continuous(name="Listing's Plane Average X", breaks=seq(-14, 14, by=1)) +
  geom_hline(yintercept=0, color="grey") +
  geom_vline(xintercept=6.5, color="darkred", linetype="dashed") +
  theme_classic() +
  ggtitle("rightward rotation")
plot_averagex_right

plot_averagex_controladj_right <- ggplot(rightLP_averagex_df, aes(x=time_group, y=right_adj_avex)) +
  geom_point(colour="red", size=3) +
  geom_errorbar(aes(ymin=right_adj_avex-rightLP_xsd, ymax=right_adj_avex+rightLP_xsd), colour="pink", width=0.2) +
  scale_x_continuous(name="Time Group", breaks=seq(0, 12,1)) +
  scale_y_continuous(name="Listing's Plane Average X", breaks=seq(-14, 14, by=1)) +
  geom_hline(yintercept=0, color="grey") +
  geom_vline(xintercept=6.5, color="darkred", linetype="dashed") +
  theme_classic()+
  ggtitle("rightward rotation")
plot_averagex_controladj_right





#left side
leftLP_control <- read.xlsx("result_turntable_9pt_left_control.xlsx", sheet="Listing's Plane")
leftLP_1 <- read.xlsx("result_turntable_9pt_left_1.xlsx", sheet="Listing's Plane")
leftLP_1.5 <- read.xlsx("result_turntable_9pt_left_1.5.xlsx", sheet="Listing's Plane")
leftLP_2 <- read.xlsx("result_turntable_9pt_left_2.xlsx", sheet="Listing's Plane")
leftLP_2.5 <- read.xlsx("result_turntable_9pt_left_2.5.xlsx", sheet="Listing's Plane")
leftLP_3 <- read.xlsx("result_turntable_9pt_left_3.xlsx", sheet="Listing's Plane")
leftLP_3.5 <- read.xlsx("result_turntable_9pt_left_3.5.xlsx", sheet="Listing's Plane")
leftLP_4 <- read.xlsx("result_turntable_9pt_left_4.xlsx", sheet="Listing's Plane")
leftLP_4.5 <- read.xlsx("result_turntable_9pt_left_4.5.xlsx", sheet="Listing's Plane")
leftLP_5 <- read.xlsx("result_turntable_9pt_left_5.xlsx", sheet="Listing's Plane")
leftLP_5.5 <- read.xlsx("result_turntable_9pt_left_5.5.xlsx", sheet="Listing's Plane")
leftLP_6 <- read.xlsx("result_turntable_9pt_left_6.xlsx", sheet="Listing's Plane")
leftLP_7 <- read.xlsx("result_turntable_9pt_left_7.xlsx", sheet="Listing's Plane")
leftLP_7.5 <- read.xlsx("result_turntable_9pt_left_7.5.xlsx", sheet="Listing's Plane")
leftLP_8 <- read.xlsx("result_turntable_9pt_left_8.xlsx", sheet="Listing's Plane")
leftLP_8.5 <- read.xlsx("result_turntable_9pt_left_8.5.xlsx", sheet="Listing's Plane")
leftLP_9 <- read.xlsx("result_turntable_9pt_left_9.xlsx", sheet="Listing's Plane")
leftLP_9.5 <- read.xlsx("result_turntable_9pt_left_9.5.xlsx", sheet="Listing's Plane")
leftLP_10 <- read.xlsx("result_turntable_9pt_left_10.xlsx", sheet="Listing's Plane")
leftLP_10.5 <- read.xlsx("result_turntable_9pt_left_10.5.xlsx", sheet="Listing's Plane")
leftLP_11 <- read.xlsx("result_turntable_9pt_left_11.xlsx", sheet="Listing's Plane")
leftLP_11.5 <- read.xlsx("result_turntable_9pt_left_11.5.xlsx", sheet="Listing's Plane")
leftLP_12 <- read.xlsx("result_turntable_9pt_left_12.xlsx", sheet="Listing's Plane")

#average x
leftLP_averagex_control <- leftLP_control$LP_average_x
leftLP_averagex_1 <- leftLP_1$LP_average_x
leftLP_averagex_1.5 <- leftLP_1.5$LP_average_x
leftLP_averagex_2 <- leftLP_2$LP_average_x
leftLP_averagex_2.5 <- leftLP_2.5$LP_average_x
leftLP_averagex_3 <- leftLP_3$LP_average_x
leftLP_averagex_3.5 <- leftLP_3.5$LP_average_x
leftLP_averagex_4 <- leftLP_4$LP_average_x
leftLP_averagex_4.5 <- leftLP_4.5$LP_average_x
leftLP_averagex_5 <- leftLP_5$LP_average_x
leftLP_averagex_5.5 <- leftLP_5.5$LP_average_x
leftLP_averagex_6 <- leftLP_6$LP_average_x
leftLP_averagex_7 <- leftLP_7$LP_average_x
leftLP_averagex_7.5 <- leftLP_7.5$LP_average_x
leftLP_averagex_8 <- leftLP_8$LP_average_x
leftLP_averagex_8.5 <- leftLP_8.5$LP_average_x
leftLP_averagex_9 <- leftLP_9$LP_average_x
leftLP_averagex_9.5 <- leftLP_9.5$LP_average_x
leftLP_averagex_10 <- leftLP_10$LP_average_x
leftLP_averagex_10.5 <- leftLP_10.5$LP_average_x
leftLP_averagex_11 <- leftLP_11$LP_average_x
leftLP_averagex_11.5 <- leftLP_11.5$LP_average_x
leftLP_averagex_12 <- leftLP_12$LP_average_x

time_group <- c(0, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12)
leftLP_averagex <- c(leftLP_averagex_control, leftLP_averagex_1, leftLP_averagex_1.5, leftLP_averagex_2, leftLP_averagex_2.5, leftLP_averagex_3, leftLP_averagex_3.5,leftLP_averagex_4, leftLP_averagex_4.5, leftLP_averagex_5, leftLP_averagex_5.5, leftLP_averagex_6, leftLP_averagex_7, leftLP_averagex_7.5, leftLP_averagex_8,leftLP_averagex_8.5,  leftLP_averagex_9, leftLP_averagex_9.5, leftLP_averagex_10,leftLP_averagex_10.5,  leftLP_averagex_11, leftLP_averagex_11.5, leftLP_averagex_12)

#standard deviation
leftLP_xsd_control <- leftLP_control$LP_SD_x
leftLP_xsd_1 <- leftLP_1$LP_SD_x
leftLP_xsd_1.5 <- leftLP_1.5$LP_SD_x
leftLP_xsd_2 <- leftLP_2$LP_SD_x
leftLP_xsd_2.5 <- leftLP_2.5$LP_SD_x
leftLP_xsd_3 <- leftLP_3$LP_SD_x
leftLP_xsd_3.5 <- leftLP_3.5$LP_SD_x
leftLP_xsd_4 <- leftLP_4$LP_SD_x
leftLP_xsd_4.5 <- leftLP_4.5$LP_SD_x
leftLP_xsd_5 <- leftLP_5$LP_SD_x
leftLP_xsd_5.5 <- leftLP_5.5$LP_SD_x
leftLP_xsd_6 <- leftLP_6$LP_SD_x
leftLP_xsd_7 <- leftLP_7$LP_SD_x
leftLP_xsd_7.5 <- leftLP_7.5$LP_SD_x
leftLP_xsd_8 <- leftLP_8$LP_SD_x
leftLP_xsd_8.5 <- leftLP_8.5$LP_SD_x
leftLP_xsd_9 <- leftLP_9$LP_SD_x
leftLP_xsd_9.5 <- leftLP_9.5$LP_SD_x
leftLP_xsd_10 <- leftLP_10$LP_SD_x
leftLP_xsd_10.5 <- leftLP_10.5$LP_SD_x
leftLP_xsd_11 <- leftLP_11$LP_SD_x
leftLP_xsd_11.5 <- leftLP_11.5$LP_SD_x
leftLP_xsd_12 <- leftLP_12$LP_SD_x

leftLP_xsd <- c(leftLP_xsd_control, leftLP_xsd_1, leftLP_xsd_1.5, leftLP_xsd_2,leftLP_xsd_2.5, leftLP_xsd_3, leftLP_xsd_3.5, leftLP_xsd_4, leftLP_xsd_4.5, leftLP_xsd_5, leftLP_xsd_5.5, leftLP_xsd_6, leftLP_xsd_7, leftLP_xsd_7.5, leftLP_xsd_8, leftLP_xsd_8.5, leftLP_xsd_9, leftLP_xsd_9.5, leftLP_xsd_10, leftLP_xsd_10.5, leftLP_xsd_11, leftLP_xsd_11.5, leftLP_xsd_12)

leftLP_averagex_df <- data.frame(time_group, leftLP_averagex, leftLP_xsd)
#adjust by subtracting control value to all data
leftLP_averagex_df$left_adj_avex <- leftLP_averagex_df$leftLP_averagex - leftLP_averagex_control

plot_averagex_left <- ggplot(leftLP_averagex_df, aes(x=time_group, y=leftLP_averagex)) +
  geom_point(colour="red", size=3) +
  geom_errorbar(aes(ymin=leftLP_averagex-leftLP_xsd, ymax=leftLP_averagex+leftLP_xsd), colour="pink", width=0.2) +
  scale_x_continuous(name="Time Group", breaks=seq(0, 12,1)) +
  scale_y_continuous(name="Listing's Plane Average X", breaks=seq(-14, 14, by=1)) +
  geom_hline(yintercept=0, color="grey") +
  geom_vline(xintercept=6.5, color="darkred", linetype="dashed") +
  theme_classic() +
  ggtitle("leftward rotation")
plot_averagex_left

plot_averagex_controladj_left <- ggplot(leftLP_averagex_df, aes(x=time_group, y=left_adj_avex)) +
  geom_point(colour="red", size=3) +
  geom_errorbar(aes(ymin=left_adj_avex-leftLP_xsd, ymax=left_adj_avex+leftLP_xsd), colour="pink", width=0.2) +
  scale_x_continuous(name="Time Group", breaks=seq(0, 12,1)) +
  scale_y_continuous(name="Listing's Plane Average X", breaks=seq(-14, 14, by=1)) +
  geom_hline(yintercept=0, color="grey") +
  geom_vline(xintercept=6.5, color="darkred", linetype="dashed") +
  theme_classic()+
  ggtitle("leftward rotation")
plot_averagex_controladj_left




#Both
plot_averagex_both <- ggplot() +
  geom_point(data=leftLP_averagex_df, aes(x=time_group, y=leftLP_averagex), colour="blue", size=3) +
  geom_errorbar(data=leftLP_averagex_df, aes(x=time_group, ymin=leftLP_averagex-leftLP_xsd, ymax=leftLP_averagex+leftLP_xsd), colour="lightblue", width=0.2) +
  geom_point(data=rightLP_averagex_df, aes(x=time_group, y=rightLP_averagex), colour="red", size=3) +
  geom_errorbar(data=rightLP_averagex_df, aes(x=time_group, ymin=rightLP_averagex-rightLP_xsd, ymax=rightLP_averagex+rightLP_xsd), colour="pink", width=0.2) +
  scale_x_continuous(name="Time Group", breaks=seq(0, 12,1)) +
  scale_y_continuous(name="Listing's Plane Average X", breaks=seq(-14, 14, by=1)) +
  geom_hline(yintercept=0, color="grey") +
  geom_vline(xintercept=6.5, color="darkred", linetype="dashed") +
  theme_classic()
plot_averagex_both

plot_averagex_both_controladj <- ggplot() +
  geom_point(data=leftLP_averagex_df, aes(x=time_group, y=left_adj_avex), colour="blue", size=3) +
  geom_errorbar(data=leftLP_averagex_df, aes(x=time_group, ymin=left_adj_avex-leftLP_xsd, ymax=left_adj_avex+leftLP_xsd), colour="lightblue", width=0.2) +
  geom_point(data=rightLP_averagex_df, aes(x=time_group, y=right_adj_avex), colour="red", size=3) +
  geom_errorbar(data=rightLP_averagex_df, aes(x=time_group, ymin=right_adj_avex-rightLP_xsd, ymax=right_adj_avex+rightLP_xsd), colour="pink", width=0.2) +
  scale_x_continuous(name="Time Group", breaks=seq(0, 12,1)) +
  scale_y_continuous(name="Listing's Plane Average X", breaks=seq(-14, 14, by=1)) +
  geom_hline(yintercept=0, color="grey") +
  geom_vline(xintercept=6.5, color="darkred", linetype="dashed") +
  theme_classic()
plot_averagex_both_controladj



output_list <- list(rightLP_averagex_df, leftLP_averagex_df)
write.xlsx(output_list, paste("LP_averagex_TT_result_", id, ".xlsx", sep=""), overwrite=TRUE)


pdf(paste("LP_averagex_bothTT_result_", id, ".pdf", sep=""), width = 10, height = 5)
plot_averagex_right
plot_averagex_controladj_right
plot_averagex_left
plot_averagex_controladj_left
plot_averagex_both
plot_averagex_both_controladj
dev.off()