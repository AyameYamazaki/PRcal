



##set working directory **************************************************************************

rm(list = ls())

library(ggplot2)
library(tidyverse)
library(readxl)
library(openxlsx)
library(ggthemes)

#date and patient initials change accordingly!!!!!!!!!!!!!!!!!!!!!!!! *****************************************************!!!!!!!!!!!!!!!!
date = "20230209"
initials = "NA"
angle = "60"
head = "HU"

id = paste(date, initials, "_", angle, head, sep="")


#left side
rightLP_control <- read.xlsx("result_turntable_per_9pt_right_control.xlsx", sheet="Listing's Plane")
rightLP_per_1 <- read.xlsx("result_turntable_per_9pt_right_1.xlsx", sheet="Listing's Plane")
rightLP_per_2 <- read.xlsx("result_turntable_per_9pt_right_2.xlsx", sheet="Listing's Plane")
rightLP_per_3 <- read.xlsx("result_turntable_per_9pt_right_3.xlsx", sheet="Listing's Plane")
rightLP_per_4 <- read.xlsx("result_turntable_per_9pt_right_4.xlsx", sheet="Listing's Plane")
rightLP_per_5 <- read.xlsx("result_turntable_per_9pt_right_5.xlsx", sheet="Listing's Plane")
rightLP_per_6 <- read.xlsx("result_turntable_per_9pt_right_6.xlsx", sheet="Listing's Plane")
rightLP_post_7 <- read.xlsx("result_turntable_post_9pt_right_7.xlsx", sheet="Listing's Plane")
rightLP_post_8 <- read.xlsx("result_turntable_post_9pt_right_8.xlsx", sheet="Listing's Plane")
rightLP_post_9 <- read.xlsx("result_turntable_post_9pt_right_9.xlsx", sheet="Listing's Plane")
rightLP_post_10 <- read.xlsx("result_turntable_post_9pt_right_10.xlsx", sheet="Listing's Plane")
rightLP_post_11 <- read.xlsx("result_turntable_post_9pt_right_11.xlsx", sheet="Listing's Plane")
rightLP_post_12 <- read.xlsx("result_turntable_post_9pt_right_12.xlsx", sheet="Listing's Plane")

#average x
rightLP_averagex_control <- rightLP_control$LP_average_x
rightLP_averagex_per_1 <- rightLP_per_1$LP_average_x
rightLP_averagex_per_2 <- rightLP_per_2$LP_average_x
rightLP_averagex_per_3 <- rightLP_per_3$LP_average_x
rightLP_averagex_per_4 <- rightLP_per_4$LP_average_x
rightLP_averagex_per_5 <- rightLP_per_5$LP_average_x
rightLP_averagex_per_6 <- rightLP_per_6$LP_average_x
rightLP_averagex_post_7 <- rightLP_post_7$LP_average_x
rightLP_averagex_post_8 <- rightLP_post_8$LP_average_x
rightLP_averagex_post_9 <- rightLP_post_9$LP_average_x
rightLP_averagex_post_10 <- rightLP_post_10$LP_average_x
rightLP_averagex_post_11 <- rightLP_post_11$LP_average_x
rightLP_averagex_post_12 <- rightLP_post_12$LP_average_x

time_group <- c(0:12)
rightLP_averagex <- c(rightLP_averagex_control, rightLP_averagex_per_1, rightLP_averagex_per_2, rightLP_averagex_per_3, rightLP_averagex_per_4, rightLP_averagex_per_5, rightLP_averagex_per_6, rightLP_averagex_post_7, rightLP_averagex_post_8, rightLP_averagex_post_9, rightLP_averagex_post_10, rightLP_averagex_post_11, rightLP_averagex_post_12)

#standard deviation
rightLP_xsd_control <- rightLP_control$LP_SD_x
rightLP_xsd_per_1 <- rightLP_per_1$LP_SD_x
rightLP_xsd_per_2 <- rightLP_per_2$LP_SD_x
rightLP_xsd_per_3 <- rightLP_per_3$LP_SD_x
rightLP_xsd_per_4 <- rightLP_per_4$LP_SD_x
rightLP_xsd_per_5 <- rightLP_per_5$LP_SD_x
rightLP_xsd_per_6 <- rightLP_per_6$LP_SD_x
rightLP_xsd_post_7 <- rightLP_post_7$LP_SD_x
rightLP_xsd_post_8 <- rightLP_post_8$LP_SD_x
rightLP_xsd_post_9 <- rightLP_post_9$LP_SD_x
rightLP_xsd_post_10 <- rightLP_post_10$LP_SD_x
rightLP_xsd_post_11 <- rightLP_post_11$LP_SD_x
rightLP_xsd_post_12 <- rightLP_post_12$LP_SD_x

rightLP_xsd <- c(rightLP_xsd_control, rightLP_xsd_per_1, rightLP_xsd_per_2, rightLP_xsd_per_3, rightLP_xsd_per_4, rightLP_xsd_per_5, rightLP_xsd_per_6, rightLP_xsd_post_7, rightLP_xsd_post_8, rightLP_xsd_post_9, rightLP_xsd_post_10, rightLP_xsd_post_11, rightLP_xsd_post_12)

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
  ggtitle("Rightward rotation")
plot_averagex_right

plot_averagex_controladj_right <- ggplot(rightLP_averagex_df, aes(x=time_group, y=right_adj_avex)) +
  geom_point(colour="red", size=3) +
  geom_errorbar(aes(ymin=right_adj_avex-rightLP_xsd, ymax=right_adj_avex+rightLP_xsd), colour="pink", width=0.2) +
  scale_x_continuous(name="Time Group", breaks=seq(0, 12,1)) +
  scale_y_continuous(name="Listing's Plane Average X", breaks=seq(-14, 14, by=1)) +
  geom_hline(yintercept=0, color="grey") +
  geom_vline(xintercept=6.5, color="darkred", linetype="dashed") +
  theme_classic()+
  ggtitle("Rightward rotation")
plot_averagex_controladj_right





#right side
leftLP_control <- read.xlsx("result_turntable_per_9pt_left_control.xlsx", sheet="Listing's Plane")
leftLP_per_1 <- read.xlsx("result_turntable_per_9pt_left_1.xlsx", sheet="Listing's Plane")
leftLP_per_2 <- read.xlsx("result_turntable_per_9pt_left_2.xlsx", sheet="Listing's Plane")
leftLP_per_3 <- read.xlsx("result_turntable_per_9pt_left_3.xlsx", sheet="Listing's Plane")
leftLP_per_4 <- read.xlsx("result_turntable_per_9pt_left_4.xlsx", sheet="Listing's Plane")
leftLP_per_5 <- read.xlsx("result_turntable_per_9pt_left_5.xlsx", sheet="Listing's Plane")
leftLP_per_6 <- read.xlsx("result_turntable_per_9pt_left_6.xlsx", sheet="Listing's Plane")
leftLP_post_7 <- read.xlsx("result_turntable_post_9pt_left_7.xlsx", sheet="Listing's Plane")
leftLP_post_8 <- read.xlsx("result_turntable_post_9pt_left_8.xlsx", sheet="Listing's Plane")
leftLP_post_9 <- read.xlsx("result_turntable_post_9pt_left_9.xlsx", sheet="Listing's Plane")
leftLP_post_10 <- read.xlsx("result_turntable_post_9pt_left_10.xlsx", sheet="Listing's Plane")
leftLP_post_11 <- read.xlsx("result_turntable_post_9pt_left_11.xlsx", sheet="Listing's Plane")
leftLP_post_12 <- read.xlsx("result_turntable_post_9pt_left_12.xlsx", sheet="Listing's Plane")

#average x
leftLP_averagex_control <- leftLP_control$LP_average_x
leftLP_averagex_per_1 <- leftLP_per_1$LP_average_x
leftLP_averagex_per_2 <- leftLP_per_2$LP_average_x
leftLP_averagex_per_3 <- leftLP_per_3$LP_average_x
leftLP_averagex_per_4 <- leftLP_per_4$LP_average_x
leftLP_averagex_per_5 <- leftLP_per_5$LP_average_x
leftLP_averagex_per_6 <- leftLP_per_6$LP_average_x
leftLP_averagex_post_7 <- leftLP_post_7$LP_average_x
leftLP_averagex_post_8 <- leftLP_post_8$LP_average_x
leftLP_averagex_post_9 <- leftLP_post_9$LP_average_x
leftLP_averagex_post_10 <- leftLP_post_10$LP_average_x
leftLP_averagex_post_11 <- leftLP_post_11$LP_average_x
leftLP_averagex_post_12 <- leftLP_post_12$LP_average_x

time_group <- c(0:12)
leftLP_averagex <- c(leftLP_averagex_control, leftLP_averagex_per_1, leftLP_averagex_per_2, leftLP_averagex_per_3, leftLP_averagex_per_4, leftLP_averagex_per_5, leftLP_averagex_per_6, leftLP_averagex_post_7, leftLP_averagex_post_8, leftLP_averagex_post_9, leftLP_averagex_post_10, leftLP_averagex_post_11, leftLP_averagex_post_12)

#standard deviation
leftLP_xsd_control <- leftLP_control$LP_SD_x
leftLP_xsd_per_1 <- leftLP_per_1$LP_SD_x
leftLP_xsd_per_2 <- leftLP_per_2$LP_SD_x
leftLP_xsd_per_3 <- leftLP_per_3$LP_SD_x
leftLP_xsd_per_4 <- leftLP_per_4$LP_SD_x
leftLP_xsd_per_5 <- leftLP_per_5$LP_SD_x
leftLP_xsd_per_6 <- leftLP_per_6$LP_SD_x
leftLP_xsd_post_7 <- leftLP_post_7$LP_SD_x
leftLP_xsd_post_8 <- leftLP_post_8$LP_SD_x
leftLP_xsd_post_9 <- leftLP_post_9$LP_SD_x
leftLP_xsd_post_10 <- leftLP_post_10$LP_SD_x
leftLP_xsd_post_11 <- leftLP_post_11$LP_SD_x
leftLP_xsd_post_12 <- leftLP_post_12$LP_SD_x

leftLP_xsd <- c(leftLP_xsd_control, leftLP_xsd_per_1, leftLP_xsd_per_2, leftLP_xsd_per_3, leftLP_xsd_per_4, leftLP_xsd_per_5, leftLP_xsd_per_6, leftLP_xsd_post_7, leftLP_xsd_post_8, leftLP_xsd_post_9, leftLP_xsd_post_10, leftLP_xsd_post_11, leftLP_xsd_post_12)

leftLP_averagex_df <- data.frame(time_group, leftLP_averagex, leftLP_xsd)
#adjust by subtracting control value to all data
leftLP_averagex_df$left_adj_avex <- leftLP_averagex_df$leftLP_averagex - leftLP_averagex_control

plot_averagex_left <- ggplot(leftLP_averagex_df, aes(x=time_group, y=leftLP_averagex)) +
  geom_point(colour="blue", size=3) +
  geom_errorbar(aes(ymin=leftLP_averagex-leftLP_xsd, ymax=leftLP_averagex+leftLP_xsd), colour="lightblue", width=0.2) +
  scale_x_continuous(name="Time Group", breaks=seq(0, 12,1)) +
  scale_y_continuous(name="Listing's Plane Average X", breaks=seq(-14, 14, by=1)) +
  geom_hline(yintercept=0, color="grey") +
  geom_vline(xintercept=6.5, color="darkred", linetype="dashed") +
  theme_classic() +
  ggtitle("Leftward rotation")
plot_averagex_left

plot_averagex_controladj_left <- ggplot(leftLP_averagex_df, aes(x=time_group, y=left_adj_avex)) +
  geom_point(colour="blue", size=3) +
  geom_errorbar(aes(ymin=left_adj_avex-leftLP_xsd, ymax=left_adj_avex+leftLP_xsd), colour="lightblue", width=0.2) +
  scale_x_continuous(name="Time Group", breaks=seq(0, 12,1)) +
  scale_y_continuous(name="Listing's Plane Average X", breaks=seq(-14, 14, by=1)) +
  geom_hline(yintercept=0, color="grey") +
  geom_vline(xintercept=6.5, color="darkred", linetype="dashed") +
  theme_classic() +
  ggtitle("Leftward rotation")
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
write.xlsx(output_list, paste("LP_averagex_TT_result_", id, "8.11.xlsx", sep=""), overwrite=TRUE)


pdf(paste("LP_averagex_bothTT_result_", id, "8.11.pdf", sep=""), width = 10, height = 5)
plot_averagex_right
plot_averagex_controladj_right
plot_averagex_left
plot_averagex_controladj_left
plot_averagex_both
plot_averagex_both_controladj
dev.off()