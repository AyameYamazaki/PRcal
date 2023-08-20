



##set working directory **************************************************************************

rm(list = ls())

library(ggplot2)
library(tidyverse)
library(readxl)
library(openxlsx)
library(ggthemes)

#date and patient initials change accordingly!!!!!!!!!!!!!!!!!!!!!!!! *****************************************************!!!!!!!!!!!!!!!!
date = "20200720"
initials = "YH"

id = paste(date, initials, "_", sep="")


#right side
time_group_rt <- c(0:30)

rightLP_control <- read.xlsx("result_caloric_9pt_right_control.xlsx", sheet="Listing's Plane")
rightLP_1 <- read.xlsx("result_caloric_9pt_right_1.xlsx", sheet="Listing's Plane")
rightLP_2 <- read.xlsx("result_caloric_9pt_right_2.xlsx", sheet="Listing's Plane")
rightLP_3 <- read.xlsx("result_caloric_9pt_right_3.xlsx", sheet="Listing's Plane")
rightLP_4 <- read.xlsx("result_caloric_9pt_right_4.xlsx", sheet="Listing's Plane")
rightLP_5 <- read.xlsx("result_caloric_9pt_right_5.xlsx", sheet="Listing's Plane")
rightLP_6 <- read.xlsx("result_caloric_9pt_right_6.xlsx", sheet="Listing's Plane")
rightLP_7 <- read.xlsx("result_caloric_9pt_right_7.xlsx", sheet="Listing's Plane")
rightLP_8 <- read.xlsx("result_caloric_9pt_right_8.xlsx", sheet="Listing's Plane")
rightLP_9 <- read.xlsx("result_caloric_9pt_right_9.xlsx", sheet="Listing's Plane")
rightLP_10 <- read.xlsx("result_caloric_9pt_right_10.xlsx", sheet="Listing's Plane")
rightLP_11 <- read.xlsx("result_caloric_9pt_right_11.xlsx", sheet="Listing's Plane")
rightLP_12 <- read.xlsx("result_caloric_9pt_right_12.xlsx", sheet="Listing's Plane")
rightLP_13 <- read.xlsx("result_caloric_9pt_right_13.xlsx", sheet="Listing's Plane")
rightLP_14 <- read.xlsx("result_caloric_9pt_right_14.xlsx", sheet="Listing's Plane")
rightLP_15 <- read.xlsx("result_caloric_9pt_right_15.xlsx", sheet="Listing's Plane")
rightLP_16 <- read.xlsx("result_caloric_9pt_right_16.xlsx", sheet="Listing's Plane")
rightLP_17 <- read.xlsx("result_caloric_9pt_right_17.xlsx", sheet="Listing's Plane")
rightLP_18 <- read.xlsx("result_caloric_9pt_right_18.xlsx", sheet="Listing's Plane")
rightLP_19 <- read.xlsx("result_caloric_9pt_right_19.xlsx", sheet="Listing's Plane")
rightLP_20 <- read.xlsx("result_caloric_9pt_right_20.xlsx", sheet="Listing's Plane")
rightLP_21 <- read.xlsx("result_caloric_9pt_right_21.xlsx", sheet="Listing's Plane")
rightLP_22 <- read.xlsx("result_caloric_9pt_right_22.xlsx", sheet="Listing's Plane")
rightLP_23 <- read.xlsx("result_caloric_9pt_right_23.xlsx", sheet="Listing's Plane")
rightLP_24 <- read.xlsx("result_caloric_9pt_right_24.xlsx", sheet="Listing's Plane")
rightLP_25 <- read.xlsx("result_caloric_9pt_right_25.xlsx", sheet="Listing's Plane")
rightLP_26 <- read.xlsx("result_caloric_9pt_right_26.xlsx", sheet="Listing's Plane")
rightLP_27 <- read.xlsx("result_caloric_9pt_right_27.xlsx", sheet="Listing's Plane")
rightLP_28 <- read.xlsx("result_caloric_9pt_right_28.xlsx", sheet="Listing's Plane")
rightLP_29 <- read.xlsx("result_caloric_9pt_right_29.xlsx", sheet="Listing's Plane")
rightLP_30 <- read.xlsx("result_caloric_9pt_right_30.xlsx", sheet="Listing's Plane")

#average x
rightLP_averagex_control <- rightLP_control$LP_average_x
rightLP_averagex_1 <- rightLP_1$LP_average_x
rightLP_averagex_2 <- rightLP_2$LP_average_x
rightLP_averagex_3 <- rightLP_3$LP_average_x
rightLP_averagex_4 <- rightLP_4$LP_average_x
rightLP_averagex_5 <- rightLP_5$LP_average_x
rightLP_averagex_6 <- rightLP_6$LP_average_x
rightLP_averagex_7 <- rightLP_7$LP_average_x
rightLP_averagex_8 <- rightLP_8$LP_average_x
rightLP_averagex_9 <- rightLP_9$LP_average_x
rightLP_averagex_10 <- rightLP_10$LP_average_x
rightLP_averagex_11 <- rightLP_11$LP_average_x
rightLP_averagex_12 <- rightLP_12$LP_average_x
rightLP_averagex_13 <- rightLP_13$LP_average_x
rightLP_averagex_14 <- rightLP_14$LP_average_x
rightLP_averagex_15 <- rightLP_15$LP_average_x
rightLP_averagex_16 <- rightLP_16$LP_average_x
rightLP_averagex_17 <- rightLP_17$LP_average_x
rightLP_averagex_18 <- rightLP_18$LP_average_x
rightLP_averagex_19 <- rightLP_19$LP_average_x
rightLP_averagex_20 <- rightLP_20$LP_average_x
rightLP_averagex_21 <- rightLP_21$LP_average_x
rightLP_averagex_22 <- rightLP_22$LP_average_x
rightLP_averagex_23 <- rightLP_23$LP_average_x
rightLP_averagex_24 <- rightLP_24$LP_average_x
rightLP_averagex_25 <- rightLP_25$LP_average_x
rightLP_averagex_26 <- rightLP_26$LP_average_x
rightLP_averagex_27 <- rightLP_27$LP_average_x
rightLP_averagex_28 <- rightLP_28$LP_average_x
rightLP_averagex_29 <- rightLP_29$LP_average_x
rightLP_averagex_30 <- rightLP_30$LP_average_x


rightLP_averagex <- c(rightLP_averagex_control, rightLP_averagex_1, rightLP_averagex_2, rightLP_averagex_3, rightLP_averagex_4, rightLP_averagex_5, rightLP_averagex_6, rightLP_averagex_7, rightLP_averagex_8, rightLP_averagex_9, rightLP_averagex_10, rightLP_averagex_11, rightLP_averagex_12, rightLP_averagex_13, rightLP_averagex_14, rightLP_averagex_15, rightLP_averagex_16, rightLP_averagex_17, rightLP_averagex_18, rightLP_averagex_19, rightLP_averagex_20, rightLP_averagex_21, rightLP_averagex_22, rightLP_averagex_23, rightLP_averagex_24, rightLP_averagex_25, rightLP_averagex_26, rightLP_averagex_27, rightLP_averagex_28, rightLP_averagex_29, rightLP_averagex_30)

#standard deviation
rightLP_xsd_control <- rightLP_control$LP_SD_x
rightLP_xsd_1 <- rightLP_1$LP_SD_x
rightLP_xsd_2 <- rightLP_2$LP_SD_x
rightLP_xsd_3 <- rightLP_3$LP_SD_x
rightLP_xsd_4 <- rightLP_4$LP_SD_x
rightLP_xsd_5 <- rightLP_5$LP_SD_x
rightLP_xsd_6 <- rightLP_6$LP_SD_x
rightLP_xsd_7 <- rightLP_7$LP_SD_x
rightLP_xsd_8 <- rightLP_8$LP_SD_x
rightLP_xsd_9 <- rightLP_9$LP_SD_x
rightLP_xsd_10 <- rightLP_10$LP_SD_x
rightLP_xsd_11 <- rightLP_11$LP_SD_x
rightLP_xsd_12 <- rightLP_12$LP_SD_x
rightLP_xsd_13 <- rightLP_13$LP_SD_x
rightLP_xsd_14 <- rightLP_14$LP_SD_x
rightLP_xsd_15 <- rightLP_15$LP_SD_x
rightLP_xsd_16 <- rightLP_16$LP_SD_x
rightLP_xsd_17 <- rightLP_17$LP_SD_x
rightLP_xsd_18 <- rightLP_18$LP_SD_x
rightLP_xsd_19 <- rightLP_19$LP_SD_x
rightLP_xsd_20 <- rightLP_20$LP_SD_x
rightLP_xsd_21 <- rightLP_21$LP_SD_x
rightLP_xsd_22 <- rightLP_22$LP_SD_x
rightLP_xsd_23 <- rightLP_23$LP_SD_x
rightLP_xsd_24 <- rightLP_24$LP_SD_x
rightLP_xsd_25 <- rightLP_25$LP_SD_x
rightLP_xsd_26 <- rightLP_26$LP_SD_x
rightLP_xsd_27 <- rightLP_27$LP_SD_x
rightLP_xsd_28 <- rightLP_28$LP_SD_x
rightLP_xsd_29 <- rightLP_29$LP_SD_x
rightLP_xsd_30 <- rightLP_30$LP_SD_x



rightLP_xsd <- c(rightLP_xsd_control, rightLP_xsd_1, rightLP_xsd_2, rightLP_xsd_3, rightLP_xsd_4, rightLP_xsd_5, rightLP_xsd_6, rightLP_xsd_7, rightLP_xsd_8, rightLP_xsd_9, rightLP_xsd_10, rightLP_xsd_11, rightLP_xsd_12, rightLP_xsd_13, rightLP_xsd_14, rightLP_xsd_15, rightLP_xsd_16, rightLP_xsd_17, rightLP_xsd_18, rightLP_xsd_19, rightLP_xsd_20, rightLP_xsd_21, rightLP_xsd_22, rightLP_xsd_23, rightLP_xsd_24, rightLP_xsd_25, rightLP_xsd_26, rightLP_xsd_27, rightLP_xsd_28, rightLP_xsd_29, rightLP_xsd_30)

rightLP_averagex_df <- data.frame(time_group_rt, rightLP_averagex, rightLP_xsd)
#adjust by subtracting control value to all data
rightLP_averagex_df$right_adj_avex <- rightLP_averagex_df$rightLP_averagex - rightLP_averagex_control

plot_averagex_right <- ggplot(rightLP_averagex_df, aes(x=time_group_rt, y=rightLP_averagex)) +
  geom_point(colour="red", size=3) +
  geom_errorbar(aes(ymin=rightLP_averagex-rightLP_xsd, ymax=rightLP_averagex+rightLP_xsd), colour="pink", width=0.2) +
  scale_x_continuous(name="Time Group", breaks=seq(0, 30,5)) +
  scale_y_continuous(name="Listing's Plane Average X", breaks=seq(-14, 14, by=1)) +
  geom_hline(yintercept=0, color="grey") +
  theme_classic() +
  ggtitle("Right ear")
plot_averagex_right

plot_averagex_controladj_right <- ggplot(rightLP_averagex_df, aes(x=time_group_rt, y=right_adj_avex)) +
  geom_point(colour="red", size=3) +
  geom_errorbar(aes(ymin=right_adj_avex-rightLP_xsd, ymax=right_adj_avex+rightLP_xsd), colour="pink", width=0.2) +
  scale_x_continuous(name="Time Group", breaks=seq(0, 12,1)) +
  scale_y_continuous(name="Listing's Plane Average X", breaks=seq(-14, 14, by=1)) +
  geom_hline(yintercept=0, color="grey") +
  theme_classic()+
  ggtitle("Right ear")
plot_averagex_controladj_right




#left side
time_group_lt <- c(0:30)

leftLP_control <- read.xlsx("result_caloric_9pt_left_control.xlsx", sheet="Listing's Plane")
leftLP_1 <- read.xlsx("result_caloric_9pt_left_1.xlsx", sheet="Listing's Plane")
leftLP_2 <- read.xlsx("result_caloric_9pt_left_2.xlsx", sheet="Listing's Plane")
leftLP_3 <- read.xlsx("result_caloric_9pt_left_3.xlsx", sheet="Listing's Plane")
leftLP_4 <- read.xlsx("result_caloric_9pt_left_4.xlsx", sheet="Listing's Plane")
leftLP_5 <- read.xlsx("result_caloric_9pt_left_5.xlsx", sheet="Listing's Plane")
leftLP_6 <- read.xlsx("result_caloric_9pt_left_6.xlsx", sheet="Listing's Plane")
leftLP_7 <- read.xlsx("result_caloric_9pt_left_7.xlsx", sheet="Listing's Plane")
leftLP_8 <- read.xlsx("result_caloric_9pt_left_8.xlsx", sheet="Listing's Plane")
leftLP_9 <- read.xlsx("result_caloric_9pt_left_9.xlsx", sheet="Listing's Plane")
leftLP_10 <- read.xlsx("result_caloric_9pt_left_10.xlsx", sheet="Listing's Plane")
leftLP_11 <- read.xlsx("result_caloric_9pt_left_11.xlsx", sheet="Listing's Plane")
leftLP_12 <- read.xlsx("result_caloric_9pt_left_12.xlsx", sheet="Listing's Plane")
leftLP_13 <- read.xlsx("result_caloric_9pt_left_13.xlsx", sheet="Listing's Plane")
leftLP_14 <- read.xlsx("result_caloric_9pt_left_14.xlsx", sheet="Listing's Plane")
leftLP_15 <- read.xlsx("result_caloric_9pt_left_15.xlsx", sheet="Listing's Plane")
leftLP_16 <- read.xlsx("result_caloric_9pt_left_16.xlsx", sheet="Listing's Plane")
leftLP_17 <- read.xlsx("result_caloric_9pt_left_17.xlsx", sheet="Listing's Plane")
leftLP_18 <- read.xlsx("result_caloric_9pt_left_18.xlsx", sheet="Listing's Plane")
leftLP_19 <- read.xlsx("result_caloric_9pt_left_19.xlsx", sheet="Listing's Plane")
leftLP_20 <- read.xlsx("result_caloric_9pt_left_20.xlsx", sheet="Listing's Plane")
leftLP_21 <- read.xlsx("result_caloric_9pt_left_21.xlsx", sheet="Listing's Plane")
leftLP_22 <- read.xlsx("result_caloric_9pt_left_22.xlsx", sheet="Listing's Plane")
leftLP_23 <- read.xlsx("result_caloric_9pt_left_23.xlsx", sheet="Listing's Plane")
leftLP_24 <- read.xlsx("result_caloric_9pt_left_24.xlsx", sheet="Listing's Plane")
leftLP_25 <- read.xlsx("result_caloric_9pt_left_25.xlsx", sheet="Listing's Plane")
leftLP_26 <- read.xlsx("result_caloric_9pt_left_26.xlsx", sheet="Listing's Plane")
leftLP_27 <- read.xlsx("result_caloric_9pt_left_27.xlsx", sheet="Listing's Plane")
leftLP_28 <- read.xlsx("result_caloric_9pt_left_28.xlsx", sheet="Listing's Plane")
leftLP_29 <- read.xlsx("result_caloric_9pt_left_29.xlsx", sheet="Listing's Plane")
leftLP_30 <- read.xlsx("result_caloric_9pt_left_30.xlsx", sheet="Listing's Plane")

#average x
leftLP_averagex_control <- leftLP_control$LP_average_x
leftLP_averagex_1 <- leftLP_1$LP_average_x
leftLP_averagex_2 <- leftLP_2$LP_average_x
leftLP_averagex_3 <- leftLP_3$LP_average_x
leftLP_averagex_4 <- leftLP_4$LP_average_x
leftLP_averagex_5 <- leftLP_5$LP_average_x
leftLP_averagex_6 <- leftLP_6$LP_average_x
leftLP_averagex_7 <- leftLP_7$LP_average_x
leftLP_averagex_8 <- leftLP_8$LP_average_x
leftLP_averagex_9 <- leftLP_9$LP_average_x
leftLP_averagex_10 <- leftLP_10$LP_average_x
leftLP_averagex_11 <- leftLP_11$LP_average_x
leftLP_averagex_12 <- leftLP_12$LP_average_x
leftLP_averagex_13 <- leftLP_13$LP_average_x
leftLP_averagex_14 <- leftLP_14$LP_average_x
leftLP_averagex_15 <- leftLP_15$LP_average_x
leftLP_averagex_16 <- leftLP_16$LP_average_x
leftLP_averagex_17 <- leftLP_17$LP_average_x
leftLP_averagex_18 <- leftLP_18$LP_average_x
leftLP_averagex_19 <- leftLP_19$LP_average_x
leftLP_averagex_20 <- leftLP_20$LP_average_x
leftLP_averagex_21 <- leftLP_21$LP_average_x
leftLP_averagex_22 <- leftLP_22$LP_average_x
leftLP_averagex_23 <- leftLP_23$LP_average_x
leftLP_averagex_24 <- leftLP_24$LP_average_x
leftLP_averagex_25 <- leftLP_25$LP_average_x
leftLP_averagex_26 <- leftLP_26$LP_average_x
leftLP_averagex_27 <- leftLP_27$LP_average_x
leftLP_averagex_28 <- leftLP_28$LP_average_x
leftLP_averagex_29 <- leftLP_29$LP_average_x
leftLP_averagex_30 <- leftLP_30$LP_average_x


leftLP_averagex <- c(leftLP_averagex_control, leftLP_averagex_1, leftLP_averagex_2, leftLP_averagex_3, leftLP_averagex_4, leftLP_averagex_5, leftLP_averagex_6, leftLP_averagex_7, leftLP_averagex_8, leftLP_averagex_9, leftLP_averagex_10, leftLP_averagex_11, leftLP_averagex_12, leftLP_averagex_13, leftLP_averagex_14, leftLP_averagex_15, leftLP_averagex_16, leftLP_averagex_17, leftLP_averagex_18, leftLP_averagex_19, leftLP_averagex_20, leftLP_averagex_21, leftLP_averagex_22, leftLP_averagex_23, leftLP_averagex_24, leftLP_averagex_25, leftLP_averagex_26, leftLP_averagex_27, leftLP_averagex_28, leftLP_averagex_29, leftLP_averagex_30)

#standard deviation
leftLP_xsd_control <- leftLP_control$LP_SD_x
leftLP_xsd_1 <- leftLP_1$LP_SD_x
leftLP_xsd_2 <- leftLP_2$LP_SD_x
leftLP_xsd_3 <- leftLP_3$LP_SD_x
leftLP_xsd_4 <- leftLP_4$LP_SD_x
leftLP_xsd_5 <- leftLP_5$LP_SD_x
leftLP_xsd_6 <- leftLP_6$LP_SD_x
leftLP_xsd_7 <- leftLP_7$LP_SD_x
leftLP_xsd_8 <- leftLP_8$LP_SD_x
leftLP_xsd_9 <- leftLP_9$LP_SD_x
leftLP_xsd_10 <- leftLP_10$LP_SD_x
leftLP_xsd_11 <- leftLP_11$LP_SD_x
leftLP_xsd_12 <- leftLP_12$LP_SD_x
leftLP_xsd_13 <- leftLP_13$LP_SD_x
leftLP_xsd_14 <- leftLP_14$LP_SD_x
leftLP_xsd_15 <- leftLP_15$LP_SD_x
leftLP_xsd_16 <- leftLP_16$LP_SD_x
leftLP_xsd_17 <- leftLP_17$LP_SD_x
leftLP_xsd_18 <- leftLP_18$LP_SD_x
leftLP_xsd_19 <- leftLP_19$LP_SD_x
leftLP_xsd_20 <- leftLP_20$LP_SD_x
leftLP_xsd_21 <- leftLP_21$LP_SD_x
leftLP_xsd_22 <- leftLP_22$LP_SD_x
leftLP_xsd_23 <- leftLP_23$LP_SD_x
leftLP_xsd_24 <- leftLP_24$LP_SD_x
leftLP_xsd_25 <- leftLP_25$LP_SD_x
leftLP_xsd_26 <- leftLP_26$LP_SD_x
leftLP_xsd_27 <- leftLP_27$LP_SD_x
leftLP_xsd_28 <- leftLP_28$LP_SD_x
leftLP_xsd_29 <- leftLP_29$LP_SD_x
leftLP_xsd_30 <- leftLP_30$LP_SD_x



leftLP_xsd <- c(leftLP_xsd_control, leftLP_xsd_1, leftLP_xsd_2, leftLP_xsd_3, leftLP_xsd_4, leftLP_xsd_5, leftLP_xsd_6, leftLP_xsd_7, leftLP_xsd_8, leftLP_xsd_9, leftLP_xsd_10, leftLP_xsd_11, leftLP_xsd_12, leftLP_xsd_13, leftLP_xsd_14, leftLP_xsd_15, leftLP_xsd_16, leftLP_xsd_17, leftLP_xsd_18, leftLP_xsd_19, leftLP_xsd_20, leftLP_xsd_21, leftLP_xsd_22, leftLP_xsd_23, leftLP_xsd_24, leftLP_xsd_25, leftLP_xsd_26, leftLP_xsd_27, leftLP_xsd_28, leftLP_xsd_29, leftLP_xsd_30)

leftLP_averagex_df <- data.frame(time_group_lt, leftLP_averagex, leftLP_xsd)
#adjust by subtracting control value to all data
leftLP_averagex_df$left_adj_avex <- leftLP_averagex_df$leftLP_averagex - leftLP_averagex_control

plot_averagex_left <- ggplot(leftLP_averagex_df, aes(x=time_group_lt, y=leftLP_averagex)) +
  geom_point(colour="red", size=3) +
  geom_errorbar(aes(ymin=leftLP_averagex-leftLP_xsd, ymax=leftLP_averagex+leftLP_xsd), colour="pink", width=0.2) +
  scale_x_continuous(name="Time Group", breaks=seq(0, 30,5)) +
  scale_y_continuous(name="Listing's Plane Average X", breaks=seq(-14, 14, by=1)) +
  geom_hline(yintercept=0, color="grey") +
  theme_classic() +
  ggtitle("left ear")
plot_averagex_left

plot_averagex_controladj_left <- ggplot(leftLP_averagex_df, aes(x=time_group_lt, y=left_adj_avex)) +
  geom_point(colour="red", size=3) +
  geom_errorbar(aes(ymin=left_adj_avex-leftLP_xsd, ymax=left_adj_avex+leftLP_xsd), colour="pink", width=0.2) +
  scale_x_continuous(name="Time Group", breaks=seq(0, 12,1)) +
  scale_y_continuous(name="Listing's Plane Average X", breaks=seq(-14, 14, by=1)) +
  geom_hline(yintercept=0, color="grey") +
  theme_classic()+
  ggtitle("left ear")
plot_averagex_controladj_left



#Both
plot_averagex_both <- ggplot() +
  geom_point(data=leftLP_averagex_df, aes(x=time_group_lt, y=leftLP_averagex), colour="blue", size=3) +
  geom_errorbar(data=leftLP_averagex_df, aes(x=time_group_lt, ymin=leftLP_averagex-leftLP_xsd, ymax=leftLP_averagex+leftLP_xsd), colour="lightblue", width=0.2) +
  geom_point(data=rightLP_averagex_df, aes(x=time_group_lt, y=rightLP_averagex), colour="red", size=3) +
  geom_errorbar(data=rightLP_averagex_df, aes(x=time_group_lt, ymin=rightLP_averagex-rightLP_xsd, ymax=rightLP_averagex+rightLP_xsd), colour="pink", width=0.2) +
  scale_x_continuous(name="Time Group", breaks=seq(0, 30,5)) +
  scale_y_continuous(name="Listing's Plane Average X", breaks=seq(-14, 14, by=1)) +
  geom_hline(yintercept=0, color="grey") +
  theme_classic()
plot_averagex_both

plot_averagex_both_controladj <- ggplot() +
  geom_point(data=leftLP_averagex_df, aes(x=time_group_lt, y=left_adj_avex), colour="blue", size=3) +
  geom_errorbar(data=leftLP_averagex_df, aes(x=time_group_lt, ymin=left_adj_avex-leftLP_xsd, ymax=left_adj_avex+leftLP_xsd), colour="lightblue", width=0.2) +
  geom_point(data=rightLP_averagex_df, aes(x=time_group_lt, y=right_adj_avex), colour="red", size=3) +
  geom_errorbar(data=rightLP_averagex_df, aes(x=time_group_lt, ymin=right_adj_avex-rightLP_xsd, ymax=right_adj_avex+rightLP_xsd), colour="pink", width=0.2) +
  scale_x_continuous(name="Time Group", breaks=seq(0, 30,1)) +
  scale_y_continuous(name="Listing's Plane Average X", breaks=seq(-14, 14, by=1)) +
  geom_hline(yintercept=0, color="grey") +
  theme_classic()
plot_averagex_both_controladj



output_list <- list(rightLP_averagex_df, leftLP_averagex_df)
write.xlsx(output_list, paste("LP_averagex_caloric_result_", id, "8.11.xlsx", sep=""), overwrite=TRUE)


pdf(paste("LP_averagex_bothcaloric_result_", id, "8.11.pdf", sep=""), width = 10, height = 5)
plot_averagex_right
plot_averagex_controladj_right
plot_averagex_left
plot_averagex_controladj_left
plot_averagex_both
plot_averagex_both_controladj
dev.off()