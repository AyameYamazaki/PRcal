
rm(list = ls())

library(ggplot2)
library(tidyverse)
library(readxl)
library(openxlsx)

id = "20220720YH"


#torsion SPEV data frame
tor_left_df <- read.xlsx("caloric_result_SPEVtor_left.xlsx", sheet="top3_SPEVtor")
tor_right_df <- read.xlsx("caloric_result_SPEVtor_right.xlsx", sheet="top3_SPEVtor")

#Z SPEV data  frame
Z_left_df <- read.xlsx("caloric_result_SPEVZ_left.xlsx", sheet="top3_SPEVZ")
Z_right_df <- read.xlsx("caloric_result_SPEVZ_right.xlsx", sheet="top3_SPEVZ")

#Y SPEV data  frame
Y_left_df <- read.xlsx("caloric_result_SPEVY_left.xlsx", sheet="top3_SPEVY")
Y_right_df <- read.xlsx("caloric_result_SPEVY_right.xlsx", sheet="top3_SPEVY")


#torsion SPEV data
tor_left <- mean(tor_left_df$SPEVtor.sorted.tor, na.rm=TRUE)
tor_right <- mean(tor_right_df$SPEVtor.sorted.tor, na.rm=TRUE)
tor_df <- data.frame(tor_left, tor_right)
tor_df <- t(tor_df)
colnames(tor_df) <- "torsion"

#Z SPEV data
Z_left <- mean(Z_left_df$SPEVZ.sorted.Z, na.rm=TRUE)
Z_right <- mean(Z_right_df$SPEVZ.sorted.Z, na.rm=TRUE)
Z_df <- data.frame(Z_left, Z_right)
Z_df <- t(Z_df)
colnames(Z_df) <- "Z"

#Y SPEV data
Y_left <- mean(Y_left_df$SPEVY.sorted.Y, na.rm=TRUE)
#Y_left <- 0
Y_right <- mean(Y_right_df$SPEVY.sorted.Y, na.rm=TRUE)
#Y_right <- 0
Y_df <- data.frame(Y_left, Y_right)
Y_df <- t(Y_df)
colnames(Y_df) <- "Y"

#CP
CP <- (abs(Z_left)-abs(Z_right))/(abs(Z_left)+abs(Z_right))*100
CP_side <- ""
if(abs(Z_left)<abs(Z_right)){
  CP_side <- "left"
}
if(abs(Z_right)<abs(Z_left)){
  CP_side <- "right"
}
view(CP)
CP_df <- data.frame(CP, CP_side)

df_summary <- data.frame(tor_df, Z_df, Y_df)
rownames(df_summary) <- c("left", "right")
view(df_summary)


output_list <- list("summary"=df_summary, "CP"=CP_df)
write.xlsx(output_list, paste("SPEV_summary_result_caloric", id, ".xlsx", sep=""), rowNames=TRUE, overwrite=TRUE)

