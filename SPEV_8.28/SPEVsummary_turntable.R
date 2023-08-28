
rm(list = ls())

library(ggplot2)
library(tidyverse)
library(readxl)
library(openxlsx)

id = "20230209NA"
head = "60HU"


#torsion SPEV data frame
tor_ltper_df <- read.xlsx("turntable_result_SPEVtor_leftper.xlsx", sheet="top3_SPEVtor")
tor_ltpost_df <- read.xlsx("turntable_result_SPEVtor_leftpost.xlsx", sheet="top3_SPEVtor")
tor_rtper_df <- read.xlsx("turntable_result_SPEVtor_rightper.xlsx", sheet="top3_SPEVtor")
tor_rtpost_df <- read.xlsx("turntable_result_SPEVtor_rightpost.xlsx", sheet="top3_SPEVtor")

#Z SPEV data  frame
Z_ltper_df <- read.xlsx("turntable_result_SPEVZ_leftper.xlsx", sheet="top3_SPEVZ")
Z_ltpost_df <- read.xlsx("turntable_result_SPEVZ_leftpost.xlsx", sheet="top3_SPEVZ")
Z_rtper_df <- read.xlsx("turntable_result_SPEVZ_rightper.xlsx", sheet="top3_SPEVZ")
Z_rtpost_df <- read.xlsx("turntable_result_SPEVZ_rightpost.xlsx", sheet="top3_SPEVZ")

#Y SPEV data  frame
Y_ltper_df <- read.xlsx("turntable_result_SPEVY_leftper.xlsx", sheet="top3_SPEVY")
Y_ltpost_df <- read.xlsx("turntable_result_SPEVY_leftpost.xlsx", sheet="top3_SPEVY")
Y_rtper_df <- read.xlsx("turntable_result_SPEVY_rightper.xlsx", sheet="top3_SPEVY")
Y_rtpost_df <- read.xlsx("turntable_result_SPEVY_rightpost.xlsx", sheet="top3_SPEVY")


#torsion SPEV data
tor_ltper <- mean(tor_ltper_df$SPEVtor.sorted.tor, na.rm=TRUE)
tor_ltpost <- mean(tor_ltpost_df$SPEVtor.sorted.tor, na.rm=TRUE)
tor_rtper <- mean(tor_rtper_df$SPEVtor.sorted.tor, na.rm=TRUE)
tor_rtpost <- mean(tor_rtpost_df$SPEVtor.sorted.tor, na.rm=TRUE)
tor_df <- data.frame(tor_ltper, tor_ltpost, tor_rtper, tor_rtpost)
tor_df <- t(tor_df)
colnames(tor_df) <- "torsion"

#Z SPEV data
#Z_ltper <- mean(Z_ltper_df$SPEVZ.sorted.Z, na.rm=TRUE)
Z_ltper <- 0
Z_ltpost <- mean(Z_ltpost_df$SPEVZ.sorted.Z, na.rm=TRUE)
Z_rtper <- mean(Z_rtper_df$SPEVZ.sorted.Z, na.rm=TRUE)
Z_rtpost <- mean(Z_rtpost_df$SPEVZ.sorted.Z, na.rm=TRUE)
Z_df <- data.frame(Z_ltper, Z_ltpost, Z_rtper, Z_rtpost)
Z_df <- t(Z_df)
colnames(Z_df) <- "Z"

#Y SPEV data
#Y_ltper <- mean(Y_ltper_df$SPEVY.sorted.Y, na.rm=TRUE)
Y_ltper <- 0
#Y_ltpost <- mean(Y_ltpost_df$SPEVY.sorted.Y, na.rm=TRUE)
Y_ltpost <- 0
#Y_rtper <- mean(Y_rtper_df$SPEVY.sorted.Y, na.rm=TRUE)
Y_rtper <- 0
#Y_rtpost <- mean(Y_rtpost_df$SPEVY.sorted.Y, na.rm=TRUE)
Y_rtpost <- 0
Y_df <- data.frame(Y_ltper, Y_ltpost, Y_rtper, Y_rtpost)
Y_df <- t(Y_df)
colnames(Y_df) <- "Y"

df_summary <- data.frame(tor_df, Z_df, Y_df)
rownames(df_summary) <- c("ltper", "ltpost", "rtper", "rtpost")
view(df_summary)

write.xlsx(df_summary, paste("SPEV_summary_result_", id, head, ".xlsx", sep=""), rowNames=TRUE)