

##set working directory **************************************************************************

rm(list = ls())

control_data <- read.csv(file=file.choose()) #csvファイルを読み込む make sure it is a control file! **********************

ear = "right"

write.csv(control_data, paste("caloric_9pt_", ear, "_control.csv", sep=""))