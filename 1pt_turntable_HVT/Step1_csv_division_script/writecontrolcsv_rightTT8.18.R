

##set working directory **************************************************************************

rm(list = ls())

control_data <- read.csv(file=file.choose()) #csvファイルを読み込む make sure it is a control file! **********************

rotation_direction = "right"

write.csv(control_data, paste("turntable_1pt_", rotation_direction, "_control.csv", sep=""))