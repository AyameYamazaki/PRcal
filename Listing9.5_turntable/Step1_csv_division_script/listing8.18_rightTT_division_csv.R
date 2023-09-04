##set working directory **************************************************************************

rm(list = ls())

library(ggplot2)
library(tidyverse)
library(readxl)

data2 <- read.csv(file=file.choose()) #csvファイルを読み込む make sure it is a the right file! **********************

rotation_direction = "right"

time <- data2[,"Application.Time"]
headquatx <- data2[,"Headset.Orientation.Quaternion.x"]
headquaty <- data2[,"Headset.Orientation.Quaternion.y"]
headquatz <- data2[,"Headset.Orientation.Quaternion.z"]
headquatw <- data2[,"Headset.Orientation.Quaternion.w"]
headquat <- data.frame(time, headquatw, headquatx, headquaty, headquatz)

#scalar to degree
headquat$degree <- (2*acos(headquat$headquatw))*180/pi
#convert headquat to unit vector
headquat$unitx <- (headquat$headquatx/((1-headquat$headquatw^2)^0.5))
headquat$unity <- (headquat$headquaty/((1-headquat$headquatw^2)^0.5))
headquat$unitz <- (headquat$headquatz/((1-headquat$headquatw^2)^0.5))
#multiply by degrees
headquat$degx <- headquat$unitx * headquat$degree
headquat$degy <- headquat$unity * headquat$degree
headquat$degz <- headquat$unitz * headquat$degree

#convert to right handed
headdeg <- data.frame(headquat$time, -headquat$degz, headquat$degx, -headquat$degy)
colnames(headdeg) <- c("time", "degX", "degY", "degZ")

plot_degX <- ggplot(headdeg) +
  geom_point(aes(x=time, y=degX)) +
  labs(title = "roll head rotation", x = "time", y = "roll")
plot_degX

plot_degY <- ggplot(headdeg) +
  geom_point(aes(x=time, y=degY)) +
  labs(title = "pitch head rotation", x = "time", y = "pitch")
plot_degY

plot_degZ <- ggplot(headdeg) +
  geom_point(aes(x=time, y=degZ)) +
  labs(title = "yaw head rotation", x = "time", y = "yaw")
plot_degZ

plot_degZ_stop <- ggplot(headdeg) +
  geom_point(aes(x=time, y=degZ)) +
  labs(title = "yaw head rotation", x = "time", y = "yaw") +
  xlim(50, 70)
plot_degZ_stop

#detect when head rotation stops
headdeg_stop <- headdeg %>% dplyr::filter(time >58 & time < 70) #change time if changing window does not work

w=140 ### change window and threshold to detect the correct stop time (disk rotates at max 1.3deg/frame)
threshold=1

len=nrow(headdeg_stop)
degz_stop <- headdeg_stop$degZ
headdeg_stop$rotating<-rep(FALSE,len)
for(i in 1:(len-(2*w+2))){
  if(abs(mean(degz_stop[i:(i+w)])-mean(degz_stop[(i+w+1):(i+2*w+1)])) <threshold ) {
    headdeg_stop$rotating[i] <- TRUE
  }
}

headdeg_stopped <- headdeg_stop %>% dplyr::filter(headdeg_stop$rotating==TRUE)
stop_time <- min(headdeg_stopped$time)-1 #check stop time is correct on the plot *****************************************
extra_time <- stop_time-60



#divide the data into groups of 10sec before rotation stops
#if stop time is less than 60sec, create per-rotation 9pt data taht is slightly less than 10sec
#if stop time is more than 60sec, create per-rotation 9pt data that is slightly longer than 10sec

ninept_1 <- data2 %>% dplyr::filter(data2$Application.Time <10)
ninept_2 <- data2 %>% dplyr::filter(data2$Application.Time >=10 & data2$Application.Time <20)
ninept_3 <- data2 %>% dplyr::filter(data2$Application.Time >=20 & data2$Application.Time <30)
ninept_4 <- data2 %>% dplyr::filter(data2$Application.Time >=30 & data2$Application.Time <40)
ninept_5 <- data2 %>% dplyr::filter(data2$Application.Time >=40 & data2$Application.Time <50)
ninept_6 <- data2 %>% dplyr::filter(data2$Application.Time >=50 & data2$Application.Time <stop_time)
ninept_7 <- data2 %>% dplyr::filter(data2$Application.Time >=stop_time & data2$Application.Time <(70+extra_time))
ninept_8 <- data2 %>% dplyr::filter(data2$Application.Time >=(70+extra_time) & data2$Application.Time <(80+extra_time))
ninept_9 <- data2 %>% dplyr::filter(data2$Application.Time >(80+extra_time) & data2$Application.Time <(90+extra_time))
ninept_10 <- data2 %>% dplyr::filter(data2$Application.Time >=(90+extra_time) & data2$Application.Time <(100+extra_time))
ninept_11 <- data2 %>% dplyr::filter(data2$Application.Time >=(100+extra_time) & data2$Application.Time <(110+extra_time))
ninept_12 <- data2 %>% dplyr::filter(data2$Application.Time >=(110+extra_time) & data2$Application.Time <120)


#graph for chekcing nystagmus
ninept_6_time <- ninept_6[,"Application.Time"]
ninept_6_lefttor <- ninept_6[,"Eye.Torsion..degrees..left"]
ninept_6_righttor <- ninept_6[,"Eye.Torsion..degrees..right"]
ninept_6_leftx <- ninept_6[,"Eye.Ray.left.dir.x"]
ninept_6_rightx <- ninept_6[,"Eye.Ray.right.dir.x"]
ninept_6_df <- data.frame(ninept_6_time, ninept_6_leftx, ninept_6_rightx, ninept_6_lefttor, ninept_6_righttor)

plot_torsion_6 <- ggplot(data=ninept_6_df)+
  geom_line(aes(x=ninept_6_time, y=ninept_6_lefttor), colour="blue")+
  geom_line(aes(x=ninept_6_time, y=ninept_6_righttor), colour="red")
plot_torsion_6
plot_x_6 <- ggplot(data=ninept_6_df)+
  geom_line(aes(x=ninept_6_time, y=ninept_6_leftx), colour="blue")+
  geom_line(aes(x=ninept_6_time, y=ninept_6_rightx), colour="red")
plot_x_6

ninept_7_time <- ninept_7[,"Application.Time"]
ninept_7_lefttor <- ninept_7[,"Eye.Torsion..degrees..left"]
ninept_7_righttor <- ninept_7[,"Eye.Torsion..degrees..right"]
ninept_7_leftx <- ninept_7[,"Eye.Ray.left.dir.x"]
ninept_7_rightx <- ninept_7[,"Eye.Ray.right.dir.x"]
ninept_7_df <- data.frame(ninept_7_time, ninept_7_leftx, ninept_7_rightx, ninept_7_lefttor, ninept_7_righttor)

plot_torsion_7 <- ggplot(data=ninept_7_df)+
  geom_line(aes(x=ninept_7_time, y=ninept_7_lefttor), colour="blue")+
  geom_line(aes(x=ninept_7_time, y=ninept_7_righttor), colour="red")
plot_torsion_7
plot_x_7 <- ggplot(data=ninept_7_df)+
  geom_line(aes(x=ninept_7_time, y=ninept_7_leftx), colour="blue")+
  geom_line(aes(x=ninept_7_time, y=ninept_7_rightx), colour="red")
plot_x_7


write.csv(ninept_1, paste("turntable_9pt_", rotation_direction, "_1.csv", sep=""))
write.csv(ninept_2, paste("turntable_9pt_", rotation_direction, "_2.csv", sep=""))
write.csv(ninept_3, paste("turntable_9pt_", rotation_direction, "_3.csv", sep=""))
write.csv(ninept_4, paste("turntable_9pt_", rotation_direction, "_4.csv", sep=""))
write.csv(ninept_5, paste("turntable_9pt_", rotation_direction, "_5.csv", sep=""))
write.csv(ninept_6, paste("turntable_9pt_", rotation_direction, "_6.csv", sep=""))
write.csv(ninept_7, paste("turntable_9pt_", rotation_direction, "_7.csv", sep=""))
write.csv(ninept_8, paste("turntable_9pt_", rotation_direction, "_8.csv", sep=""))
write.csv(ninept_9, paste("turntable_9pt_", rotation_direction, "_9.csv", sep=""))
write.csv(ninept_10, paste("turntable_9pt_", rotation_direction, "_10.csv", sep=""))
write.csv(ninept_11, paste("turntable_9pt_", rotation_direction, "_11.csv", sep=""))
write.csv(ninept_12, paste("turntable_9pt_", rotation_direction, "_12.csv", sep=""))