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
#if stop time is less than pre0sec, create per-rotation 9pt data taht is slightly less than 10sec
#if stop time is more than pre0sec, create per-rotation 9pt data that is slightly longer than 10sec

onept_pre <- data2 %>% dplyr::filter(data2$Application.Time <stop_time)
onept_post <- data2 %>% dplyr::filter(data2$Application.Time >=stop_time & data2$Application.Time <120)



#graph for chekcing nystagmus
onept_pre_time <- onept_pre[,"Application.Time"]
onept_pre_lefttor <- onept_pre[,"Eye.Torsion..degrees..left"]
onept_pre_righttor <- onept_pre[,"Eye.Torsion..degrees..right"]
onept_pre_leftx <- onept_pre[,"Eye.Ray.left.dir.x"]
onept_pre_rightx <- onept_pre[,"Eye.Ray.right.dir.x"]
onept_pre_df <- data.frame(onept_pre_time, onept_pre_leftx, onept_pre_rightx, onept_pre_lefttor, onept_pre_righttor)

plot_torsion_pre <- ggplot(data=onept_pre_df)+
  geom_line(aes(x=onept_pre_time, y=onept_pre_lefttor), colour="blue")+
  geom_line(aes(x=onept_pre_time, y=onept_pre_righttor), colour="red")
plot_torsion_pre
plot_x_pre <- ggplot(data=onept_pre_df)+
  geom_line(aes(x=onept_pre_time, y=onept_pre_leftx), colour="blue")+
  geom_line(aes(x=onept_pre_time, y=onept_pre_rightx), colour="red")
plot_x_pre

onept_post_time <- onept_post[,"Application.Time"]
onept_post_lefttor <- onept_post[,"Eye.Torsion..degrees..left"]
onept_post_righttor <- onept_post[,"Eye.Torsion..degrees..right"]
onept_post_leftx <- onept_post[,"Eye.Ray.left.dir.x"]
onept_post_rightx <- onept_post[,"Eye.Ray.right.dir.x"]
onept_post_df <- data.frame(onept_post_time, onept_post_leftx, onept_post_rightx, onept_post_lefttor, onept_post_righttor)

plot_torsion_post <- ggplot(data=onept_post_df)+
  geom_line(aes(x=onept_post_time, y=onept_post_lefttor), colour="blue")+
  geom_line(aes(x=onept_post_time, y=onept_post_righttor), colour="red")
plot_torsion_post
plot_x_post <- ggplot(data=onept_post_df)+
  geom_line(aes(x=onept_post_time, y=onept_post_leftx), colour="blue")+
  geom_line(aes(x=onept_post_time, y=onept_post_rightx), colour="red")
plot_x_post


write.csv(onept_pre, paste("turntable_1pt_", rotation_direction, "_pre.csv", sep=""))
write.csv(onept_post, paste("turntable_1pt_", rotation_direction, "_post.csv", sep=""))

