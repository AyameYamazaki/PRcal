##make sure set working directory is correct***********************************************************************


rm(list = ls())


library(ggplot2)
library(tidyverse)
library(readxl)
library(RcppRoll)
library(scatterplot3d)
library(forecast)
library(openxlsx)
library(zoo)



#change the below parameters according to the data file!!!!!!!!!!!!!!
which_side = "left"
fileno = 1
eye = "left"


#do not change SD below 2.5
band_sd = 5


#change the parameters accordingly***********************************************************************
paste("turntable_", "9pt_", which_side, "_", fileno, ".csv", sep="")

data <- read.csv(paste("turntable_", "9pt_", which_side, "_", fileno, ".csv", sep="")) 

righteye.data <- data %>% dplyr::select("Application.Time", "Eye.Ray.right.dir.x", "Eye.Ray.right.dir.y", "Eye.Ray.right.dir.z", "Eye.Torsion..degrees..right", "Eye.State.right")
lefteye.data <- data %>% dplyr::select("Application.Time", "Eye.Ray.left.dir.x", "Eye.Ray.left.dir.y", "Eye.Ray.left.dir.z", "Eye.Torsion..degrees..left", "Eye.State.left")

if(eye=="right"){
  pre_fick <- righteye.data
}else{
  if(eye=="left"){
    pre_fick <- lefteye.data
}else{
    print("ERROR")
  }
}

time <- pre_fick[,1]
eyerayx <- pre_fick[,2]
eyerayy <- pre_fick[,3]
eyerayz <- pre_fick[,4]
torsion <- pre_fick[,5]
blink <- pre_fick[,6]


# eye ray x noise removal do not change
k_erx=2 # threshold = k_erx * SD 
w_erx=30  # window size = w*2+1 
l=nrow(pre_fick) 

df_erx <- data.frame(time, eyerayx, blink)
df_erx$presmooth_erx <- as.numeric(df_erx$eyerayx)
df_erx$outlier_erx<-rep(FALSE,l)
df_erx$blinktrue_erx <- rep(FALSE,l)
for(i in 1:l){
  if(df_erx$blink[i]=="Closed"){
    df_erx$blinktrue_erx[i] <- TRUE#if eye state is "Closed", input TRUE into the df_erx$blinktrue_erx
  }
}

if(l>(w_erx*2+1)){
  erx=df_erx$presmooth_erx
  for(t in (w_erx+1):(l-w_erx)){
    if(df_erx$blinktrue_erx[t]==TRUE){
      df_erx$presmooth_erx[(t-4):(t+10)] <- NaN
      df_erx$presmooth_erx[t] <- NaN
    }
  }
  
  if(erx[(t-1)]-erx[(t)]-erx[(t+1)]==0){#replace with previous data if there are three same consecutive numbers
    df_erx$presmooth_erx[(t-1):(t+1)] <- NaN
  }
}

if(l>(w_erx*2+1)){
  for(t in (w_erx+1):(l-w_erx)){
    if(any(is.na(df_erx$presmooth_erx[(t-w_erx):(t+w_erx)]))){
      next
    }
    Ut_erx<-erx[(t-w_erx):(t+w_erx)]#eye ray x within the window
        if(abs(erx[t]-mean(Ut_erx))>k_erx*sd(Ut_erx)) { #detect outlier if more than k_erx*standard deviation away from mean
      df_erx$outlier_erx[t] <- TRUE
      df_erx$presmooth_erx[t] <- mean(Ut_erx)　#replace outlier with mean
    }
  }
}


erxNR <- df_erx$presmooth_erx
erxNR_blinktrue <- df_erx$blinktrue_erx
#create eye ray x df with blink omitted
df_eyerayx <- data.frame(time, eyerayx, erxNR, erxNR_blinktrue)

#correct the value from first window
first_x_mean <- mean(df_erx$eyerayx[1:(w_erx*2+1)], na.rm=TRUE)
first_x_sd <- k_erx*sd(df_erx$eyerayx[1:(w_erx*2+1)], na.rm=TRUE)
for(i in 1:(w_erx*2+1)){
  if(is.na(df_erx$eyerayx[i])){
    next
  }
  if(df_erx$eyerayx[i]<first_x_mean+first_x_sd & df_erx$eyerayx[i]>first_x_mean-first_x_sd){
    df_eyerayx$erxNR[i] <- df_eyerayx$eyerayx[i]
  }else{
    df_eyerayx$erxNR[i] <- first_x_mean
  }
}
for(i in 2:(w_erx*2+1)){
  if(erx[(i-1)]-erx[(i)]-erx[(i+1)]==0){#replace with NaN if there are three same consecutive numbers
    df_eyerayx$erxNR[(i-1):(i+1)] <- NaN
  }
  if(df_eyerayx$erxNR_blinktrue[i]==TRUE){
    df_eyerayx$erxNR[(i-1):(i+1)] <- NaN
  }
}


#correct the value from last window
last_x_mean <- mean(df_erx$eyerayx[(l-w_erx*2):l], na.rm=TRUE)
last_x_sd <- k_erx*sd(df_erx$eyerayx[(l-w_erx*2):l], na.rm=TRUE)
for(i in (l-w_erx*2):l){
    if(is.na(df_erx$eyerayx[i])){
    next
  }
  if(df_erx$eyerayx[i]<(last_x_mean+last_x_sd) & df_erx$eyerayx[i]>(last_x_mean-last_x_sd)){
    df_eyerayx$erxNR[i] <- df_eyerayx$eyerayx[i]
  }else{
    df_eyerayx$erxNR[i] <- last_x_mean
  }
}
for(i in (l-w_erx*2):(l-1)){
  if(erx[(i-1)]-erx[(i)]-erx[(i+1)]==0){#replace with NaN if there are three same consecutive numbers
    df_eyerayx$erxNR[(i-1):(i+1)] <- NaN
  }
  if(df_eyerayx$erxNR_blinktrue[i]==TRUE){
    df_eyerayx$erxNR[(i-1):(i+1)] <- NaN
  }
}

#Passband noise removal only for particularly noisy data
df_eyerayx$erxNR_PB <- if_else(df_eyerayx$erxNR>-0.5&df_eyerayx$erxNR<0.5, df_eyerayx$erxNR, NaN)

#plot the original eye ray x in blue and the final noise removed eye ray x in red
plot_eyerayx <- ggplot() +
  geom_line(data=df_eyerayx, aes(x=time, y = eyerayx), color = "blue", linewidth = 0.2) +
  geom_line(data=df_eyerayx, aes(x=time, y = erxNR_PB), color = "red", linewidth = 0.2)+
    scale_x_continuous(breaks = seq(0,300,10))+
  ylim(-0.5, 0.5)
plot_eyerayx



# eye ray y noise removal do not change
k_ery=2 # threshold = k_ery * SD 
w_ery=30  # window size = w*2+1 
l=nrow(pre_fick)

df_ery <- data.frame(time, eyerayy, blink)
df_ery$presmooth_ery <- as.numeric(df_ery$eyerayy)
df_ery$outlier_ery<-rep(FALSE,l)
df_ery$blinktrue_ery <- rep(FALSE,l)
for(i in 1:l){
  if(df_ery$blink[i]=="Closed"){
    df_ery$blinktrue_ery[i] <- TRUE
  }
}

if(l>(w_ery*2+1)){
  ery=df_ery$presmooth_ery
  for(t in (w_ery+1):(l-w_ery)){
      if(df_ery$blinktrue_ery[t]==TRUE){
        df_ery$presmooth_ery[(t-4):(t+10)] <- NaN
      df_ery$presmooth_ery[t] <- NaN
      }
  }
    
    if(ery[(t-1)]-ery[(t)]-ery[(t+1)]==0){
      df_ery$presmooth_ery[(t-1):(t+1)] <- NaN
    }
}

if(l>(w_ery*2+1)){
  for(t in (w_ery+1):(l-w_ery)){
    if(any(is.na(df_ery$presmooth_ery[(t-w_ery):(t+w_ery)]))){
      next
    }
    Ut_ery<-ery[(t-w_ery):(t+w_ery)]
        if(abs(ery[t]-mean(Ut_ery))>k_ery*sd(Ut_ery)) { 
      df_ery$outlier_ery[t] <- TRUE 
      df_ery$presmooth_ery[t] <- mean(Ut_ery)
    }
    }
  }

eryNR <- df_ery$presmooth_ery
eryNR_blinktrue <- df_ery$blinktrue_ery
#create eye ray y df with blink omitted
df_eyerayy <- data.frame(time, eyerayy, eryNR, eryNR_blinktrue)

#correct the value from first window
first_y_mean <- mean(df_ery$eyerayy[1:(w_ery*2+1)], na.rm=TRUE)
first_y_sd <- k_ery*sd(df_ery$eyerayy[1:(w_ery*2+1)], na.rm=TRUE)
for(i in 1:(w_ery*2+1)){
  if(is.na(df_ery$eyerayy[i])){
    next
  }
  if(df_ery$eyerayy[i]<first_y_mean+first_y_sd & df_ery$eyerayy[i]>first_y_mean-first_y_sd){
    df_eyerayy$eryNR[i] <- df_eyerayy$eyerayy[i]
  }else{
    df_eyerayy$eryNR[i] <- first_y_mean
  }
}
for(i in 2:(w_ery*2+1)){
  if(ery[(i-1)]-ery[(i)]-ery[(i+1)]==0){#replace with NaN if there are three same consecutive numbers
    df_eyerayy$eryNR[(i-1):(i+1)] <- NaN
  }
  if(df_eyerayy$eryNR_blinktrue[i]==TRUE){
    df_eyerayy$eryNR[(i-1):(i+1)] <- NaN
  }
}


#correct the value from last window
last_y_mean <- mean(df_ery$eyerayy[(l-w_ery*2):l], na.rm=TRUE)
last_y_sd <- k_ery*sd(df_ery$eyerayy[(l-w_ery*2):l], na.rm=TRUE)
for(i in (l-w_ery*2):l){
  if(is.na(df_ery$eyerayy[i])){
    next
  }
  if(df_ery$eyerayy[i]<(last_y_mean+last_y_sd) & df_ery$eyerayy[i]>(last_y_mean-last_y_sd)){
    df_eyerayy$eryNR[i] <- df_eyerayy$eyerayy[i]
  }else{
    df_eyerayy$eryNR[i] <- last_y_mean
  }
}
for(i in (l-w_ery*2):(l-1)){
  if(ery[(i-1)]-ery[(i)]-ery[(i+1)]==0){#replace with NaN if there are three same consecutive numbers
    df_eyerayy$eryNR[(i-1):(i+1)] <- NaN
  }
  if(df_eyerayy$eryNR_blinktrue[i]==TRUE){
    df_eyerayy$eryNR[(i-1):(i+1)] <- NaN
  }
}

#Passband noise removal only for particularly noisy data
df_eyerayy$eryNR_PB <- if_else(df_eyerayy$eryNR>-0.3&df_eyerayy$eryNR<0.3, df_eyerayy$eryNR, NaN)

#plot the original eye ray y in blue and the final noise removed eye ray y in red
plot_eyerayy <- ggplot() +
  geom_line(data=df_eyerayy, aes(x=time, y = eyerayy), color = "blue", linewidth = 0.2) +
  geom_line(data=df_eyerayy, aes(x=time, y = eryNR_PB), color = "red", linewidth = 0.2)+
  scale_x_continuous(breaks = seq(0,300,10))+
  ylim(-0.25, 0.25)
plot_eyerayy




# eye ray z noise removal do not change
k_erz=2 # threshold = k_erz * SD 
w_erz=30  # window size = w*2+1 
l=nrow(pre_fick)

df_erz <- data.frame(time, eyerayz, blink)
df_erz$presmooth_erz <- as.numeric(df_erz$eyerayz)
df_erz$outlier_erz<-rep(FALSE,l)
df_erz$blinktrue_erz <- rep(FALSE,l)
for(i in 1:l){
  if(df_erz$blink[i]=="Closed"){
    df_erz$blinktrue_erz[i] <- TRUE#if eye state is "Closed", input TRUE into the df_erz$blinktrue_erz
  }
}

if(l>(w_erz*2+1)){
  erz=df_erz$presmooth_erz
  for(t in (w_erz+1):(l-w_erz)){
    if(df_erz$blinktrue_erz[t]==TRUE){
      df_erz$presmooth_erz[(t-4):(t+10)] <- NaN
      df_erz$presmooth_erz[t] <- NaN
    }
    }
    
    if(erz[(t-1)]-erz[(t)]-erz[(t+1)]==0){#replace with previous data if there are three same consecutive numbers 
      df_erz$presmooth_erz[(t-1):(t+1)] <- NaN
    }
  }

if(l>(w_erz*2+1)){
  for(t in (w_erz+1):(l-w_erz)){
    if(any(is.na(df_erz$presmooth_erz[(t-w_erz):(t+w_erz)]))){
      next
    }
    Ut_erz<-erz[(t-w_erz):(t+w_erz)]
    if(abs(erz[t]-mean(Ut_erz))>k_erz*sd(Ut_erz)) { 
      df_erz$outlier_erz[t] <- TRUE 
        df_erz$presmooth_erz[t] <- mean(Ut_erz)
    }
  }
}


erzNR <- df_erz$presmooth_erz
erzNR_blinktrue <- df_erz$blinktrue_erz
#create eye ray z df with blink omitted
df_eyerayz <- data.frame(time, eyerayz, erzNR, erzNR_blinktrue)

#correct the value from first window
first_z_mean <- mean(df_erz$eyerayz[1:(w_erz*2+1)], na.rm=TRUE)
first_z_sd <- k_erz*sd(df_erz$eyerayz[1:(w_erz*2+1)], na.rm=TRUE)
for(i in 1:(w_erz*2+1)){
  if(is.na(df_erz$eyerayz[i])){
    next
  }
  if(df_erz$eyerayz[i]<first_z_mean+first_z_sd & df_erz$eyerayz[i]>first_z_mean-first_z_sd){
    df_eyerayz$erzNR[i] <- df_eyerayz$eyerayz[i]
  }else{
    df_eyerayz$erzNR[i] <- first_z_mean
  }
}
for(i in 2:(w_erz*2+1)){
  if(erz[(i-1)]-erz[(i)]-erz[(i+1)]==0){#replace with NaN if there are three same consecutive numbers
    df_eyerayz$erzNR[(i-1):(i+1)] <- NaN
    }
    if(df_eyerayz$erzNR_blinktrue[i]==TRUE){
      df_eyerayz$erzNR[(i-1):(i+1)] <- NaN
    }
  }

#correct the value from last window
last_z_mean <- mean(df_erz$eyerayz[(l-w_erz*2):l], na.rm=TRUE)
last_z_sd <- k_erz*sd(df_erz$eyerayz[(l-w_erz*2):l], na.rm=TRUE)
for(i in (l-w_erz*2):l){
  if(is.na(df_erz$eyerayz[i])){
    next
  }
  if(df_erz$eyerayz[i]<(last_z_mean+last_z_sd) & df_erz$eyerayz[i]>(last_z_mean-last_z_sd)){
    df_eyerayz$erzNR[i] <- df_eyerayz$eyerayz[i]
  }else{
    df_eyerayz$erzNR[i] <- last_z_mean
  }
}
for(i in (l-w_erz*2):(l-1)){
  if(erz[(i-1)]-erz[(i)]-erz[(i+1)]==0){#replace with NaN if there are three same consecutive numbers
    df_eyerayz$erzNR[(i-1):(i+1)] <- NaN
  }
  if(df_eyerayz$erzNR_blinktrue[i]==TRUE){
    df_eyerayz$erzNR[(i-1):(i+1)] <- NaN
  }
}


#plot the original eye ray z in blue and the final noise removed eye ray z in red
plot_eyerayz <- ggplot() +
  geom_line(data=df_eyerayz, aes(x=time, y = eyerayz), color = "blue", linewidth = 0.2) +
  geom_line(data=df_eyerayz, aes(x=time, y = erzNR), color = "red", linewidth = 0.2)+
  scale_x_continuous(breaks = seq(0,300,10))+
  ylim(0.9, 1.1)
plot_eyerayz





# torsion noise removal do not change
k_tor=2 # threshold = k_tor * SD 
w_tor=30  # window size = w*2+1 
highpass = mean(torsion, na.rm=TRUE) + band_sd*sd(torsion, na.rm=TRUE) 
lowpass = mean(torsion, na.rm=TRUE) - band_sd*sd(torsion, na.rm=TRUE)

df_tor <- data.frame(time, torsion, blink)
df_tor$presmooth_tor <- as.numeric(df_tor$torsion)
df_tor$outlier_tor<-rep(FALSE,l)
df_tor$blinktrue_tor <- rep(FALSE,l)
for(i in 1:l){
  if(df_tor$blink[i]=="Closed"){
    df_tor$blinktrue_tor[i] <- TRUE#if eye state is "Closed", input TRUE into the df_tor$blinktrue_tor
  }
}

if(l>(w_tor*2+1)){
  tor=df_tor$presmooth_tor
  for(t in (w_tor+1):(l-w_tor)){
    if(df_tor$blinktrue_tor[t]==TRUE){
      df_tor$presmooth_tor[(t-4):(t+10)] <- NaN
       df_tor$presmooth_tor[t] <- NaN
    }
  }
  if(tor[(t-1)]-tor[(t)]-tor[(t+1)]==0){#replace with NaN if there are three same consecutive numbers
    df_tor$presmooth_tor[(t-1):(t+1)] <- NaN
  }
}

if(l>(w_tor*2+1)){
  for(t in (w_tor+1):(l-w_tor)){
        if(any(is.na(df_tor$presmooth_tor[(t-w_tor):(t+w_tor)]))){
      next
    }
    Ut_tor<-tor[(t-w_tor):(t+w_tor)]#torsion within the window
    if(abs(tor[t]-mean(Ut_tor))>k_tor*sd(Ut_tor)) { 
      df_tor$outlier_tor[t] <- TRUE 
        df_tor$presmooth_tor[t] <- mean(Ut_tor)　
    }

  }
}



torNR <- df_tor$presmooth_tor
torNR_blinktrue <- df_tor$blinktrue_tor
#create df with blink omitted
df_torsion <- data.frame(time, torsion, torNR, torNR_blinktrue)

#correct the value from first window
first_tor_mean <- mean(df_torsion$torsion[1:(w_tor*2+1)], na.rm=TRUE)
first_tor_sd <- k_tor*sd(df_torsion$torsion[1:(w_tor*2+1)], na.rm=TRUE)
for(i in 1:(w_tor*2+1)){
  if(is.na(df_tor$torsion[i])){
    next
  }
  if(df_torsion$torsion[i]<first_tor_mean+first_tor_sd & df_tor$torsion[i]>first_tor_mean-first_tor_sd){
    df_torsion$torNR[i] <- df_torsion$torsion[i]
  }else{
    df_torsion$torNR[i] <- first_tor_mean
  }
}
for(i in 2:(w_tor*2+1)){
  if(tor[(i-1)]-tor[(i)]-tor[(i+1)]==0){#replace with NaN if there are three same consecutive numbers
    df_torsion$torNR[(i-1):(i+1)] <- NaN
  }
  if(df_torsion$torNR_blinktrue[i]==TRUE){
    df_torsion$torNR[(i-1):(i+1)] <- NaN
  }
}


#correct the value from last window
last_tor_mean <- mean(df_torsion$torsion[(l-w_tor*2):l], na.rm=TRUE)
last_tor_sd <- k_tor*sd(df_torsion$torsion[(l-w_tor*2):l], na.rm=TRUE)
for(i in (l-w_tor*2):l){
  if(is.na(df_tor$torsion[i])){
    next
  }
  if(df_torsion$torsion[i]<(last_tor_mean+last_tor_sd) & df_torsion$torsion[i]>(last_tor_mean-last_tor_sd)){
    df_torsion$torNR[i] <- df_torsion$torsion[i]
  }else{
    df_torsion$torNR[i] <- last_tor_mean
  }
}
for(i in (l-w_tor*2):(l-1)){
  if(tor[(i-1)]-tor[(i)]-tor[(i+1)]==0){#replace with NaN if there are three same consecutive numbers
    df_torsion$torNR[(i-1):(i+1)] <- NaN
  }
  if(df_torsion$torNR_blinktrue[i]==TRUE){
    df_torsion$torNR[(i-1):(i+1)] <- NaN
  }
}

#Passband noise removal only for particularly noisy data
df_torsion$torNR_PB <- if_else(df_torsion$torNR>lowpass&df_torsion$torNR<highpass, df_torsion$torNR, NaN)


#plot the original torsion in blue and the final noise removed torsion in red
plot_torsion <- ggplot() +
  geom_line(data=df_torsion, aes(x=time, y = torsion), color = "blue", linewidth = 0.2) +
  geom_line(data=df_torsion, aes(x=time, y = torNR_PB), color = "red", linewidth = 0.2) +
  ylim(-20, 20)
plot_torsion




























# function that converts degrees to radian
radian <- function(x){
  rad <-  x * pi / 180
  return(rad)
}

#dataframe for fick's coordinate
raw_data <- data.frame(time, eyerayx, eyerayy, eyerayz, torsion)
names(raw_data) <- c("time", "eyerayx", "eyerayy", "eyerayz", "torsion")
fick <- data.frame(df_eyerayx$time, df_eyerayx$erxNR, df_eyerayy$eryNR, df_eyerayz$erzNR, torsion_rad = radian(df_torsion$torNR_PB))
names(fick) <- c("time", "serxNR", "seryNR", "serzNR", "torsion_rad")

fick <- fick %>%
  mutate(Y = if_else(seryNR < 0,
                     acos(sqrt(serxNR^2 + serzNR^2)),
                     -acos(sqrt(serxNR^2 + serzNR^2))
                     )
         ) %>%
  mutate(Z = -asin(serxNR/sqrt(serxNR^2 + serzNR^2))) %>%
  mutate(dyx = -(1 + 1/((tan(Z))^2)*serxNR/seryNR)) %>% 
  mutate(X = if_else(serxNR*seryNR==0,
                     torsion_rad,
                     if_else(serxNR*seryNR>0,
                             torsion_rad + (pi/2 + atan2(dyx,1)),
                             torsion_rad - (pi/2 - atan2(dyx,1))
                     )
  )
  )

# convert to intrinsic rotation
rotation <- fick %>% 
  dplyr::select(time, X, Y, Z) %>% 
  rename(Θx = X, Θy = Y, Θz = Z) %>% 
  mutate(QZscalar = cos(Θz/2), QZx = 0, QZy = 0, QZz = -sin(Θz/2),
         RZscalar = cos(Θz/2), RZx = 0, RZy = 0, RZz = sin(Θz/2)) %>% 
  mutate(QYscalar = cos(Θy/2), QYx = sin(Θz)*sin(Θy/2), QYy = -cos(Θz)*sin(Θy/2), QYz = 0,
         RYscalar = QYscalar, RYx = -QYx, RYy = -QYy, RYz = 0) %>% 
  mutate(QXscalar = cos(Θx/2), QXx = -cos(Θy)*cos(Θz)*sin(Θx/2), QXy = -cos(Θy)*sin(Θz)*sin(Θx/2), QXz = sin(Θy)*sin(Θx/2),
         RXscalar = QXscalar, RXx = -QXx, RXy = -QXy, RXz = -QXz) %>%  #Ry x Rz
  mutate(RyRz_scalar = RYscalar*RZscalar - RYx*RZx - RYy*RZy - RYz*RZz,
         x = RYscalar*RZx + RZscalar*RYx + RYy*RZz - RYz*RZy,
         y = RYscalar*RZy + RZscalar*RYy + RYz*RZx - RZz*RYx,
         z = RYscalar*RZz + RZscalar*RYz + RYx*RZy + RZx*RYy) %>%  #Rx*Ry*Rz
  mutate(RxRyRz_scalar = RXscalar*RyRz_scalar - RXx*x - RXy*y - RXz*z,
         x1 = RXscalar*x + RyRz_scalar*RXx + RXy*z - RXz*y,
         y1 = RXscalar*y + RyRz_scalar*RXy + RXz*x - RXx*z,
         z1 = RXscalar*z + RyRz_scalar*RXz + RXx*y - RXy*x) %>%  #axis rotation
  mutate(Θ = 2*acos(RxRyRz_scalar),
         x2 = x1/(1-RxRyRz_scalar^2)^0.5,
         y2 = y1/(1-RxRyRz_scalar^2)^0.5,
         z2 = z1/(1-RxRyRz_scalar^2)^0.5,
         "x^2+y^2+z^2" = x2^2 + y2^2 + z2^2) %>% #rotational vector in degrees
  mutate(x3 = if_else(Θ == 0,
                      0,
                      Θ*180/pi*x2),
         y3 = if_else(Θ == 0,
                      0,
                      Θ*180/pi*y2),
         z3 = if_else(Θ == 0,
                      0,
                      Θ*180/pi*z2)) 


##regression plane from x=-ay-bz-c, from 
dp_threshold = 10 # do not change

lm <- lm(x3 ~ y3 + z3, rotation, na.action=na.omit) 
inv_l_DP <- c(lm$coefficients)
# since it is x=-ay-bz-c, reverse to minus
l_DP <- -(inv_l_DP)
names(l_DP) <- c("c", "a", "b")

#removing outliers on three dimensional plane (inactivated by large dp threshold)
rotation$x3_estimated = -l_DP["a"]*rotation$y3 - l_DP["b"]*rotation$z3 - l_DP["c"]
rotation$x3_distance = abs(rotation$x3 - rotation$x3_estimated)


rotation_threshold = dp_threshold*sd(rotation$x3, na.rm=TRUE) 
rotation$outlier = rotation$x3_distance > rotation_threshold
rotation_outremoved <- dplyr::filter(rotation, rotation$outlier == FALSE)
df_centroid <- data.frame(rotation_outremoved$x3, rotation_outremoved$y3, rotation_outremoved$z3)

centroid <- colMeans(df_centroid)
# Create the 3D scatterplot
s3d <- scatterplot3d(rotation$x3, rotation$y3,rotation$z3, pch=20, color = "blue", type="p", lty.hplot=2)
s3d$points3d(rotation_outremoved$x3, rotation_outremoved$y3, rotation_outremoved$z3, col="green", pch=20)


#vector perpendicular to the plane is (1, a, b). Therefore divide by square root of (1+a^2+b^2) to obtain normalized unit vector.
RVectorX <- 1/sqrt(1+l_DP[2]^2+l_DP[3]^2)
RVectorY <- l_DP[2]/sqrt(1+l_DP[2]^2+l_DP[3]^2)
RVectorZ <- l_DP[3]/sqrt(1+l_DP[2]^2+l_DP[3]^2)
normal_vector <- c(RVectorX, RVectorY, RVectorZ)
s3d$points3d(rbind(centroid, centroid + normal_vector), type = "l", col = "red", lwd = 2)

#calculate arctanz/x as elevation of displacement plane
ArctanZX <- atan2(RVectorZ, RVectorX)
deg_atanzx <- 180/pi*ArctanZX

RVector <- data.frame(RVectorX, RVectorY, RVectorZ, deg_atanzx)

#Primary position vector
PP_x <- 2*RVectorX^2-1
PP_y <- (2*RVectorX*RVectorY*((1-RVectorX^2)^0.5)/(RVectorY^2+RVectorZ^2)^0.5)
PP_z <- (2*RVectorX*RVectorZ*((1-RVectorX^2)^0.5)/(RVectorY^2+RVectorZ^2)^0.5)

Primary_position <- data.frame(PP_x, PP_y, PP_z)

#2d plots of displacement planes
yx <- ggplot()+
  geom_point(data=rotation, aes(x =y3, y = x3), color="blue", size=0.5)+
  geom_point(data=rotation_outremoved, aes(x =y3, y = x3), color="green", size=0.5)+
  labs(title = "DP_yx", x = "y", y = "x")+
  xlim(-20, 20) +
  ylim(-20, 20)
yz <- ggplot()+
  geom_point(data=rotation, aes(x =y3, y = z3), color="blue", size=0.5)+
  geom_point(data=rotation_outremoved, aes(x =y3, y = z3), color="green", size=0.5)+
  labs(title = "DP_yz", x = "y", y = "z")+
  xlim(-20, 20) +
  ylim(-20, 20)
xz <- ggplot()+
  geom_point(data=rotation, aes(x =x3, y = z3), color="blue", size=0.5)+
  geom_point(data=rotation_outremoved, aes(x =x3, y = z3), color="green", size=0.5)+
  labs(title = "DP_xz", x = "x", y = "z")+
  xlim(-20, 20) +
  ylim(-20, 20)
yx
yz
xz

# rotate the plane so that primary position is aligned to reference position
PrimaryBased <- rotation_outremoved %>% 
  select(1, 33, 34, 35, 36) %>% 
  mutate(RVectorX, RVectorY, RVectorZ) %>% 
  mutate(RotVectorX = 0, 
         RotVectorY = RVectorZ/sqrt(RVectorY^2+RVectorZ^2),
         RotVectorZ = -RVectorY/sqrt(RVectorY^2+RVectorZ^2),
         Rotdegree = 2*acos(RVectorX)) %>% 
  mutate(RotQatScalar = RVectorX,
         xqat = 0,
         yqat = RVectorZ*(sqrt(1-RVectorX^2))/sqrt(RVectorY^2 + RVectorZ^2),
         zqat = -RVectorY*(sqrt(1-RVectorX^2))/sqrt(RVectorY^2 + RVectorZ^2)) %>% 
  mutate(RRxRyRz_scalar = RotQatScalar*RxRyRz_scalar - (xqat*x1 + yqat*y1 + zqat*z1),
         RRxRyRz_x = RotQatScalar*x1 + RxRyRz_scalar*xqat + yqat*z1 - zqat*y1,
         RRxRyRz_y = RotQatScalar*y1 + RxRyRz_scalar*yqat + zqat*x1 - xqat*z1,
         RRxRyRz_z = RotQatScalar*z1 + RxRyRz_scalar*zqat + xqat*y1 - yqat*x1) %>% 
  mutate(AxisRotationΘ = 2*acos(RRxRyRz_scalar),
         RRxRyRz_xrad = RRxRyRz_x/sqrt(1-RRxRyRz_scalar^2),
         RRxRyRz_yrad = RRxRyRz_y/sqrt(1-RRxRyRz_scalar^2),
         RRxRyRz_zrad = RRxRyRz_z/sqrt(1-RRxRyRz_scalar^2),
         "x^2+y^2+z^2" = RRxRyRz_xrad^2 + RRxRyRz_yrad^2 + RRxRyRz_zrad^2,
         RRxRyRz_Θdeg = AxisRotationΘ*180/pi) %>% 
  mutate(rotvecdegx = RRxRyRz_Θdeg* RRxRyRz_xrad,
         rotvecdegy = RRxRyRz_Θdeg* RRxRyRz_yrad,
         rotvecdegz = RRxRyRz_Θdeg* RRxRyRz_zrad)

#remove noise of x from LP
primary_recurrence_nonomitted <- PrimaryBased %>% dplyr::select(rotvecdegx, rotvecdegy, rotvecdegz)
lm_LP <- lm(rotvecdegx ~ rotvecdegy + rotvecdegz, primary_recurrence_nonomitted, na.action=na.omit)
inv_l_LP <- c(lm$coefficients)
l_LP <- -(inv_l_LP)
names(l_LP) <- c("c", "a", "b")

lp_sd = 10 #do not change

#removing outliers on three dimensional plane
primary_recurrence_nonomitted$x_estimated = -l_LP["a"]*primary_recurrence_nonomitted$rotvecdegy - l_LP["b"]*primary_recurrence_nonomitted$rotvecdegz - l_LP["c"]
primary_recurrence_nonomitted$x_distance = abs(primary_recurrence_nonomitted$rotvecdegx - primary_recurrence_nonomitted$x_estimated)

primary_recurrence_nonomitted_threshold = lp_sd*sd(primary_recurrence_nonomitted$rotvecdegx, na.rm=TRUE) 
primary_recurrence_nonomitted$outlier = primary_recurrence_nonomitted$x_distance > primary_recurrence_nonomitted_threshold
primary_recurrence_nonomitted_outremoved <- primary_recurrence_nonomitted %>% dplyr::filter(primary_recurrence_nonomitted$outlier == FALSE)

primary_recurrence <- na.omit(primary_recurrence_nonomitted_outremoved)

#plot Listing Plane with primary position
s3d_LP <- scatterplot3d(primary_recurrence_nonomitted$rotvecdegx, primary_recurrence_nonomitted$rotvecdegy, primary_recurrence_nonomitted$rotvecdegz, color="blue",  pch=20, type="p", lty.hplot=2)
s3d_LP$points3d(primary_recurrence$rotvecdegx, primary_recurrence$rotvecdegy,primary_recurrence$rotvecdegz, pch=16, col = "green")
centroid_LP <- colMeans(primary_recurrence)
normal_vectorLP <- c(PP_x, PP_y, PP_z)
s3d_LP$points3d(rbind(centroid_LP, centroid_LP + normal_vectorLP), type = "l", col = "red", lwd = 2)

#2d plots of listings plane
#change upper and lower bound of the plot if out of range*******************************
xlim_upp = 80
xlim_low = -80
ylim_upp = 80
ylim_low = -80

yx_lp <- ggplot()+
  geom_point(data=primary_recurrence_nonomitted, aes(x =rotvecdegy, y = rotvecdegx), color="blue", size=0.5)+
  geom_point(data=primary_recurrence, aes(x =rotvecdegy, y = rotvecdegx), color="green", size=0.5)+
  labs(title = "LP_yx", x = "y", y = "x")+
  xlim(xlim_low, xlim_upp) +
  ylim(ylim_low, ylim_upp)

yz_lp <- ggplot()+
  geom_point(data=primary_recurrence_nonomitted, aes(x =rotvecdegy, y = rotvecdegz), color="blue", size=0.5)+
  geom_point(data=primary_recurrence, aes(x =rotvecdegy, y = rotvecdegz), color="green", size=0.5)+
  labs(title = "LP_yz", x = "y", y = "z")+
  xlim(xlim_low, xlim_upp) +
  ylim(ylim_low, ylim_upp)

xz_lp <- ggplot()+
  geom_point(data=primary_recurrence_nonomitted, aes(x =rotvecdegx, y = rotvecdegz), color="blue", size=0.5)+
  geom_point(data=primary_recurrence, aes(x =rotvecdegx, y = rotvecdegz), color="green", size=0.5)+
  labs(title = "LP_xz", x = "x", y = "z")+
  xlim(xlim_low, xlim_upp) +
  ylim(ylim_low, ylim_upp)

yx_lp
yz_lp
xz_lp

#calculate mean offset and standard deviation of the plane
LP_average_x <- mean(primary_recurrence$rotvecdegx, na.rm = TRUE)
LP_SD_x <- sd(primary_recurrence$rotvecdegx, na.rm = TRUE)
LP <- data.frame(LP_average_x, LP_SD_x)
setting <- data.frame(band_sd, eye)

library(openxlsx)
output_list <- list("Listing's Plane"=LP,  "rawdata"=pre_fick, "Fick"=fick, "Rotation"=rotation, "RotationNR"=rotation_outremoved, "DPnormalvector"=RVector, "Primary Based"=PrimaryBased, "PrimaryBasedNR"=primary_recurrence, "Primary position"=Primary_position, "setting"=setting)
write.xlsx(output_list, paste("result_turntable_" , "9pt_", which_side, "_", fileno, ".xlsx", sep = ""), overwrite = TRUE)

pdf(paste("2dplots_turntable_" , "9pt_", which_side, "_", fileno, ".pdf", sep = ""), width = 10, height = 10,  bg="white")
print(yx)
print(yz)
print(xz)
print(yx_lp)
print(yz_lp)
print(xz_lp)
dev.off()

pdf(paste("rawplot_turntable_", "9pt_", which_side, "_", fileno, ".pdf", sep = ""), width = 10, height = 10, bg="white")
plot_eyerayx
plot_eyerayy
plot_eyerayz
plot_torsion
dev.off()

pdf(paste("3dplots_turntable_", "9pt_", which_side, "_", fileno, ".pdf", sep = ""), width = 10, height = 10, bg="white")
s3d <- scatterplot3d(rotation$x3, rotation$y3,rotation$z3, pch=20, color = "blue", type="p", lty.hplot=2)
s3d$points3d(rotation_outremoved$x3, rotation_outremoved$y3, rotation_outremoved$z3, col="green", pch=20)
s3d$points3d(rbind(centroid, centroid + normal_vector), type = "l", col = "red", lwd = 2)
s3d_LP <- scatterplot3d(primary_recurrence_nonomitted$rotvecdegx, primary_recurrence_nonomitted$rotvecdegy, primary_recurrence_nonomitted$rotvecdegz, color="blue",  pch=20, type="p", lty.hplot=2)
s3d_LP$points3d(primary_recurrence$rotvecdegx, primary_recurrence$rotvecdegy,primary_recurrence$rotvecdegz, pch=16, col = "green")
s3d_LP$points3d(rbind(centroid_LP, centroid_LP + normal_vectorLP), type = "l", col = "red", lwd = 2)
dev.off()
