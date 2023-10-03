
##set working directory **************************************************************************

rm(list = ls())


library(ggplot2)
library(tidyverse)
library(readxl)
library(RcppRoll)
library(scatterplot3d)
library(forecast)
library(openxlsx)
library(zoo)

# check if the eye ray x nystagmus is to the right or left. If the quick phase is to the left, choose the "per" R script file. If it is to the right, choose the "post" R script file.

#change the below parameters according to the data file!!!!!!!!!!!!!!
which_side = "left"
fileno = 5

eye = "right"

Y_timelimit = 0.03
Rolling_threshold = 3L #determine the window size for rolling average
#for pitch eye movement, the nystagmus may be variable in direction. Therefore it is necessary to determine the direction by observing Y_plot first and if quick phase is upward (Y_plot is -ve), choose the upward nystagmus file. Vice versa. If no identifiable nystagmus, skip this file.


data <- read.csv(paste("caloric_", "9pt_", which_side, "_", fileno, ".csv", sep="")) 

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
k_erx=2 
w_erx=30 
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
    Ut_erx<-erx[(t-w_erx):(t+w_erx)]
    if(abs(erx[t]-mean(Ut_erx))>k_erx*sd(Ut_erx)) { 
      df_erx$outlier_erx[t] <- TRUE 
      df_erx$presmooth_erx[t] <- mean(Ut_erx)　
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
df_eyerayx$erxNR_PB <- if_else(df_eyerayx$erxNR>-0.2&df_eyerayx$erxNR<0.2, df_eyerayx$erxNR, NaN)

#plot the original eye ray x in blue and the final noise removed eye ray x in red
plot_eyerayx <- ggplot() +
  geom_line(data=df_eyerayx, aes(x=time, y = eyerayx), color = "blue", linewidth = 0.2) +
  geom_line(data=df_eyerayx, aes(x=time, y = erxNR_PB), color = "red", linewidth = 0.2)+
    scale_x_continuous(breaks = seq(0,120,10))+
  ylim(-0.25, 0.25)
plot_eyerayx



# eye ray y noise removal
k_ery=2 
w_ery=30 
l=nrow(pre_fick) 

df_ery <- data.frame(time, eyerayy, blink)
df_ery$presmooth_ery <- as.numeric(df_ery$eyerayy)
df_ery$outlier_ery<-rep(FALSE,l)
df_ery$blinktrue_ery <- rep(FALSE,l)
for(i in 1:l){
  if(df_ery$blink[i]=="Closed"){
    df_ery$blinktrue_ery[i] <- TRUE#if eye state is "Closed", input TRUE into the df_ery$blinktrue_ery
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
    
    if(ery[(t-1)]-ery[(t)]-ery[(t+1)]==0){#replace with previous data if there are three same consecutive numbers
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
df_eyerayy$eryNR_PB <- if_else(df_eyerayy$eryNR>-0.2&df_eyerayy$eryNR<0.2, df_eyerayy$eryNR, NaN)

#plot the original eye ray y in blue and the final noise removed eye ray y in red
plot_eyerayy <- ggplot() +
  geom_line(data=df_eyerayy, aes(x=time, y = eyerayy), color = "blue", linewidth = 0.2) +
  geom_line(data=df_eyerayy, aes(x=time, y = eryNR_PB), color = "red", linewidth = 0.2)+
  scale_x_continuous(breaks = seq(0,120,10))+
  ylim(-0.25, 0.25)
plot_eyerayy




# eye ray z noise removal
k_erz=2 
w_erz=30 
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
  scale_x_continuous(breaks = seq(0,120,10))+
  ylim(0.9, 1.1)
plot_eyerayz




raw_data <- data.frame(time, eyerayx, eyerayy, eyerayz, torsion)
names(raw_data) <- c("time", "eyerayx", "eyerayy", "eyerayz", "torsion")
fick <- data.frame(df_eyerayx$time, df_eyerayx$erxNR, df_eyerayy$eryNR, df_eyerayz$erzNR)
names(fick) <- c("time", "serxNR", "seryNR", "serzNR")


NR_eyerays<- fick %>%
  mutate(Yrad = if_else(is.na(-asin(seryNR/sqrt(seryNR^2 + serzNR^2))), 0, -asin(seryNR/sqrt(seryNR^2 + serzNR^2)))) %>% #FickのY座標 
  mutate(Ydeg = Yrad*180/pi) 



#Smooth the Ydeg

NR_eyerays$smoothedY <- rollapply(NR_eyerays$Ydeg, width=Rolling_threshold, FUN=mean, partial=TRUE)
df_smoothY <- data.frame(NR_eyerays$time, NR_eyerays$smoothedY)
colnames(df_smoothY) <- c("time", "smoothY")
velY <- diff(df_smoothY$smoothY)/diff(df_smoothY$time)
velY <- c(NaN, velY)
df_smoothY_vel <- cbind(df_smoothY, velY)# difference between consecutive Y divided by consecutive time

#where vel <-30, there is downward saccade. Since the below script assumes downward saccade as part of the SPEV, we need to create a block so that SPEV calculation does not include the downward saccade.
downsac_threshold = -30 #determine threshold for detecting downward saccade
df_smoothY_vel$downsac <- rep(FALSE, nrow(df_smoothY_vel))
for(i in 2:nrow(df_smoothY_vel)){
  if(df_smoothY_vel$velY[i] < downsac_threshold){
    df_smoothY_vel$downsac[i] <- TRUE #TRUE if downwardsac is occurring
  }
}
#locate the beginning of the downward sac and add values onto the same place so that below SPEV script correctly knows it is at the end of one SPEV.
correcting_threshold = 3
df_smoothY_vel$downsacelev <- rep(0,nrow(df_smoothY_vel))
for(i in 2:nrow(df_smoothY_vel)){
  if(df_smoothY_vel$downsac[i]==TRUE){
    df_smoothY_vel$downsacelev[i] <- df_smoothY_vel$smoothY[i]+correcting_threshold
  }else{
    df_smoothY_vel$downsacelev[i] <- df_smoothY_vel$smoothY[i]
  }
}

Y_plot <- ggplot()+
  geom_point(data=NR_eyerays, aes(x=time, y=Ydeg, col="Ydeg"), size=0.5)+
  geom_line(data=NR_eyerays, aes(x=time, y=smoothedY, col="smoothedY Line"))+
  geom_point(data=NR_eyerays, aes(x=time, y=smoothedY, col="smoothedY Point"),size=0.5)+
  geom_line(data=df_smoothY_vel, aes(x=time, y=downsacelev, col="correcteddownsac"))
  labs(title = "Y_degree", x = "time", y = "Ydeg")+
  scale_x_continuous(breaks=seq(0,10,by=1))+
  ylim(-15, 15)
Y_plot


#########check the Y_plot and if the quick phase is DOWNWARD IN THE PLOT (i.e. nystagmus is upward), do not use this script, but the  other one. ##################################################################


Y_threshold = 0.05
Y_Hz = 70
Y_duration_min <- min(time)
Y_duration_max <- max(time)

preYdeg <- df_smoothY_vel %>%
  dplyr::filter(time >= Y_duration_min & time <= Y_duration_max)
Ydeg <- na.omit(preYdeg$downsacelev)

#using Y as detecting nystagmus
BOTTOM <- 0
TOP <- 1
Y.index <- array()
Y.bottomOrTop <- array()
numOfExtremePoints <- 0
numOfBottom <- 0
numOfTop <- 0
thresholdValue <- 0.0
arraySize <- length(Ydeg)

maxValue <- max(Ydeg)
minValue <- min(Ydeg)
thresholdValue <- Y_threshold*(maxValue-minValue)

flag <- 0
k1 <- 1
k2 <- 1
temp_max <- Ydeg[1]
temp_min <- Ydeg[1]
for(j in 2:arraySize){
  if(temp_max < Ydeg[j])
    temp_max <- Ydeg[j]
  if(temp_min > Ydeg[j])
    temp_min <- Ydeg[j]
  
  if( (temp_min + thresholdValue) < Ydeg[j]){#//increase
    flag <- 1
    k1 <- j
    numOfExtremePoints <- numOfExtremePoints + 1
    numOfBottom <- numOfBottom + 1
    Y.index[numOfExtremePoints] <- 1
    Y.bottomOrTop[numOfExtremePoints] <- BOTTOM
    break
  }
  if( (temp_max - thresholdValue) > Ydeg[j]){#//decrease
    flag <- 3
    k2 <- j
    numOfExtremePoints <- numOfExtremePoints + 1
    numOfTop <- numOfTop + 1
    Y.index[numOfExtremePoints] <- 1
    Y.bottomOrTop[numOfExtremePoints] <- TOP
    break
  }
}

if(j == arraySize){#// No change
  return (NULL);
}

#  //------main
for(j in 2:arraySize-1){
  
  if(flag==1){
    if(Ydeg[j] > Ydeg[j+1]){#//point from increase to decrease is regarded as temporary top 
      flag <- 2
      k2 <- j
    }
  }
  if(flag==2){
    if(Ydeg[k2] < Ydeg[j]){#//If y[j] is more than y[k2], y[j] is regarded as temporary top.(update)
      k2 <- j
    }
    if( (Ydeg[k2] - thresholdValue) > Ydeg[j] & (k2-k1)/Y_Hz > Y_timelimit ){#// top is decided when decrease of more than threshold*(max-min) from temporary top
      flag <- 3
      numOfExtremePoints <- numOfExtremePoints + 1
      numOfTop <- numOfTop + 1
      Y.index[numOfExtremePoints] <- k2
      Y.bottomOrTop[numOfExtremePoints] <- TOP
    }
  }
  if(flag==3){
    if(Ydeg[j] < Ydeg[j+1]){#//point from decrease to increase is regarded as temporary bottom 
      flag <- 4
      k1 <- j
    }
  }
  if(flag==4){
    if(Ydeg[k1] > Ydeg[j]){#//If y[j] is less than y[k1], y[j] is regarded as temporary bottom.(update)
      k1 <- j
    }
    if( (Ydeg[k1] + thresholdValue) < Ydeg[j]  ){#//botom is decided when increase of more than threshold*(max-min) from temporary bottom
      flag <- 1
      numOfExtremePoints <- numOfExtremePoints + 1
      numOfBottom <- numOfBottom + 1
      Y.index[numOfExtremePoints] <- k1
      Y.bottomOrTop[numOfExtremePoints] <- BOTTOM
    }
  }
  
}


#index: index of extreme points in y
#bottomOrTop: bottom(0) or top (1) at index
#numOfExtremePoints: Number of extreme points
#numOfBottom: Number of bottoms at extreme points
#numOfTop: Number of tops at extreme points
#threshold: threshold value (ratio) for judging whether increase or decrease
#threhsoldValue: threshold value to y in threshold

#results
Y.result <- data.frame(Y.index, Y.bottomOrTop) %>% 
  mutate(SPEVtime = NR_eyerays$time[Y.index]) %>% 
  mutate(Y = NR_eyerays$smoothedY[Y.index])

#allocate back to correct timestamp
Y.true.index <- array()
for (i in 1:nrow(Y.result)) {
  INDEX <- which(NR_eyerays$time == Y.result$SPEVtime[i])  
  Y.true.index[i] <- INDEX
}
Y.result <- cbind(Y.true.index, Y.result[,-1])


#calculate SPV
Y.SPEVtime.diff <- data.frame(diff(Y.result$SPEVtime))
Y.SPEVtime.diff[1,] <- 0
Y.SPEVtime.diff[nrow(Y.SPEVtime.diff),] <- 0
Y.SPEVtime.diff <- rbind(c(NaN), Y.SPEVtime.diff)

Y.Ydeg.diff <- data.frame(diff(Y.result$Y))
Y.Ydeg.diff[1,] <- 0
Y.Ydeg.diff[nrow(Y.Ydeg.diff),] <- 0
Y.Ydeg.diff <- rbind(c(NaN), Y.Ydeg.diff)

Y.result <- Y.result %>% 
  cbind(Y.SPEVtime.diff) %>% 
  cbind(Y.Ydeg.diff) 
names(Y.result) <- c("index", "bottomOrTop", "SPEVtime", "Y", "SPEVtime.diff", "Y.diff")

#extract correct side of nystagmus
Y.YdegLIST <- dplyr::filter(Y.result, bottomOrTop == 0) %>% 
  mutate(SPEVY = Y.diff/SPEVtime.diff)
Y.SPEVY.sorted <- select(Y.YdegLIST, SPEVY) %>% 
  arrange(SPEVY)
names(Y.SPEVY.sorted) <- c("SPEVY.sorted")
Y.YdegLIST <- cbind(Y.YdegLIST, Y.SPEVY.sorted)

C = 1
D = 1

SPEVtime.start.Y <- array()
SPEVtime.end.Y <- array()
Y.start.Y <- array()
Y.end.Y <- array()

for (i in 0:(nrow(Y.YdegLIST)-3)) { #get rid of NAN
  A <- which(Y.YdegLIST$SPEVY == Y.YdegLIST$SPEVY.sorted[C+i])
  SPEVtime.end.Y[i+1] <- Y.YdegLIST$SPEVtime[A]
  SPEVtime.start.Y[i+1] <- SPEVtime.end.Y[i+1] - Y.YdegLIST$SPEVtime.diff[A]
  Y.end.Y[i+1] <- Y.YdegLIST$Y[A]
  Y.start.Y[i+1] <- Y.end.Y[i+1] - Y.YdegLIST$Y.diff[A]
}

numOfResult = 9

slope.SPEVtime.Y <- data.frame(rbind(SPEVtime.start.Y[C:(C+numOfResult)], SPEVtime.end.Y[C:(C+numOfResult)]))
slope.SPEVY.Y <- data.frame(rbind(Y.start.Y[D:(D+numOfResult)], Y.end.Y[D:(D+numOfResult)]))
colnames(slope.SPEVtime.Y) <- c(LETTERS[1:ncol(slope.SPEVtime.Y)])
colnames(slope.SPEVY.Y) <- c(LETTERS[1:ncol(slope.SPEVY.Y)])

#plot the lines over eye position
SPEVY_plot <- ggplot() + 
  labs(x="Time", y="Ydeg") + 
  geom_line(aes(x=NR_eyerays$time, y=NR_eyerays$smoothedY, col="Ydeg"), linewidth=0.5) + 
  geom_line(aes(x=slope.SPEVtime.Y$A, y=slope.SPEVY.Y$A, col="A"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.Y$B, y=slope.SPEVY.Y$B, col="B"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.Y$C, y=slope.SPEVY.Y$C, col="C"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.Y$D, y=slope.SPEVY.Y$D, col="D"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.Y$E, y=slope.SPEVY.Y$E, col="E"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.Y$F, y=slope.SPEVY.Y$F, col="F"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.Y$G, y=slope.SPEVY.Y$G, col="G"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.Y$H, y=slope.SPEVY.Y$H, col="H"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.Y$I, y=slope.SPEVY.Y$I, col="I"), linewidth=1.5) + 
  scale_y_continuous(breaks = seq(-15,15,1)) +
  scale_x_continuous(breaks=seq(floor(min(NR_eyerays$time)),floor(max(NR_eyerays$time)), 1))
SPEVY_plot


#########check the Y_plot and if the quick phase is DOWNWARD IN THE PLOT (i.e. nystagmus is upward), do not use this script, but the  other one. ##################################################################
# sort in right order and get rid of values that are 0
SPEVY.sorted.Y <- Y.YdegLIST$SPEVY.sorted[-c(nrow(Y.YdegLIST), nrow(Y.YdegLIST)-1)]
Y.sorted.data <- cbind(data.frame(SPEVY.sorted.Y), SPEVtime.start.Y) %>% 
  cbind(SPEVtime.end.Y) %>% 
  cbind(Y.start.Y) %>% 
  cbind(Y.end.Y) 

Y.sorted.data <- Y.sorted.data %>% 
  mutate(expected.SPEV = if_else(Y.start.Y != 0 & Y.end.Y != 0, (Y.end.Y-Y.start.Y)/(SPEVtime.end.Y-SPEVtime.start.Y), NaN)) %>% 
  mutate(RANK = rank(expected.SPEV, na.last = TRUE, ties.method = "min")) %>%
  mutate(plot=LETTERS[1:nrow(Y.sorted.data)])

Y.sorted.data.numofResult <- Y.sorted.data %>%
  dplyr::slice(1:numOfResult)

Chron.Y <- Y.sorted.data.numofResult %>%
  arrange(SPEVtime.start.Y)
view(Chron.Y)

#choose the top 3 SPEV index by checking the graph and Chron.Y (data in chronological order to help identify the letter).******************************************************************************************************************************

SPEVY_1 <- "G"
SPEVY_2 <- "H"
SPEVY_3 <- "E"

#create the result SPEV dataframe with the top 3 SPEV confirmed by manual inspeection
result_SPEVY <- Y.sorted.data %>% dplyr::filter(plot %in% c(SPEVY_1, SPEVY_2, SPEVY_3))
Average_SPEVY <- mean(result_SPEVY$SPEVY.sorted.Y)
setting <- data.frame(fileno, Rolling_threshold, Y_timelimit, eye)

library(openxlsx)
output_list <- list("Average_SPEVY"=Average_SPEVY, "top3_SPEVY"=result_SPEVY, "All_SPEVY"=Y.sorted.data, "Eyeray_data"=NR_eyerays, "setting"=setting)
write.xlsx(output_list, paste("caloric_result_SPEVY_", which_side, ".xlsx", sep=""))

pdf(paste("caloric_Yplot", which_side, ".pdf", sep=""),  width = 15, height = 10)
print(Y_plot)
print(SPEVY_plot)
dev.off()


