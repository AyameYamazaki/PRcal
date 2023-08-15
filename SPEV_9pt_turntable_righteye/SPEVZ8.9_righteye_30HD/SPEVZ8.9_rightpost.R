
##set working directory **************************************************************************

rm(list = ls())

library(ggplot2)
library(tidyverse)
library(readxl)
library(RcppRoll)
library(scatterplot3d)
library(pracma)
library(zoo)
library(forecast)

# check if the eye ray x nystagmus is to the right or left. If the quick phase is to the left, choose the "per" R script file. If it is to the right, choose the "post" R script file.

which_side = "right"
per_post = "post" #is it at the beginning of rotation (per) or after rotation stops (post)
headpos = "30HD"
#left per1 shows leftward horizontal nystagmus
#left post7 shows rightward horizontal nystagmus

data <- read.csv("turntable_post_9pt_right_7.csv")

rt.data <- data %>% dplyr::select("Application.Time", "Eye.Ray.right.dir.x", "Eye.Ray.right.dir.y", "Eye.Ray.right.dir.z", "Eye.Torsion..degrees..right", "Eye.State.right")

pre_fick <- rt.data

time <- pre_fick$Application.Time
eyerayx <- pre_fick$Eye.Ray.right.dir.x
eyerayy <- pre_fick$Eye.Ray.right.dir.y
eyerayz <- pre_fick$Eye.Ray.right.dir.z
torsion <- pre_fick$Eye.Torsion..degrees..right
blink <- pre_fick$Eye.State.right


# eye ray x noise removal
k_erx=2 # threshold = k_erx * SD **********************************************************************************
w_erx=4  # window size = w*2+1 ************************************************************************************
l=nrow(data) # 行数を取得

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
    Ut_erx<-erx[(t-w_erx):(t+w_erx)]#eye ray x within the window
    if(erx[(t-1)]-erx[(t)]-erx[(t+1)]==0){ #replace with mean if there are three same consecutive numbers
      df_erx$presmooth_erx[t] <- mean(Ut_erx)
    }
    
    if(abs(erx[t]-mean(Ut_erx))>k_erx*sd(Ut_erx)) { #平均との差が標準偏差のk_erx倍より大きいまたは小さい値を外れ値
      df_erx$outlier_erx[t] <- TRUE # 条件を満たした場合TRUEを代入
      df_erx$presmooth_erx[t] <- mean(Ut_erx)　#outlierになったところはは平均値で置き換え
    }
  }
  for(t in (w_erx+1):(l-w_erx)){
    Ut_erx<-erx[(t-w_erx):(t+w_erx)]#eye ray x within the window
    Blink_erx <- df_erx[(t-w_erx):(t+w_erx),]#df_erx of within the window
    for(b in 1:nrow(Blink_erx)){
      if(Blink_erx$blinktrue_erx[b]==TRUE){
        df_erx$presmooth_erx[b] <- mean(Ut_erx)
      }
    }
  }
}
erxNR <- df_erx$presmooth_erx
erxNR_blinktrue <- df_erx$blinktrue_erx
#create eye ray x df with blink omitted
df_eyerayx <- data.frame(time, eyerayx, erxNR, erxNR_blinktrue)

#correct the value from first window
for(i in 1:9){
  if(is.na(df_eyerayx$erxNR[i])){
    df_eyerayx$erxNR[i] <- mean(df_eyerayx$erxNR[1:9], is.na=TRUE)
  }else{
    df_eyerayx$erxNR[i] <- df_eyerayx$eyerayx[i]
  }
}

#Savitzky-Golay smoothing
df_eyerayx$smoothed_erxNR <- savgol(df_eyerayx$erxNR,9)
#correct the value from first window
for(i in 1:9){
  if(is.na(df_eyerayx$smoothed_erxNR[i])){
    df_eyerayx$smoothed_erxNR[i] <- mean(df_eyerayx$erxNR[1:9], is.na=TRUE)
  }else{
    df_eyerayx$smoothed_erxNR[i] <- df_eyerayx$erxNR[i]
  }
}
#correct the value from last window
for(i in (l-9):l){
  if(is.na(df_eyerayx$smoothed_erxNR[i])){
    df_eyerayx$smoothed_erxNR[i] <- mean(df_eyerayx$erxNR[(l-9):l], is.na=TRUE)
  }else{
    df_eyerayx$smoothed_erxNR[i] <- df_eyerayx$erxNR[i]
  }
}


#plot the original eye ray x in blue and the final noise removed eye ray x in red
plot_eyerayx <- ggplot(df_eyerayx, aes(x=time)) +
  geom_point(aes(y = eyerayx), color = "blue", size = 0.2) +
  geom_point(aes(y = erxNR), color = "red", size = 0.2) +
  geom_point(aes(y = smoothed_erxNR), color = "pink", size = 0.2) +
  ylim(-0.5, 0.5)
plot_eyerayx



#eye ray y noise removal
k_ery=2 # threshold = k_ery * SD ********************************************************************************
w_ery=4  # window size = w*2+1 **********************************************************************************

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
    Ut_ery<-ery[(t-w_ery):(t+w_ery)]#eye ray y within the window
    if(ery[(t-1)]-ery[(t)]-ery[(t+1)]==0){ #replace with mean if there are three same consecutive numbers
      df_ery$presmooth_ery[t] <- mean(Ut_ery)
    }
    
    if(abs(ery[t]-mean(Ut_ery))>k_ery*sd(Ut_ery)) { #平均との差が標準偏差のk_ery倍より大きいまたは小さい値を外れ値
      df_ery$outlier_ery[t] <- TRUE # 条件を満たした場合TRUEを代入
      df_ery$presmooth_ery[t] <- mean(Ut_ery)　#outlierになったところはは平均値で置き換え
    }
  }
  for(t in (w_ery+1):(l-w_ery)){
    Ut_ery<-ery[(t-w_ery):(t+w_ery)]#eye ray y within the window
    Blink_ery <- df_ery[(t-w_ery):(t+w_ery),]#df_ery of within the window
    for(b in 1:nrow(Blink_ery)){
      if(Blink_ery$blinktrue_ery[b]==TRUE){
        df_ery$presmooth_ery[b] <- mean(Ut_ery)
      }
    }
  }
}
eryNR <- df_ery$presmooth_ery
eryNR_blinktrue <- df_ery$blinktrue_ery
df_eyerayy <- data.frame(time, eyerayy, eryNR, eryNR_blinktrue)

#correct the value from first window
for(i in 1:9){
  if(is.na(df_eyerayy$eryNR[i])){
    df_eyerayy$eryNR[i] <- mean(df_eyerayy$eryNR[1:9], is.na=TRUE)
  }else{
    df_eyerayy$eryNR[i] <- df_eyerayy$eyerayy[i]
  }
}

#Savitzky-Golay smoothing
df_eyerayy$smoothed_eryNR <- savgol(df_eyerayy$eryNR,9)
#correct the value from first window
for(i in 1:9){
  if(is.na(df_eyerayy$smoothed_eryNR[i])){
    df_eyerayy$smoothed_eryNR[i] <- mean(df_eyerayy$eryNR[1:9], is.na=TRUE)
  }else{
    df_eyerayy$smoothed_eryNR[i] <- df_eyerayy$eryNR[i]
  }
}
#correct the value from last window
for(i in (l-9):l){
  if(is.na(df_eyerayy$smoothed_eryNR[i])){
    df_eyerayy$smoothed_eryNR[i] <- mean(df_eyerayy$eryNR[(l-9):l], is.na=TRUE)
  }else{
    df_eyerayy$smoothed_eryNR[i] <- df_eyerayy$eryNR[i]
  }
}


#plot the original eye ray y in blue and the final noise removed eye ray y in red
plot_eyerayy <- ggplot(df_eyerayy, aes(x=time)) +
  geom_point(aes(y = eyerayy), color = "blue", size = 0.2) +
  geom_point(aes(y = eryNR), color = "red", size = 0.2) +
  geom_point(aes(y = smoothed_eryNR), color = "pink", size = 0.2) +
  ylim(-0.5, 0.5)
plot_eyerayy




# eye ray z noise removal
k_erz=2 # threshold = k_erz * SD **********************************************************************************
w_erz=4  # window size = w*2+1 ************************************************************************************

df_erz <- data.frame(time, eyerayz, blink)
df_erz$presmooth_erz <- as.numeric(df_erz$eyerayz)
df_erz$outlier_erz<-rep(FALSE,l)
df_erz$blinktrue_erz <- rep(FALSE,l)
for(i in 1:l){
  if(df_erz$blink[i]=="Closed"){
    df_erz$blinktrue_erz[i] <- TRUE
  }
}

if(l>(w_erz*2+1)){
  erz=df_erz$presmooth_erz
  for(t in (w_erz+1):(l-w_erz)){
    Ut_erz<-erz[(t-w_erz):(t+w_erz)]
    if(ery[(t-1)]-ery[(t)]-ery[(t+1)]==0){
      df_erz$presmooth_erz[t] <- mean(Ut_erz)
    }
    
    if(abs(erz[t]-mean(Ut_erz))>k_erz*sd(Ut_erz)) {
      df_erz$outlier_erz[t] <- TRUE 
      df_erz$presmooth_erz[t] <- mean(Ut_erz)
    }
  }
  for(t in (w_erz+1):(l-w_erz)){
    Ut_erz<-erz[(t-w_erz):(t+w_erz)]
    Blink_erz <- df_erz[(t-w_erz):(t+w_erz),]
    for(b in 1:nrow(Blink_erz)){
      if(Blink_erz$blinktrue_erz[b]==TRUE){
        df_erz$presmooth_erz[b] <- mean(Ut_erz)
      }
    }
  }
}
erzNR <- df_erz$presmooth_erz
erzNR_blinktrue <- df_erz$blinktrue_erz
df_eyerayz <- data.frame(time, eyerayz, erzNR, erzNR_blinktrue)

#correct the value from first window
for(i in 1:9){
  if(is.na(df_eyerayz$erzNR[i])){
    df_eyerayz$erzNR[i] <- mean(df_eyerayz$erzNR[1:9], is.na=TRUE)
  }else{
    df_eyerayz$erzNR[i] <- df_eyerayz$eyerayz[i]
  }
}

#plot the original eye ray z in blue and the final noise removed eye ray z in red
plot_eyerayz <- ggplot(df_eyerayz, aes(x=time)) +
  geom_point(aes(y = eyerayz), color = "blue", size = 0.2) +
  geom_point(aes(y = erzNR), color = "red", size = 0.2) +
  ylim(0.7, 1.3)
plot_eyerayz




# torsion noise removal
k_tor=2 # threshold = k_tor * SD ***********************************************************************************
w_tor=4  # window size = w*2+1 *************************************************************************************

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
    Ut_tor<-tor[(t-w_tor):(t+w_tor)]#torsion within the window
    if(tor[(t-1)]-tor[(t)]-tor[(t+1)]==0){ #replace with mean if there are three same consecutive numbers
      df_tor$presmooth_tor[t] <- mean(Ut_tor)
    }
    
    if(abs(tor[t]-mean(Ut_tor))>k_tor*sd(Ut_tor)) { #平均との差が標準偏差のk_tor倍より大きいまたは小さい値を外れ値
      df_tor$outlier_tor[t] <- TRUE # 条件を満たした場合TRUEを代入
      df_tor$presmooth_tor[t] <- mean(Ut_tor)　#outlierになったところはは平均値で置き換え
    }
  }
  for(t in (w_tor+1):(l-w_tor)){
    Ut_tor<-tor[(t-w_tor):(t+w_tor)]#torsion within the window
    Blink_tor <- df_tor[(t-w_tor):(t+w_tor),]#df_tor of within the window
    for(b in 1:nrow(Blink_tor)){
      if(Blink_tor$blinktrue_tor[b]==TRUE){
        df_tor$presmooth_tor[b] <- mean(Ut_tor)
      }
    }
  }
}
torNR <- df_tor$presmooth_tor
torNR_blinktrue <- df_tor$blinktrue_tor
df_torsion <- data.frame(time, torsion, torNR, torNR_blinktrue)

#correct the value from first window
for(i in 1:9){
  if(is.na(df_torsion$torNR[i])){
    df_torsion$torNR[i] <- mean(df_torsion$torNR[1:9], is.na=TRUE)
  }else{
    df_torsion$torNR[i] <- df_torsion$torsion[i]
  }
}

#Savitzky-Golay smoothing
df_torsion$smoothed_torNR <- savgol(df_torsion$torNR, 9)
#correct the value from first window
for(i in 1:9){
  if(is.na(df_torsion$smoothed_torNR[i])){
    df_torsion$smoothed_torNR[i] <- mean(df_torsion$torNR[1:9], is.na=TRUE)
  }else{
    df_torsion$smoothed_torNR[i] <- df_torsion$torNR[i]
  }
}
#correct the value from last window
for(i in (l-9):l){
  if(is.na(df_torsion$smoothed_torNR[i])){
    df_torsion$smoothed_torNR[i] <- mean(df_torsion$torNR[(l-9):l], is.na=TRUE)
  }else{
    df_torsion$smoothed_torNR[i] <- df_torsion$torNR[i]
  }
}



#plot the original torsion in blue and the final noise removed torsion in red
plot_torsion <- ggplot(df_torsion, aes(x=time)) +
  geom_point(aes(y = torsion), color = "blue", size = 0.2) +
  geom_point(aes(y = torNR), color = "red", size = 0.2) +
  geom_point(aes(y = smoothed_torNR), color = "pink", size = 0.2) +
  ylim(-15, 15) #change limit according to the data ********************************************************
plot_torsion


#radデータ作成。
radian <- function(x){
  rad <-  x * pi / 180
  return(rad)
}


raw_data <- data.frame(time, eyerayx, eyerayy, eyerayz, torsion)
names(raw_data) <- c("time", "eyerayx", "eyerayy", "eyerayz", "torsion")

fick <- data.frame(df_eyerayx$time, df_eyerayx$smoothed_erxNR, df_eyerayy$smoothed_eryNR, df_eyerayz$erzNR, torsion_rad = radian(df_torsion$smoothed_torNR))
names(fick) <- c("time", "serxNR", "seryNR", "serzNR", "torsion_rad")

NR_eyerays<- fick %>%
  mutate(Zrad = if_else(is.na(-asin(serxNR/sqrt(serxNR^2 + serzNR^2))), 0, -asin(serxNR/sqrt(serxNR^2 + serzNR^2)))) %>% #FickのZ座標 
  mutate(Zdeg = Zrad*180/pi) #FickのZ座標をradからdegreeに変換

#Smooth the Zdeg
Rolling_threshold = 9L #determine the window size for rolling average
NR_eyerays$smoothedZ <- rollapply(NR_eyerays$Zdeg, width=Rolling_threshold, FUN=mean, partial=TRUE)
df_smoothz <- data.frame(NR_eyerays$time, NR_eyerays$smoothedZ)
colnames(df_smoothz) <- c("time", "smoothz")
velz <- diff(df_smoothz$smoothz)/diff(df_smoothz$time)
velz <- c(NaN, velz)
df_smoothz_vel <- cbind(df_smoothz, velz)# difference between consecutive z divided by consecutive time

#where vel <-30, there is rightward saccade. Since the below script assumes rightward saccade as part of the SPEV, we need to create a block so that SPEV calculation does not include the rightward saccade.
Rsac_threshold = -30 #determine threshold for detecting rightward saccade
df_smoothz_vel$rightsac <- rep(FALSE, nrow(df_smoothz_vel))
for(i in 2:nrow(df_smoothz_vel)){
  if(df_smoothz_vel$velz[i] < Rsac_threshold){
    df_smoothz_vel$rightsac[i] <- TRUE #TRUE if rightsac is occurring
  }
}
#locate the beginning of the rightward sac and add values onto the same place so that below SPEV script correctly knows it is at the end of one SPEV.
df_smoothz_vel$rightsacelev <- rep(0,nrow(df_smoothz_vel))
for(i in 2:nrow(df_smoothz_vel)){
  if(df_smoothz_vel$rightsac[i]==TRUE){
    df_smoothz_vel$rightsacelev[i] <- df_smoothz_vel$smoothz[i]+3
  }else{
    df_smoothz_vel$rightsacelev[i] <- df_smoothz_vel$smoothz[i]
  }
}



Z_plot <- ggplot()+
  geom_point(data=NR_eyerays, aes(x=time, y=Zdeg, col="Zdeg"), size=0.5)+
  geom_line(data=NR_eyerays, aes(x=time, y=smoothedZ, col="smoothedZ Line"))+
  geom_point(data=NR_eyerays, aes(x=time, y=smoothedZ, col="smoothedZ Point"),size=0.5)+
  geom_line(data=df_smoothz_vel, aes(x=time, y=rightsacelev, col="correctedRsac"))
  labs(title = "Z_degree", x = "time", y = "Zdeg")+
  scale_x_continuous(breaks=seq(0,10,by=1))+
  ylim(-15, 15)
Z_plot

#フレームレート(Z_Hz)、threshold値、timelimit値を設定(functionのところ)。
#例えばtimelimit=0.3であれば、0.3秒間以上続く緩徐相からSPEVを抽出するようになる。
#SPEV2でのslopeのプロットが一発でうまくいったら、SPEV3はとばしてSPEV4へ。
Z_threshold = 0.05
Z_Hz = 70
Z_timelimit = 0.03

x <- na.omit(NR_eyerays$serxNR)
y <- na.omit(NR_eyerays$seryNR)
Zdeg <- na.omit(df_smoothz_vel$rightsacelev)

#using Z as detecting nystagmus
BOTTOM <- 0
TOP <- 1
Z.index <- array()
Z.bottomOrTop <- array()
numOfExtremePoints <- 0
numOfBottom <- 0
numOfTop <- 0
thresholdValue <- 0.0
arraySize <- length(Zdeg)

maxValue <- max(Zdeg)
minValue <- min(Zdeg)
thresholdValue <- Z_threshold*(maxValue-minValue)

flag <- 0
k1 <- 1
k2 <- 1
temp_max <- Zdeg[1]
temp_min <- Zdeg[1]
for(j in 2:arraySize){
  if(temp_max < Zdeg[j])
    temp_max <- Zdeg[j]
  if(temp_min > Zdeg[j])
    temp_min <- Zdeg[j]
  
  if( (temp_min + thresholdValue) < Zdeg[j]){#//increase
    flag <- 1
    k1 <- j
    numOfExtremePoints <- numOfExtremePoints + 1
    numOfBottom <- numOfBottom + 1
    Z.index[numOfExtremePoints] <- 1
    Z.bottomOrTop[numOfExtremePoints] <- BOTTOM
    break
  }
  if( (temp_max - thresholdValue) > Zdeg[j]){#//decrease
    flag <- 3
    k2 <- j
    numOfExtremePoints <- numOfExtremePoints + 1
    numOfTop <- numOfTop + 1
    Z.index[numOfExtremePoints] <- 1
    Z.bottomOrTop[numOfExtremePoints] <- TOP
    break
  }
}

if(j == arraySize){#// No change
  return (NULL);
}

#  //------main
for(j in 2:arraySize-1){
  
  if(flag==1){
    if(Zdeg[j] > Zdeg[j+1]){#//point from increase to decrease is regarded as temporary top 
      flag <- 2
      k2 <- j
    }
  }
  if(flag==2){
    if(Zdeg[k2] < Zdeg[j]){#//If y[j] is more than y[k2], y[j] is regarded as temporary top.(update)
      k2 <- j
    }
    if( (Zdeg[k2] - thresholdValue) > Zdeg[j] & (k2-k1)/Z_Hz > Z_timelimit ){#// top is decided when decrease of more than threshold*(max-min) from temporary top
      flag <- 3
      numOfExtremePoints <- numOfExtremePoints + 1
      numOfTop <- numOfTop + 1
      Z.index[numOfExtremePoints] <- k2
      Z.bottomOrTop[numOfExtremePoints] <- TOP
    }
  }
  if(flag==3){
    if(Zdeg[j] < Zdeg[j+1]){#//point from decrease to increase is regarded as temporary bottom 
      flag <- 4
      k1 <- j
    }
  }
  if(flag==4){
    if(Zdeg[k1] > Zdeg[j]){#//If y[j] is less than y[k1], y[j] is regarded as temporary bottom.(update)
      k1 <- j
    }
    if( (Zdeg[k1] + thresholdValue) < Zdeg[j]  ){#//botom is decided when increase of more than threshold*(max-min) from temporary bottom
      flag <- 1
      numOfExtremePoints <- numOfExtremePoints + 1
      numOfBottom <- numOfBottom + 1
      Z.index[numOfExtremePoints] <- k1
      Z.bottomOrTop[numOfExtremePoints] <- BOTTOM
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


#結果。SPEVtimeとは、極値をとったときのApplication.Timeを指す。
Z.result <- data.frame(Z.index, Z.bottomOrTop) %>% 
  mutate(SPEVtime = NR_eyerays$time[Z.index]) %>% 
  mutate(Z = NR_eyerays$smoothedZ[Z.index])

#NAを除いたデータに上から順にindexが割り当てられているので、本来のindexに戻す。
Z.true.index <- array()
for (i in 1:nrow(Z.result)) {
  INDEX <- which(NR_eyerays$time == Z.result$SPEVtime[i])  
  Z.true.index[i] <- INDEX
}
Z.result <- cbind(Z.true.index, Z.result[,-1])


#Zdegの極値がどれくらい離れているか、極値そのものの差と極値をとる時間の差を計算。
Z.SPEVtime.diff <- data.frame(diff(Z.result$SPEVtime))
Z.SPEVtime.diff[1,] <- 0
Z.SPEVtime.diff[nrow(Z.SPEVtime.diff),] <- 0
Z.SPEVtime.diff <- rbind(c(NaN), Z.SPEVtime.diff)

Z.Zdeg.diff <- data.frame(diff(Z.result$Z))
Z.Zdeg.diff[1,] <- 0
Z.Zdeg.diff[nrow(Z.Zdeg.diff),] <- 0
Z.Zdeg.diff <- rbind(c(NaN), Z.Zdeg.diff)

Z.result <- Z.result %>% 
  cbind(Z.SPEVtime.diff) %>% 
  cbind(Z.Zdeg.diff) 
names(Z.result) <- c("index", "bottomOrTop", "SPEVtime", "Z", "SPEVtime.diff", "Z.diff")

#SPEV抽出
Z.ZdegLIST <- filter(Z.result, bottomOrTop == 0) %>% 
  mutate(SPEVZ = Z.diff/SPEVtime.diff)
Z.SPEVZ.sorted <- select(Z.ZdegLIST, SPEVZ) %>% 
  arrange(SPEVZ)
names(Z.SPEVZ.sorted) <- c("SPEVZ.sorted")
Z.ZdegLIST <- cbind(Z.ZdegLIST, Z.SPEVZ.sorted)


#結果をソート＆グラフ描画。slopeが気に入らなかったら、CとDの値を変更(スクリプト4での変更も可)。
C = 1
D = 1

SPEVtime.start.Z <- array()
SPEVtime.end.Z <- array()
Z.start.Z <- array()
Z.end.Z <- array()

for (i in 0:(nrow(Z.ZdegLIST)-3)) { #Z.SPEVZ.sortedのNaNを含まないようにするには、最大2行を引かなければならない。
  A <- which(Z.ZdegLIST$SPEVZ == Z.ZdegLIST$SPEVZ.sorted[C+i])
  SPEVtime.end.Z[i+1] <- Z.ZdegLIST$SPEVtime[A]
  SPEVtime.start.Z[i+1] <- SPEVtime.end.Z[i+1] - Z.ZdegLIST$SPEVtime.diff[A]
  Z.end.Z[i+1] <- Z.ZdegLIST$Z[A]
  Z.start.Z[i+1] <- Z.end.Z[i+1] - Z.ZdegLIST$Z.diff[A]
}

numOfResult = 9

slope.SPEVtime.Z <- data.frame(rbind(SPEVtime.start.Z[C:(C+numOfResult)], SPEVtime.end.Z[C:(C+numOfResult)]))
slope.SPEVZ.Z <- data.frame(rbind(Z.start.Z[D:(D+numOfResult)], Z.end.Z[D:(D+numOfResult)]))
colnames(slope.SPEVtime.Z) <- c(LETTERS[1:ncol(slope.SPEVtime.Z)])
colnames(slope.SPEVZ.Z) <- c(LETTERS[1:ncol(slope.SPEVZ.Z)])

#may need to add or delete the latter lines if the data does not exist
SPEVZ_plot <- ggplot() + 
  labs(x="Time", y="Zdeg") + 
  geom_line(aes(x=NR_eyerays$time, y=NR_eyerays$smoothedZ, col="Zdeg"), linetype="dashed", linewidth=0.5) + 
  geom_line(aes(x=slope.SPEVtime.Z$A, y=slope.SPEVZ.Z$A, col="A"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.Z$B, y=slope.SPEVZ.Z$B, col="B"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.Z$C, y=slope.SPEVZ.Z$C, col="C"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.Z$D, y=slope.SPEVZ.Z$D, col="D"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.Z$E, y=slope.SPEVZ.Z$E, col="E"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.Z$F, y=slope.SPEVZ.Z$F, col="F"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.Z$G, y=slope.SPEVZ.Z$G, col="G"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.Z$H, y=slope.SPEVZ.Z$H, col="H"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.Z$I, y=slope.SPEVZ.Z$I, col="I"), linewidth=1.5) + 
  scale_y_continuous(breaks = seq(-15,15,1)) +
  scale_x_continuous(breaks=seq(floor(min(NR_eyerays$time)),floor(max(NR_eyerays$time)), 1))
SPEVZ_plot
#Z.ZdegLISTのSPEVZ.sortedの値を確認。外れ値をみたらグラフを確認。
#グラフが気に入らなかったらCとDの値を変更(あとで変更してもよい)。
#C=1からC=2に変更すれば、1番傾きが大きいものが除外され、新たに11番目が追加される。

#ソートされたデータの整理。
SPEVZ.sorted.Z <- Z.ZdegLIST$SPEVZ.sorted[-c(nrow(Z.ZdegLIST), nrow(Z.ZdegLIST)-1)]
Z.sorted.data <- cbind(data.frame(SPEVZ.sorted.Z), SPEVtime.start.Z) %>% 
  cbind(SPEVtime.end.Z) %>% 
  cbind(Z.start.Z) %>% 
  cbind(Z.end.Z) 

#SPEV観測開始地点もしくは終了地点にてZdegが0になるものに関しては、エラーである可能性が高いので、NULLとする。
#Rank関数により順位を出している。Zdegから算出したSPEV予測値上から5つが予測値中1-5位になっているか確認。

Z.sorted.data <- Z.sorted.data %>% 
  mutate(expected.SPEV = if_else(Z.start.Z != 0 & Z.end.Z != 0, (Z.end.Z-Z.start.Z)/(SPEVtime.end.Z-SPEVtime.start.Z), NaN)) %>% 
  mutate(RANK = rank(expected.SPEV, na.last = TRUE, ties.method = "min")) %>%
  mutate(plot=LETTERS[1:nrow(Z.sorted.data)])

Z.sorted.data.numofResult <- Z.sorted.data %>%
  dplyr::slice(1:numOfResult)

Chron.Z <- Z.sorted.data.numofResult %>%
  arrange(SPEVtime.start.Z)
view(Chron.Z)

#choose the top 3 SPEV index by checking the graph and Chron.Z (data in chronological order to help identify the letter).******************************************************************************************************************************
SPEVZ_1 <- "D"
SPEVZ_2 <- "E"
SPEVZ_3 <- "F"

#create the result SPEV dataframe with the top 3 SPEV confirmed by manual check
result_SPEVZ <- Z.sorted.data %>% dplyr::filter(plot %in% c(SPEVZ_1, SPEVZ_2, SPEVZ_3))
Average_SPEVZ <- mean(result_SPEVZ$SPEVZ.sorted.Z)


library(openxlsx)
output_list <- list("Average_SPEVZ"=Average_SPEVZ, "top3_SPEVZ"=result_SPEVZ, "All_SPEVZ"=Z.sorted.data, "Eyeray_data"=NR_eyerays)
write.xlsx(output_list, paste("turntable_result_SPEVZ_", which_side, per_post, ".xlsx", sep=""))

pdf(paste("turntable_Zplot", which_side, per_post, ".pdf", sep=""),  width = 15, height = 10)
print(Z_plot)
print(SPEVZ_plot)
dev.off()