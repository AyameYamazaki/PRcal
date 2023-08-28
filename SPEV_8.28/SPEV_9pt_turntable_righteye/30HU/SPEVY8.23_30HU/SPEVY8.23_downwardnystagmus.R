
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

#change the below parameters according to the data file!!!!!!!!!!!!!!
which_side = "right"
per_post = "post" #is it at the beginning of rotation (per) or after rotation stops (post)
headpos = "30HU"
eye = "right"
#for pitch eye movement, the nystagmus may be variable in direction. Therefore it is necessary to determine the direction by observing Y_plot first and if quick phase is upward (Y_plot is -ve), choose the upward nystagmus file. Vice versa. If no identifiable nystagmus, skip this file.

if(per_post=="per"){
  fileno <- 1
}else{
  if(per_post=="post"){
    fileno <- 7
  }else{
    print("ERROR")
  }
}
#check file name here
paste("turntable_","9pt_", which_side, "_", fileno, ".csv", sep="")
data <- read.csv(paste("turntable_","9pt_", which_side, "_", fileno, ".csv", sep="")) 

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


# eye ray x noise removal
k_erx=2 # threshold = k_erx * SD **********************************************************************************
w_erx=4  # window size = w*2+1 ************************************************************************************
l=nrow(pre_fick) # 行数を取得

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



raw_data <- data.frame(time, eyerayx, eyerayy, eyerayz, torsion)
names(raw_data) <- c("time", "eyerayx", "eyerayy", "eyerayz", "torsion")

fick <- data.frame(df_eyerayx$time, df_eyerayx$smoothed_erxNR, df_eyerayy$smoothed_eryNR, df_eyerayz$erzNR)
names(fick) <- c("time", "serxNR", "seryNR", "serzNR")

NR_eyerays<- fick %>%
  mutate(Yrad = if_else(is.na(-asin(seryNR/sqrt(seryNR^2 + serzNR^2))), 0, -asin(seryNR/sqrt(seryNR^2 + serzNR^2)))) %>% #FickのY座標 
  mutate(Ydeg = Yrad*180/pi) #FickのY座標をradからdegreeに変換

#Smooth the Ydeg
Rolling_threshold = 5L #determine the window size for rolling average
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

#フレームレート(Y_Hz)、threshold値、timelimit値を設定(functionのところ)。
#例えばtimelimit=0.3であれば、0.3秒間以上続く緩徐相からSPEVを抽出するようになる。
#SPEV2でのslopeのプロットが一発でうまくいったら、SPEV3はとばしてSPEV4へ。
#specify time frame if necessary

Y_threshold = 0.05
Y_Hz = 70
Y_timelimit = 0.01
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


#結果。SPEVtimeとは、極値をとったときのApplication.Timeを指す。
Y.result <- data.frame(Y.index, Y.bottomOrTop) %>% 
  mutate(SPEVtime = NR_eyerays$time[Y.index]) %>% 
  mutate(Y = NR_eyerays$smoothedY[Y.index])

#NAを除いたデータに上から順にindexが割り当てられているので、本来のindexに戻す。
Y.true.index <- array()
for (i in 1:nrow(Y.result)) {
  INDEX <- which(NR_eyerays$time == Y.result$SPEVtime[i])  
  Y.true.index[i] <- INDEX
}
Y.result <- cbind(Y.true.index, Y.result[,-1])


#Ydegの極値がどれくらい離れているか、極値そのものの差と極値をとる時間の差を計算。
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

#SPEV抽出
Y.YdegLIST <- dplyr::filter(Y.result, bottomOrTop == 0) %>% 
  mutate(SPEVY = Y.diff/SPEVtime.diff)
Y.SPEVY.sorted <- select(Y.YdegLIST, SPEVY) %>% 
  arrange(SPEVY)
names(Y.SPEVY.sorted) <- c("SPEVY.sorted")
Y.YdegLIST <- cbind(Y.YdegLIST, Y.SPEVY.sorted)


#結果をソート＆グラフ描画。slopeが気に入らなかったら、CとDの値を変更(スクリプト4での変更も可)。
C = 1
D = 1

SPEVtime.start.Y <- array()
SPEVtime.end.Y <- array()
Y.start.Y <- array()
Y.end.Y <- array()

for (i in 0:(nrow(Y.YdegLIST)-3)) { #Y.SPEVY.sortedのNaNを含まないようにするには、最大2行を引かなければならない。
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

#may need to add or delete the latter lines if the data does not exist
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

#Y.YdegLISTのSPEVY.sortedの値を確認。外れ値をみたらグラフを確認。
#グラフが気に入らなかったらCとDの値を変更(あとで変更してもよい)。
#C=1からC=2に変更すれば、1番傾きが大きいものが除外され、新たに11番目が追加される。

#ソートされたデータの整理。
SPEVY.sorted.Y <- Y.YdegLIST$SPEVY.sorted[-c(nrow(Y.YdegLIST), nrow(Y.YdegLIST)-1)]
Y.sorted.data <- cbind(data.frame(SPEVY.sorted.Y), SPEVtime.start.Y) %>% 
  cbind(SPEVtime.end.Y) %>% 
  cbind(Y.start.Y) %>% 
  cbind(Y.end.Y) 

#SPEV観測開始地点もしくは終了地点にてYdegが0になるものに関しては、エラーである可能性が高いので、NULLとする。
#Rank関数により順位を出している。Ydegから算出したSPEV予測値上から5つが予測値中1-5位になっているか確認。

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

#create the result SPEV dataframe with the top 3 SPEV confirmed by manual check
result_SPEVY <- Y.sorted.data %>% dplyr::filter(plot %in% c(SPEVY_1, SPEVY_2, SPEVY_3))
Average_SPEVY <- mean(result_SPEVY$SPEVY.sorted.Y)
setting <- data.frame(Rolling_threshold, Y_timelimit)


library(openxlsx)
output_list <- list("Average_SPEVY"=Average_SPEVY, "top3_SPEVY"=result_SPEVY, "All_SPEVY"=Y.sorted.data, "Eyeray_data"=NR_eyerays, "setting"=setting)
write.xlsx(output_list, paste("turntable_result_SPEVY_", which_side, per_post, ".xlsx", sep=""))

pdf(paste("turntable_Yplot", which_side, per_post, ".pdf", sep=""),  width = 15, height = 10)
print(Y_plot)
print(SPEVY_plot)
dev.off()


