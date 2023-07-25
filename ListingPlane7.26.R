



##set working directory **************************************************************************

rm(list = ls())

library(ggplot2)
library(tidyverse)
library(patchwork)
library(reshape2)
library(readxl)
library(RcppRoll)
library(car)
library(rgl)
library(scatterplot3d)
library(forecast)
library(pracma)

data <- read.csv(file=file.choose()) #csvファイルを読み込む make sure it is a control file! **********************



which_side = "left" #rotation or caloric ear**********************************************************************


lt.data <- dplyr::select(data, 2, 25:27, 36, 46) #左eye ray direction、torsionを選択する
rt.data <- dplyr::select(data, 2, 32:34, 38, 48) #右eye ray direction、torsionを選択する
headset_quat <- dplyr::select(data, 8:11)

#Choose which eye (if right, type "rt.data". If left, type "lt.data")
which_eye = lt.data #choose the eye!!! ****************************************************************************

pre_fick <- which_eye

time <- pre_fick[,1]
eyerayx <- pre_fick[,2]
eyerayy <- pre_fick[,3]
eyerayz <- pre_fick[,4]
torsion <- pre_fick[,6]
blink <- pre_fick[,5]


# outlier 確認
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
    if(abs(mean(erx[t]-2:erx[t]))-abs(mean(erx[t]:erx[t]+2))==0){
      df_erx$presmooth_erx[t] <- mean(Ut_erx)
    }
    
    if(abs(erx[t]-mean(Ut_erx))>k_erx*sd(Ut_erx)) { #平均との差が標準偏差のk_erx倍より大きいまたは小さい値を外れ値
      #if(abs(erx[t]-median(Ut_erx))>k_erx*mad(Ut_erx)) {　#中央値と中央絶対偏差を使う方法も
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
#correct the value from first window
for(i in (l-9):l){
  if(is.na(df_eyerayx$smoothed_erxNR[i])){
    df_eyerayx$smoothed_erxNR[i] <- mean(df_eyerayx$erxNR[(l-9):l], is.na=TRUE)
  }else{
    df_eyerayx$smoothed_erxNR[i] <- df_eyerayx$erxNR[i]
  }
}
df_eyerayx <- df_eyerayx %>% dplyr::filter(erxNR_blinktrue==FALSE)

#plot the original eye ray x in blue and the final noise removed eye ray x in red
ggplot(df_eyerayx, aes(x=time)) +
  geom_point(aes(y = eyerayx), color = "blue", size = 0.2) +
  geom_point(aes(y = erxNR), color = "red", size = 0.2) +
  geom_point(aes(y = smoothed_erxNR), color = "pink", size = 0.2) +
  ylim(-0.5, 0.5)




# outlier 確認
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
    Ut_ery<-ery[(t-w_ery):(t+w_ery)]#eye ray x within the window
    if(abs(mean(ery[t]-2:ery[t]))-abs(mean(ery[t]:ery[t]+2))==0){
      df_ery$presmooth_ery[t] <- mean(Ut_ery)
    }
    
    if(abs(ery[t]-mean(Ut_ery))>k_ery*sd(Ut_ery)) { #平均との差が標準偏差のk_ery倍より大きいまたは小さい値を外れ値
      #if(abs(ery[t]-median(Ut_ery))>k_ery*mad(Ut_ery)) {　#中央値と中央絶対偏差を使う方法も
      df_ery$outlier_ery[t] <- TRUE # 条件を満たした場合TRUEを代入
      df_ery$presmooth_ery[t] <- mean(Ut_ery)　#outlierになったところはは平均値で置き換え
    }
  }
  for(t in (w_ery+1):(l-w_ery)){
    Ut_ery<-ery[(t-w_ery):(t+w_ery)]#eye ray x within the window
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
#create eye ray x df with blink omitted
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
df_eyerayy$smoothed_eryNR <- savgol(df_eyerayy$eryNR, 9)
#correct the value from first window
for(i in 1:9){
  if(is.na(df_eyerayy$smoothed_eryNR[i])){
    df_eyerayy$smoothed_eryNR[i] <- mean(df_eyerayy$eryNR[1:9], is.na=TRUE)
  }else{
    df_eyerayy$smoothed_eryNR[i] <- df_eyerayy$eryNR[i]
  }
}
#correct the value from first window
for(i in (l-9):l){
  if(is.na(df_eyerayy$smoothed_eryNR[i])){
    df_eyerayy$smoothed_eryNR[i] <- mean(df_eyerayy$eryNR[(l-9):l], is.na=TRUE)
  }else{
    df_eyerayy$smoothed_eryNR[i] <- df_eyerayy$eryNR[i]
  }
}
df_eyerayy <- df_eyerayy %>% dplyr::filter(eryNR_blinktrue==FALSE)

#plot the original eye ray x in blue and the final noise removed eye ray x in red
ggplot(df_eyerayy, aes(x=time)) +
  geom_point(aes(y = eyerayy), color = "blue", size = 0.2) +
  geom_point(aes(y = eryNR), color = "red", size = 0.2) +
  geom_point(aes(y = smoothed_eryNR), color = "pink", size = 0.2) +
  ylim(-0.5, 0.5)




# outlier 確認
k_erz=2 # threshold = k_erz * SD **********************************************************************************
w_erz=4  # window size = w*2+1 ************************************************************************************

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
    Ut_erz<-erz[(t-w_erz):(t+w_erz)]#eye ray x within the window
    if(abs(mean(erz[t]-2:erz[t]))-abs(mean(erz[t]:erz[t]+2))==0){
      df_erz$presmooth_erz[t] <- mean(Ut_erz)
    }
    
    if(abs(erz[t]-mean(Ut_erz))>k_erz*sd(Ut_erz)) { #平均との差が標準偏差のk_erz倍より大きいまたは小さい値を外れ値
      #if(abs(erz[t]-median(Ut_erz))>k_erz*mad(Ut_erz)) {　#中央値と中央絶対偏差を使う方法も
      df_erz$outlier_erz[t] <- TRUE # 条件を満たした場合TRUEを代入
      df_erz$presmooth_erz[t] <- mean(Ut_erz)　#outlierになったところはは平均値で置き換え
    }
  }
  for(t in (w_erz+1):(l-w_erz)){
    Ut_erz<-erz[(t-w_erz):(t+w_erz)]#eye ray x within the window
    Blink_erz <- df_erz[(t-w_erz):(t+w_erz),]#df_erz of within the window
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
df_eyerayz <- df_eyerayz %>% dplyr::filter(erzNR_blinktrue==FALSE)

#plot the original eye ray x in blue and the final noise removed eye ray x in red
ggplot(df_eyerayz, aes(x=time)) +
  geom_point(aes(y = eyerayz), color = "blue", size = 0.2) +
  geom_point(aes(y = erzNR), color = "red", size = 0.2) +
  ylim(0.7, 1.3)





# outlier 確認
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
    Ut_tor<-tor[(t-w_tor):(t+w_tor)]#eye ray x within the window
    if(abs(mean(tor[t]-2:tor[t]))-abs(mean(tor[t]:tor[t]+2))==0){
      df_tor$presmooth_tor[t] <- mean(Ut_tor)
    }
    
    if(abs(tor[t]-mean(Ut_tor))>k_tor*sd(Ut_tor)) { #平均との差が標準偏差のk_tor倍より大きいまたは小さい値を外れ値
      #if(abs(tor[t]-median(Ut_tor))>k_tor*mad(Ut_tor)) {　#中央値と中央絶対偏差を使う方法も
      df_tor$outlier_tor[t] <- TRUE # 条件を満たした場合TRUEを代入
      df_tor$presmooth_tor[t] <- mean(Ut_tor)　#outlierになったところはは平均値で置き換え
    }
  }
  for(t in (w_tor+1):(l-w_tor)){
    Ut_tor<-tor[(t-w_tor):(t+w_tor)]#eye ray x within the window
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
#correct the value from first window
for(i in (l-9):l){
  if(is.na(df_torsion$smoothed_torNR[i])){
    df_torsion$smoothed_torNR[i] <- mean(df_torsion$torNR[(l-9):l], is.na=TRUE)
  }else{
    df_torsion$smoothed_torNR[i] <- df_torsion$torNR[i]
  }
}

df_torsion <- df_torsion %>% dplyr::filter(torNR_blinktrue==FALSE)

#plot the original eye ray x in blue and the final noise removed eye ray x in red
ggplot(df_torsion, aes(x=time)) +
  geom_point(aes(y = torsion), color = "blue", size = 0.2) +
  geom_point(aes(y = torNR), color = "red", size = 0.2) +
  geom_point(aes(y = smoothed_torNR), color = "pink", size = 0.2) +
  ylim(-10, 15) #change limit according to the data ********************************************************



#radデータ作成。
radian <- function(x){
  rad <-  x * pi / 180
  return(rad)
}

#Fick's co-ordinate(rad)の作成。
raw_data <- data.frame(time, eyerayx, eyerayy, eyerayz, torsion)
names(raw_data) <- c("time", "eyerayx", "eyerayy", "eyerayz", "torsion")
fick <- data.frame(df_eyerayx$time, df_eyerayx$smoothed_erxNR, df_eyerayy$smoothed_eryNR, df_eyerayz$erzNR, torsion_rad = radian(df_torsion$smoothed_torNR))
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

#Fick座標系から主軸座標系へのquaternionを介した変換
#rotationタブ作成
#第一回旋
rotation <- fick %>% 
  dplyr::select(time, X, Y, Z) %>% 
  rename(Θx = X, Θy = Y, Θz = Z) %>% 
  mutate(QZscalar = cos(Θz/2), QZx = 0, QZy = 0, QZz = -sin(Θz/2),
         RZscalar = cos(Θz/2), RZx = 0, RZy = 0, RZz = sin(Θz/2)) %>%  #第２回旋
  mutate(QYscalar = cos(Θy/2), QYx = sin(Θz)*sin(Θy/2), QYy = -cos(Θz)*sin(Θy/2), QYz = 0,
         RYscalar = QYscalar, RYx = -QYx, RYy = -QYy, RYz = 0) %>% #第３回旋 
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


##回帰タブ作成
#重回帰分析の式　３変数の回帰式　x+ay+bz+c=0　は平面を表す→回帰平面。 
#z=ax+by+cでもよいが、上記式よりx=-ay-bz-cと考える。

lm <- lm(x3 ~ y3 + z3, rotation, na.action=na.omit) #y^ = a + b1*x1 + b2*x2の形で重回帰分析を実施。
inv_l_DP <- c(lm$coefficients)
#x=-ay-bz-cのため、符号反転する必要あり」
l_DP <- -(inv_l_DP)
names(l_DP) <- c("c", "a", "b")

#removinng outliers on three dimensional plane
rotation$x3_estimated = -l_DP["a"]*rotation$y3 - l_DP["b"]*rotation$z3 - l_DP["c"]
rotation$x3_distance = abs(rotation$x3 - rotation$x3_estimated)

rotation_threshold = 2*sd(rotation$x3, na.rm=TRUE) #change threshold according to data ################################################################
rotation$outlier = rotation$x3_distance > rotation_threshold
rotation_outremoved <- dplyr::filter(rotation, rotation$outlier == FALSE)
df_centroid <- data.frame(rotation_outremoved$x3, rotation_outremoved$y3, rotation_outremoved$z3)

centroid <- colMeans(df_centroid)
# Create the 3D scatterplot
s3d <- scatterplot3d(rotation_outremoved$x3, rotation_outremoved$y3, rotation_outremoved$z3, color="blue", pch=20, type="p", lty.hplot=2)

#vector perpendicular to the plane is (1, a, b). Therefore divide by square root of (1+a^2+b^2) to obtain normalized unit vector.
RVectorX <- 1/sqrt(1+l_DP[2]^2+l_DP[3]^2)
RVectorY <- l_DP[2]/sqrt(1+l_DP[2]^2+l_DP[3]^2)
RVectorZ <- l_DP[3]/sqrt(1+l_DP[2]^2+l_DP[3]^2)
normal_vector <- c(RVectorX, RVectorY, RVectorZ)
# Draw a line in the direction of the normal vector, starting from the centroid
s3d$points3d(rbind(centroid, centroid + normal_vector), type = "l", col = "red", lwd = 2)

#直交ベクトルの前傾角　arctanz/x
ArctanZX <- atan2(RVectorZ, RVectorX)
deg_atanzx <- 180/pi*ArctanZX

RVector <- data.frame(RVectorX, RVectorY, RVectorZ, ArctanZX)

#Primary position
PP_x <- 2*RVectorX^2-1
PP_y <- (2*RVectorX*RVectorY*((1-RVectorX^2)^0.5)/(RVectorY^2+RVectorZ^2)^0.5)
PP_z <- (2*RVectorX*RVectorZ*((1-RVectorX^2)^0.5)/(RVectorY^2+RVectorZ^2)^0.5)

Primary_position <- data.frame(PP_x, PP_y, PP_z)

#2dグラフ note that compared to excel the yx plane's x-axis is inverted.
yx <- ggplot(rotation_outremoved)+
  geom_point(aes(x =y3, y = x3))+
  labs(title = "DP_yx", x = "y", y = "x")+
  xlim(-20, 20) +
  ylim(-20, 20)
yz <- ggplot(rotation_outremoved)+
  geom_point(aes(x =y3, y = z3))+
  labs(title = "DP_yz", x = "y", y = "z")+
  xlim(-20, 20) +
  ylim(-20, 20)
xz <- ggplot(rotation_outremoved)+
  geom_point(aes(x =x3, y = z3))+
  labs(title = "DP_xz", x = "x", y = "z")+
  xlim(-20, 20) +
  ylim(-20, 20)
yz
xz
yx

#primary positionタブ
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

lp_sd = 2 #change threshold according to data ******************************************************************************

#removing outliers on three dimensional plane
primary_recurrence_nonomitted$x_estimated = -l_LP["a"]*primary_recurrence_nonomitted$rotvecdegy - l_LP["b"]*primary_recurrence_nonomitted$rotvecdegz - l_LP["c"]
primary_recurrence_nonomitted$x_distance = abs(primary_recurrence_nonomitted$rotvecdegx - primary_recurrence_nonomitted$x_estimated)

primary_recurrence_nonomitted_threshold = lp_sd*sd(primary_recurrence_nonomitted$rotvecdegx, na.rm=TRUE) 
primary_recurrence_nonomitted$outlier = primary_recurrence_nonomitted$x_distance > primary_recurrence_nonomitted_threshold
primary_recurrence_nonomitted_outremoved <- primary_recurrence_nonomitted %>% dplyr::filter(primary_recurrence_nonomitted$outlier == FALSE)

primary_recurrence <- na.omit(primary_recurrence_nonomitted_outremoved)

#plot Listing Plane with primary position
s3d_LP <- scatterplot3d(primary_recurrence$rotvecdegx, primary_recurrence$rotvecdegy,primary_recurrence$rotvecdegz, color="green", pch=20, type="p", lty.hplot=2)
centroid_LP <- colMeans(primary_recurrence)
normal_vectorLP <- c(PP_x, PP_y, PP_z)
s3d_LP$points3d(rbind(centroid_LP, centroid_LP + normal_vectorLP), type = "l", col = "red", lwd = 2)

#2dグラフの作成
#change upper and lower bound of the plot if Listing Plane is out of range*******************************
xlim_upp = 40
xlim_low = 0
ylim_upp = 40
ylim_low = -40

yx_lp <- ggplot(primary_recurrence)+
  geom_point(aes(x =rotvecdegy, y = rotvecdegx))+
  labs(title = "LP_yx", x = "y", y = "x")+
  xlim(xlim_low, xlim_upp) +
  ylim(ylim_low, ylim_upp)

yz_lp <- ggplot(primary_recurrence)+
  geom_point(aes(x =rotvecdegy, y = rotvecdegz))+
  labs(title = "LP_yz", x = "y", y = "z")+
  xlim(xlim_low, xlim_upp) +
  ylim(ylim_low, ylim_upp)

xz_lp <- ggplot(primary_recurrence)+
  geom_point(aes(x =rotvecdegx, y = rotvecdegz))+
  labs(title = "LP_xz", x = "x", y = "z")+
  xlim(xlim_low, xlim_upp) +
  ylim(ylim_low, ylim_upp)

yx_lp
yz_lp
xz_lp

#listing planeからaverage, SD計算
LP_average_x <- mean(primary_recurrence$rotvecdegx, na.rm = TRUE)
LP_SD_x <- sd(primary_recurrence$rotvecdegx, na.rm = TRUE)
LP <- data.frame(LP_average_x, LP_SD_x)

library(openxlsx)
output_list <- list("Listing's Plane"=LP, "Fick"=fick, "Rotation"=rotation, "DPnormalvector"=RVector, "Primary Based"=PrimaryBased, "Primary position NR"=primary_recurrence, "Primary position"=Primary_position)
write.xlsx(output_list, paste("turntable_", which_side, "_control.xlsx", sep = ""), overwrite = TRUE)

pdf(paste("turntable_", which_side, "_control.pdf", sep = ""), width = 10, height = 10)
print(yx)
print(yz)
print(xz)
print(yx_lp)
print(yz_lp)
print(xz_lp)
dev.off()

pdf(paste("turntable_", which_side, "_control_scatterplot.pdf", sep = ""), width = 10, height = 10)
s3d <- scatterplot3d(recurrence$x, recurrence$y, recurrence$z, color="blue", pch=20, type="p", lty.hplot=2)
s3d$points3d(rbind(centroid, centroid + normal_vector), type = "l", col = "red", lwd = 2)
s3d_LP <- scatterplot3d(PrimaryBased$rotvecdegx, PrimaryBased$rotvecdegy,PrimaryBased$rotvecdegz, color="green", pch=20, type="p", lty.hplot=2)
s3d_LP$points3d(rbind(primary_centroid, primary_centroid + primary_normal_vector), type = "l", col = "red", lwd = 2)
dev.off()


