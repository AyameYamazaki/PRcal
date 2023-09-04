
rm(list = ls())


library(ggplot2)
library(tidyverse)
library(readxl)
library(RcppRoll)
library(scatterplot3d)
library(forecast)
library(openxlsx)
library(zoo)


which_side = "left"
pre_post = "pre"

eye = "left"


filename <- paste("turntable_1pt_", which_side, "_", pre_post, ".csv", sep="")
data <- read.csv(paste("turntable_1pt_", which_side, "_", pre_post, ".csv", sep=""))

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
w_erx=30  # window size = w*2+1 ************************************************************************************
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
    if(df_erx$blinktrue_erx[t]==TRUE){
      df_erx$presmooth_erx[(t-4):(t+2)] <- NaN
    }
  }
  
  if(erx[(t-1)]-erx[(t)]-erx[(t+1)]==0){#replace with previous data if there are three same consecutive numbers
    df_erx$presmooth_erx[(t-1):(t+1)] <- NaN
  }
}

if(l>(w_erx*2+1)){
  for(t in (w_erx+1):(l-w_erx)){
    Ut_erx<-erx[(t-w_erx):(t+w_erx)]#eye ray x within the window
    if(abs(erx[t]-mean(Ut_erx))>k_erx*sd(Ut_erx)) { #平均との差が標準偏差のk_erx倍より大きいまたは小さい値を外れ値
      df_erx$outlier_erx[t] <- TRUE # 条件を満たした場合TRUEを代入
      df_erx$presmooth_erx[t] <- mean(Ut_erx)　#outlierになったところはは平均値で置き換え
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

#plot the original eye ray x in blue and the final noise removed eye ray x in red
plot_eyerayx <- ggplot() +
  geom_line(data=df_eyerayx, aes(x=time, y = eyerayx), color = "blue", linewidth = 0.2) +
  geom_line(data=df_eyerayx, aes(x=time, y = erxNR), color = "red", linewidth = 0.2)+
  ylim(-0.5, 0.5)
plot_eyerayx



# eye ray y noise removal
k_ery=2 # threshold = k_ery * SD **********************************************************************************
w_ery=30  # window size = w*2+1 ************************************************************************************
l=nrow(pre_fick) # 行数を取得

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
        df_ery$presmooth_ery[(t-4):(t+2)] <- NaN
      }
  }
    
    if(ery[(t-1)]-ery[(t)]-ery[(t+1)]==0){#replace with previous data if there are three same consecutive numbers
      df_ery$presmooth_ery[(t-1):(t+1)] <- NaN
    }
}

if(l>(w_ery*2+1)){
  for(t in (w_ery+1):(l-w_ery)){
    Ut_ery<-ery[(t-w_ery):(t+w_ery)]#eye ray y within the window
    if(abs(ery[t]-mean(Ut_ery))>k_ery*sd(Ut_ery)) { #平均との差が標準偏差のk_ery倍より大きいまたは小さい値を外れ値
      df_ery$outlier_ery[t] <- TRUE # 条件を満たした場合TRUEを代入
      df_ery$presmooth_ery[t] <- mean(Ut_ery)　#outlierになったところはは平均値で置き換え
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


#plot the original eye ray y in blue and the final noise removed eye ray y in red
plot_eyerayy <- ggplot() +
  geom_line(data=df_eyerayy, aes(x=time, y = eyerayy), color = "blue", linewidth = 0.2) +
  geom_line(data=df_eyerayy, aes(x=time, y = eryNR), color = "red", linewidth = 0.2)+
  ylim(-0.5, 0.5)
plot_eyerayy



# eye ray z noise removal
k_erz=2 # threshold = k_erz * SD **********************************************************************************
w_erz=30  # window size = w*2+1 ************************************************************************************
l=nrow(pre_fick) # 行数を取得

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
      df_erz$presmooth_erz[(t-4):(t+2)] <- NaN
    }
    }
    
  if(erz[(t-1)]-erz[(t)]-erz[(t+1)]==0){#replace with previous data if there are three same consecutive numbers
    df_erz$presmooth_erz[(t-1):(t+1)] <- NaN
    }
  }

if(l>(w_erz*2+1)){
  for(t in (w_erz+1):(l-w_erz)){
    Ut_erz<-erz[(t-w_erz):(t+w_erz)]#eye ray z within the window
    if(abs(erz[t]-mean(Ut_erz))>k_erz*sd(Ut_erz)) { #平均との差が標準偏差のk_erz倍より大きいまたは小さい値を外れ値
      df_erz$outlier_erz[t] <- TRUE # 条件を満たした場合TRUEを代入
      df_erz$presmooth_erz[t] <- mean(Ut_erz)　#outlierになったところはは平均値で置き換え
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


#plot the original eye ray z in blue and the final noise removed eye ray z in red
plot_eyerayz <- ggplot() +
  geom_line(data=df_eyerayz, aes(x=time, y = eyerayz), color = "blue", linewidth = 0.2) +
  geom_line(data=df_eyerayz, aes(x=time, y = erzNR), color = "red", linewidth = 0.2) +
  ylim(0.7, 1.1)
plot_eyerayz



raw_data <- data.frame(time, eyerayx, eyerayy, eyerayz, torsion)
names(raw_data) <- c("time", "eyerayx", "eyerayy", "eyerayz", "torsion")
fick <- data.frame(df_eyerayx$time, df_eyerayx$erxNR, df_eyerayy$eryNR, df_eyerayz$erzNR)
names(fick) <- c("time", "serxNR", "seryNR", "serzNR")

NR_eyerays<- fick %>%
  mutate(Zrad = if_else(is.na(-asin(serxNR/sqrt(serxNR^2 + serzNR^2))), 0, -asin(serxNR/sqrt(serxNR^2 + serzNR^2)))) %>% #FickのZ座標 
  mutate(Zdeg = Zrad*180/pi) #FickのZ座標をradからdegreeに変換


#Smooth the Zdeg
Rolling_threshold = 100L #determine the window size for rolling average
NR_eyerays$smoothedZ <- rollapply(NR_eyerays$Zdeg, width=Rolling_threshold, FUN=mean, partial=TRUE, align="left")
df_smoothZ <- data.frame(NR_eyerays$time, NR_eyerays$smoothedZ)
colnames(df_smoothZ) <- c("time", "smoothZ")

setting <- data.frame(eye, which_side, Rolling_threshold)

Z_plot <- ggplot()+
  geom_line(data=NR_eyerays, aes(x=time, y=Zdeg, col="Zdeg"), linewidth=0.3) +
  geom_line(data=df_smoothZ, aes(x=time, y=smoothZ, col="smoothedZ Line"), linewidth=1)+
  labs(title = "Z_degree", x = "time", y = "Zdeg")+
  scale_x_continuous(breaks=seq(0,120,20))
Z_plot


pdf(paste("1ptturntable_Z_plot",  which_side, "_", pre_post, ".pdf", sep=""),  width = 15, height = 10)
print(Z_plot)
dev.off()


output_list <- list("Z_data"=NR_eyerays, "setting"=setting)
write.xlsx(output_list, paste("1ptturntable_Z_result_",  which_side, "_", pre_post, ".xlsx", sep=""))

