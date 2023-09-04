
rm(list = ls())

library(ggplot2)
library(tidyverse)
library(readxl)
library(RcppRoll)
library(scatterplot3d)
library(zoo)
library(forecast)
library(openxlsx)

which_side = "left"
pre_post = "pre"

eye = "left"
band_sd =2.5

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



l = nrow(pre_fick)
# torsion noise removal
k_tor=2 # threshold = k_tor * SD ***********************************************************************************
w_tor=30  # window size = w*2+1 ************************************************************************************
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
      df_tor$presmooth_tor[(t-4):(t+2)] <- NaN
    }
  }
  
  if(tor[(t-1)]-tor[(t)]-tor[(t+1)]==0){#replace with NaN if there are three same consecutive numbers
    df_tor$presmooth_tor[(t-1):(t+1)] <- NaN
  }
}

if(l>(w_tor*2+1)){
  for(t in (w_tor+1):(l-w_tor)){
    Ut_tor<-tor[(t-w_tor):(t+w_tor)]#torsion within the window
    if(abs(tor[t]-mean(Ut_tor))>k_tor*sd(Ut_tor)) { #平均との差が標準偏差のk_tor倍より大きいまたは小さい値を外れ値
      df_tor$outlier_tor[t] <- TRUE # 条件を満たした場合TRUEを代入
      df_tor$presmooth_tor[t] <- mean(Ut_tor)　#outlierになったところはは平均値で置き換え
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

#Passband noise removal only for particularly noisy data
df_torsion$torNR_PB <- if_else(df_torsion$torNR>lowpass&df_torsion$torNR<highpass, df_torsion$torNR, NaN)


#Smooth the torsion
Rolling_threshold = 100L #determine the window size for rolling average
df_torsion$smoothedtor <- rollapply(df_torsion$torNR_PB, width=Rolling_threshold, FUN=mean, partial=TRUE, na.rm=TRUE, align="left")
df_smoothtor <- data.frame(df_torsion$time, df_torsion$smoothedtor)
colnames(df_smoothtor) <- c("time", "smoothtor")
setting <- data.frame(eye, which_side, Rolling_threshold)

tor_plot <- ggplot()+
  geom_line(data=df_torsion, aes(x=time, y=torsion, col="torsion"), linewidth=0.2) +
  geom_line(data=df_torsion, aes(x=time, y=torNR_PB, col="torNR_PB"), linewidth=0.2) +
  geom_line(data=df_smoothtor, aes(x=time, y=smoothtor, col="smoothtor"), linewidth=0.8)+
labs(title = "torsion", x = "time", y = "torsion(deg)") +
  ylim(-20, 20)
tor_plot


pdf(paste("1ptturntable_tor_plot",  which_side, "_", pre_post, ".pdf", sep=""),  width = 15, height = 10)
print(tor_plot)
dev.off()


output_list <- list("tor_data"=df_torsion, "setting"=setting)
write.xlsx(output_list, paste("1ptturntable_tor_result_",  which_side, "_", pre_post, ".xlsx", sep=""))

