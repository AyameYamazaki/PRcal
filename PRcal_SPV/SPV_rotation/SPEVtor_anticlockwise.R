
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

# check if the  nystagmus is upward or downward on the plot. If the quick phase is upward, choose the "per" R script file. If it is downward, choose the "post" R script file.

#change the below parameters according to the data file!!!!!!!!!!!!!! change to left post accordingly
which_side = "right"
per_post = "per" #is it at the beginning of rotation (per) or after rotation stops (post)
headpos = "30ND"
eye = "right"

band_sd = 2.5

#Use this script for 30ND rightper and leftpost, 30NU leftper and rightpost and 60NU leftper and rightpost, although check the raw data before determining which side, as some subjects exhibit inversion with 30NU


tor_timelimit = 0.03
Rolling_threshold = 5L #determine the window size for rolling average

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



l = nrow(pre_fick)
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
    Ut_tor<-tor[(t-w_tor):(t+w_tor)]
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




raw_data <- data.frame(time, eyerayx, eyerayy, eyerayz, torsion)
names(raw_data) <- c("time", "eyerayx", "eyerayy", "eyerayz", "torsion")

#Smooth the torsion

df_torsion$smoothedtor <- rollapply(df_torsion$torNR_PB, width=Rolling_threshold, FUN=mean, partial=TRUE)
#df_torsion$smoothedtor <- savgol(df_torsion$torsion, 39)
df_smoothtor <- data.frame(df_torsion$time, df_torsion$smoothedtor)
colnames(df_smoothtor) <- c("time", "smoothtor")
veltor <- diff(df_smoothtor$smoothtor)/diff(df_smoothtor$time)
veltor <- c(NaN, veltor)
df_smoothtor_vel <- cbind(df_smoothtor, veltor)# difference between consecutive tor divided by consecutive time

#where vel <-20, there is downward saccade. Since the below script assumes downward saccade as part of the SPEV, we need to create a block so that SPEV calculation does not include the downward saccade.
downsac_threshold = -20 #determine threshold for detecting downward saccade
df_smoothtor_vel$downsac <- rep(FALSE, nrow(df_smoothtor_vel))
for(i in 2:nrow(df_smoothtor_vel)){
  if(is.na(df_smoothtor_vel$veltor[i])){
    next
  }
  if(df_smoothtor_vel$veltor[i] < downsac_threshold){
    df_smoothtor_vel$downsac[i] <- TRUE #TRUE if downwardsac is occurring
  }
}
#locate the beginning of the downward sac and add values onto the same place so that below SPEV script correctly knows it is at the end of one SPEV.


correcting_threshold = 0 # input 0 if the saccade is not affected in the plot. input 3 if affected.

df_smoothtor_vel$downsacelev <- rep(0,nrow(df_smoothtor_vel))
for(i in 2:nrow(df_smoothtor_vel)){
  if(df_smoothtor_vel$downsac[i]==TRUE){
    df_smoothtor_vel$downsacelev[i] <- df_smoothtor_vel$smoothtor[i]+correcting_threshold
  }else{
    df_smoothtor_vel$downsacelev[i] <- df_smoothtor_vel$smoothtor[i]
  }
}



tor_plot <- ggplot()+
  geom_point(data=df_torsion, aes(x=time, y=torsion, col="torsion"), size=0.5)+
  geom_line(data=df_torsion, aes(x=time, y=smoothedtor, col="smoothedtor Line"))+
  geom_point(data=df_torsion, aes(x=time, y=smoothedtor, col="smoothedtor Point"),size=0.5)+
  geom_line(data=df_smoothtor_vel, aes(x=time, y=downsacelev, col="correcteddownsac"))
labs(title = "torsion_degree", x = "time", y = "torsion(deg)")+
  scale_x_continuous(breaks=seq(0,10,by=1))+
  ylim(-15, 15)
tor_plot


#########check the tor_plot and if the quick phase is DOWNWARD IN THE PLOT (i.e. nystagmus is upward), do not use this script, but the  other one. ##################################################################


tor_threshold = 0.05 # do not change
tor_Hz = 70 #do not change

tor_duration_min <- min(time)
tor_duration_max <- max(time)

pretordeg <- df_smoothtor_vel %>%
  dplyr::filter(time >= tor_duration_min & time <= tor_duration_max)
tordeg <- na.omit(pretordeg$downsacelev)

#using tor as detecting nystagmus
BOTTOM <- 0
TOP <- 1
tor.index <- array()
tor.bottomOrTop <- array()
numOfExtremePoints <- 0
numOfBottom <- 0
numOfTop <- 0
thresholdValue <- 0.0
arraySize <- length(tordeg)

maxValue <- max(tordeg)
minValue <- min(tordeg)
thresholdValue <- tor_threshold*(maxValue-minValue)

flag <- 0
k1 <- 1
k2 <- 1
temp_max <- tordeg[1]
temp_min <- tordeg[1]
for(j in 2:arraySize){
  if(temp_max < tordeg[j])
    temp_max <- tordeg[j]
  if(temp_min > tordeg[j])
    temp_min <- tordeg[j]
  
  if( (temp_min + thresholdValue) < tordeg[j]){#//increase
    flag <- 1
    k1 <- j
    numOfExtremePoints <- numOfExtremePoints + 1
    numOfBottom <- numOfBottom + 1
    tor.index[numOfExtremePoints] <- 1
    tor.bottomOrTop[numOfExtremePoints] <- BOTTOM
    break
  }
  if( (temp_max - thresholdValue) > tordeg[j]){#//decrease
    flag <- 3
    k2 <- j
    numOfExtremePoints <- numOfExtremePoints + 1
    numOfTop <- numOfTop + 1
    tor.index[numOfExtremePoints] <- 1
    tor.bottomOrTop[numOfExtremePoints] <- TOP
    break
  }
}

if(j == arraySize){#// No change
  return (NULL);
}

#  //------main
for(j in 2:arraySize-1){
  
  if(flag==1){
    if(tordeg[j] > tordeg[j+1]){#//point from increase to decrease is regarded as temporary top 
      flag <- 2
      k2 <- j
    }
  }
  if(flag==2){
    if(tordeg[k2] < tordeg[j]){#//If y[j] is more than y[k2], y[j] is regarded as temporary top.(update)
      k2 <- j
    }
    if( (tordeg[k2] - thresholdValue) > tordeg[j] & (k2-k1)/tor_Hz > tor_timelimit ){#// top is decided when decrease of more than threshold*(max-min) from temporary top
      flag <- 3
      numOfExtremePoints <- numOfExtremePoints + 1
      numOfTop <- numOfTop + 1
      tor.index[numOfExtremePoints] <- k2
      tor.bottomOrTop[numOfExtremePoints] <- TOP
    }
  }
  if(flag==3){
    if(tordeg[j] < tordeg[j+1]){#//point from decrease to increase is regarded as temporary bottom 
      flag <- 4
      k1 <- j
    }
  }
  if(flag==4){
    if(tordeg[k1] > tordeg[j]){#//If y[j] is less than y[k1], y[j] is regarded as temporary bottom.(update)
      k1 <- j
    }
    if( (tordeg[k1] + thresholdValue) < tordeg[j]  ){#//botom is decided when increase of more than threshold*(max-min) from temporary bottom
      flag <- 1
      numOfExtremePoints <- numOfExtremePoints + 1
      numOfBottom <- numOfBottom + 1
      tor.index[numOfExtremePoints] <- k1
      tor.bottomOrTop[numOfExtremePoints] <- BOTTOM
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
tor.result <- data.frame(tor.index, tor.bottomOrTop) %>% 
  mutate(SPEVtime = df_torsion$time[tor.index]) %>% 
  mutate(tor = df_torsion$smoothedtor[tor.index])

#allocate back to correct timestamp
tor.true.index <- array()
for (i in 1:nrow(tor.result)) {
  INDEX <- which(df_torsion$time == tor.result$SPEVtime[i])  
  tor.true.index[i] <- INDEX
}
tor.result <- cbind(tor.true.index, tor.result[,-1])


#calculate SPV
tor.SPEVtime.diff <- data.frame(diff(tor.result$SPEVtime))
tor.SPEVtime.diff[1,] <- 0
tor.SPEVtime.diff[nrow(tor.SPEVtime.diff),] <- 0
tor.SPEVtime.diff <- rbind(c(NaN), tor.SPEVtime.diff)

tor.tordeg.diff <- data.frame(diff(tor.result$tor))
tor.tordeg.diff[1,] <- 0
tor.tordeg.diff[nrow(tor.tordeg.diff),] <- 0
tor.tordeg.diff <- rbind(c(NaN), tor.tordeg.diff)

tor.result <- tor.result %>% 
  cbind(tor.SPEVtime.diff) %>% 
  cbind(tor.tordeg.diff) 
names(tor.result) <- c("index", "bottomOrTop", "SPEVtime", "tor", "SPEVtime.diff", "tor.diff")

#extract correct side of nystagmus
tor.tordegLIST <- dplyr::filter(tor.result, bottomOrTop == 0) %>% 
  mutate(SPEVtor = tor.diff/SPEVtime.diff)
tor.SPEVtor.sorted <- select(tor.tordegLIST, SPEVtor) %>% 
  arrange(SPEVtor)
names(tor.SPEVtor.sorted) <- c("SPEVtor.sorted")
tor.tordegLIST <- cbind(tor.tordegLIST, tor.SPEVtor.sorted)

C = 1
D = 1

SPEVtime.start.tor <- array()
SPEVtime.end.tor <- array()
tor.start.tor <- array()
tor.end.tor <- array()

for (i in 0:(nrow(tor.tordegLIST)-3)) { #get rid of NAN
  A <- which(tor.tordegLIST$SPEVtor == tor.tordegLIST$SPEVtor.sorted[C+i])
  SPEVtime.end.tor[i+1] <- tor.tordegLIST$SPEVtime[A]
  SPEVtime.start.tor[i+1] <- SPEVtime.end.tor[i+1] - tor.tordegLIST$SPEVtime.diff[A]
  tor.end.tor[i+1] <- tor.tordegLIST$tor[A]
  tor.start.tor[i+1] <- tor.end.tor[i+1] - tor.tordegLIST$tor.diff[A]
}

numOfResult = 9

slope.SPEVtime.tor <- data.frame(rbind(SPEVtime.start.tor[C:(C+numOfResult)], SPEVtime.end.tor[C:(C+numOfResult)]))
slope.SPEVtor.tor <- data.frame(rbind(tor.start.tor[D:(D+numOfResult)], tor.end.tor[D:(D+numOfResult)]))
colnames(slope.SPEVtime.tor) <- c(LETTERS[1:ncol(slope.SPEVtime.tor)])
colnames(slope.SPEVtor.tor) <- c(LETTERS[1:ncol(slope.SPEVtor.tor)])

#plot the lines over eye position
SPEVtor_plot <- ggplot() + 
  labs(x="Time", y="tordeg") + 
  geom_line(aes(x=df_torsion$time, y=df_torsion$smoothedtor, col="tordeg"), linewidth=0.5) + 
  geom_line(aes(x=slope.SPEVtime.tor$A, y=slope.SPEVtor.tor$A, col="A"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.tor$B, y=slope.SPEVtor.tor$B, col="B"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.tor$C, y=slope.SPEVtor.tor$C, col="C"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.tor$D, y=slope.SPEVtor.tor$D, col="D"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.tor$E, y=slope.SPEVtor.tor$E, col="E"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.tor$F, y=slope.SPEVtor.tor$F, col="F"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.tor$G, y=slope.SPEVtor.tor$G, col="G"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.tor$H, y=slope.SPEVtor.tor$H, col="H"), linewidth=1.5) + 
  geom_line(aes(x=slope.SPEVtime.tor$I, y=slope.SPEVtor.tor$I, col="I"), linewidth=1.5) + 
  scale_y_continuous(breaks = seq(-15,15,1)) +
  scale_x_continuous(breaks=seq(floor(min(df_torsion$time)),floor(max(df_torsion$time)), 1))
SPEVtor_plot

# sort in right order and get rid of values that are 0
SPEVtor.sorted.tor <- tor.tordegLIST$SPEVtor.sorted[-c(nrow(tor.tordegLIST), nrow(tor.tordegLIST)-1)]
tor.sorted.data <- cbind(data.frame(SPEVtor.sorted.tor), SPEVtime.start.tor) %>% 
  cbind(SPEVtime.end.tor) %>% 
  cbind(tor.start.tor) %>% 
  cbind(tor.end.tor) 

tor.sorted.data <- tor.sorted.data %>% 
  mutate(expected.SPEV = if_else(tor.start.tor != 0 & tor.end.tor != 0, (tor.end.tor-tor.start.tor)/(SPEVtime.end.tor-SPEVtime.start.tor), NaN)) %>% 
  mutate(RANK = rank(expected.SPEV, na.last = TRUE, ties.method = "min")) %>%
  mutate(plot=LETTERS[1:nrow(tor.sorted.data)])

tor.sorted.data.numofResult <- tor.sorted.data %>%
  dplyr::slice(1:numOfResult)

Chron.tor <- tor.sorted.data.numofResult %>%
  arrange(SPEVtime.start.tor)
view(Chron.tor)

#choose the top 3 SPV index by checking the graph and Chron.tor (data in chronological order to help identify the letter).******************************************************************************************************************************

SPEVtor_1 <- "C"
SPEVtor_2 <- "B"
SPEVtor_3 <- "A"

#create the result SPEV dataframe with the top 3 SPEV confirmed by manual inspection
result_SPEVtor <- tor.sorted.data %>% dplyr::filter(plot %in% c(SPEVtor_1, SPEVtor_2, SPEVtor_3))
Average_SPEVtor <- mean(result_SPEVtor$SPEVtor.sorted.tor)
setting <- data.frame(Rolling_threshold, tor_timelimit)


library(openxlsx)
output_list <- list("Average_SPEVtor"=Average_SPEVtor, "top3_SPEVtor"=result_SPEVtor, "All_SPEVtor"=tor.sorted.data, "setting"=setting)
write.xlsx(output_list, paste("turntable_result_SPEVtor_", which_side, per_post, ".xlsx", sep=""))

pdf(paste("turntable_torplot", which_side, per_post, ".pdf", sep=""),  width = 15, height = 10)
print(tor_plot)
print(SPEVtor_plot)
dev.off()

