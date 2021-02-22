# calculate accuracy on contingencies on day 1
rm(list=ls())
library(dplyr)# select comes from this
library(ggplot2)
source("data_files/selPart.R")

# sel files
cd<-getwd()
setwd("data_files")
files<-selPart(1)
setwd(cd)

# initialise an empty dataframe
day1res<-data.frame(matrix(ncol=3, nrow=length(files)))
colnames(day1res)<-c("SubNum","PracticeNr", "corrAss" )

# 1. calculate how many pract trials per participant and if the participants got the associations
NrPrTri<-vector()
for (j in 1: (length(files))){
  day1<-read.csv(paste("data_files/", files[j], sep=""))
  
# select practice 
practice<-day1[day1$switch_cond==0,]
# exclude NAs
practice<-practice[!is.na(practice$myownaccuracy),]

#NrPrTri[j]<-nrow(practice)
day1res$SubNum[j]<-as.numeric(substr(files[j], 1,2))
day1res$PracticeNr[j]<-nrow(practice)

day1res$corrAss[j]<-mean(day1$task_resp_2.corr, na.rm=T)
}

# 2. calculate cumulative accuracy per participant and reaction times
AccDay1<-data.frame(SubNum=vector(), butterfly = vector(),
                    response = vector(), accuracy=vector(), cumAcc=vector(), RT=vector(), trialN=vector())
for (j in 1 : (length(files))){
  day1<-read.csv(paste("data_files/", files[j], sep=""))
  # select task1
  task1<-day1[day1$switch_cond==1,]
  # exclude NAs
  task1<-task1[!is.na(task1$task_resp.corr),]
  SubNum<-rep(as.numeric(substr(files[j], 1,2)), times=nrow(task1))
  
  # report the butterfly
  task1$butterfly<-NA
  for (t in 1: nrow(task1)){
  task1$butterfly[t]<-substr(task1$cuedButter[t], 9, (nchar(as.character(task1$cuedButter[t]))-4))
  }

  task1$myownaccuracy<-as.numeric(as.character(task1$myownaccuracy))
  task1$cumAcc<-cummean(task1$myownaccuracy)
  task1$RT<-task1$task_resp.rt
  trialNum<-seq(1:nrow(task1))
  partData<-data.frame(cbind(SubNum,task1$butterfly,task1$task_resp.keys, task1$myownaccuracy, task1$cumAcc, task1$RT, trialNum))
  
  names(partData)[c(2,3,4,5,6,7)]<-c("butterfly","response", "accuracy", "cumAcc", "RT","trialNum")
  AccDay1<-rbind(AccDay1, partData)
}

AccDay1$cumAcc<-as.numeric(as.character(AccDay1$cumAcc))
AccDay1$trialNum<-as.numeric(as.character(AccDay1$trialNum))
AccDay1$RT<-as.numeric(as.character(AccDay1$RT))

# exclude participant 1 and 3
#excl<-c(5, 10)
#AccDay1<-AccDay1[!AccDay1$SubNum %in% excl, ]

# plot cumACC
# by participant
ggplot(AccDay1[AccDay1$SubNum!=1,], aes(x=trialNum, y = cumAcc))+ geom_line()+
  #aes(colour = factor(SubNum))
  facet_wrap(.~SubNum)

# plot RT
# by participant
ggplot(AccDay1, aes(x=trialNum, y = RT))+ geom_line()+
  #aes(colour = factor(SubNum))
  facet_wrap(.~SubNum)


# cut the trials for participants who got to do the trials twice
longFile<-vector()
for ( j in 2 : (length(files))){
  SubNum<-as.numeric(substr(files[j], 1,2))
  partTrials<-AccDay1[AccDay1$SubNum==SubNum,]

  if (nrow(partTrials)>42){ partTrials<-partTrials[-(1:42),]
  partTrials$trialNum<-seq(1:42)}
  
  longFile<-rbind(longFile, partTrials)
}

# add a column indicating the list and the trial from change
longFile$listN<-1
longFile$trialtfromchange<-rep(seq(1:42), times=10)
# print the file
write.csv(longFile, "output_files/day1_accuracy.csv", row.names = F)
 