
#############
########### recogn
rm(list=ls())
library(dplyr)
library(ggplot2)
library(lme4)
library(car)
source("helper_functions/selPart.R")



# write the file
recogData<-read.csv( "output_files/recognitionData.csv")

# now I want personalised switch points
subjects<-unique(recogData$SubNum)
recogData$switchpoint<-NA
for (n in 1:length(subjects)){
  # subset the data
  sub<-recogData[recogData$SubNum==subjects[n],]
  
  firsttrails<-rep(0, times = nrow(sub[sub$listN==3,] )+nrow(sub[sub$listN==0,] ))
  # selectfirst switch
  trialfirstswitch<-sub[sub$listN==3,]
  # which one is the first trial that is accurate after the switch?
  firstswitchpoint<-which(trialfirstswitch$accuracy==1)[1]
  
  secondtrials<-c(rep(0, times = firstswitchpoint-1), rep(1, times=nrow(sub[sub$listN==3,])-(firstswitchpoint-1)))
  
  #secondtrials<-c(rep(0, times = firstswitchpoint), rep(1, times=nrow(sub[sub$listN==3,])-(firstswitchpoint)))
  
  
  trialsecondswitch<-sub[sub$listN==4,]
  secondswitchpoint<-which(trialsecondswitch$accuracy==1)[1]
  
  #thirdstrials<-c(rep(0, times = secondswitchpoint-1), rep(1, times=nrow(sub[sub$listN==4,])-(secondswitchpoint-1)))
  thirdstrials<-c(rep(0, times = secondswitchpoint), rep(1, times=nrow(sub[sub$listN==4,])-(secondswitchpoint)))
  
  trialthirdswitch<-sub[sub$listN==5,]
  thirdswitchpoint<-which(trialthirdswitch$accuracy==1)[1]
  
  fourthtrials<-c(rep(0, times = thirdswitchpoint-1), rep(1, times=nrow(sub[sub$listN==5,])-(thirdswitchpoint-1)))
  #fourthtrials<-c(rep(0, times = thirdswitchpoint), rep(1, times=nrow(sub[sub$listN==5,])-(thirdswitchpoint)))
  
  
  # merge them together
  trails<-c(firsttrails, secondtrials, thirdstrials,fourthtrials)
  
  recogData$switchpoint[recogData$SubNum==subjects[n]]<-trails
  
}

recogData$switchpoint<-as.factor(recogData$switchpoint)
levels(recogData$switchpoint)<-c("afterCP", "beforeCP")

recogData$accuracy<-as.factor(recogData$accuracy)

recogData$switchpoint<-relevel(recogData$switchpoint, ref="afterCP")

recogData<-recogData[recogData$befAft!=2,]

# plot it # we are looking for an interaction between feedback and personalised-switch point
ggplot(recogData[recogData$listN!=2 ,], aes(x=switchpoint, y=recogAcc))+ geom_bar(aes(switchpoint, recogAcc, fill=switchpoint),
                                                                              position="dodge",stat="summary", fun.y="mean")+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  facet_wrap(.~accuracy)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab(" % Hit")+
  # xlab("feedback")+
  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

# aggregate
ggplot(recogData[recogData$listN!=2 ,], aes(x=switchpoint, y=recogAcc))+ geom_bar(aes(switchpoint, recogAcc, fill=switchpoint),
                                                                                  position="dodge",stat="summary", fun.y="mean")+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  #facet_wrap(.~accuracy)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab(" % Hit")+
  # xlab("feedback")+
  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")


# is that significant?
switchpointacc<-glmer(recogAcc~switchpoint+(1|SubNum), family=binomial(), data= recogData)
summary(switchpointacc)

# does switchpoint explain additional variance compared to accuracy>
modacc<-glmer(recogAcc~accuracy+(1+accuracy|SubNum), family=binomial(), data= recogData)
summary(modacc)

modswitchpoint<-glmer(recogAcc~accuracy+switchpoint+(1+accuracy+switchpoint|SubNum), family=binomial(), data= recogData)
summary(modswitchpoint)

anova(modacc, modswitchpoint)
