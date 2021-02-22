# calculate accuracy on contingencies on day 2
rm(list=ls())
library(dplyr)# select comes from this
library(ggplot2)
library(lme4)
library(lmerTest)
source("helper_functions/selPart.R")
source("helper_functions/getflow.R")
source("helper_functions/getcorrFlow.R")


# sel files
cd<-getwd()
setwd("data_files")
files<-selPart(2)
setwd(cd)

# 2. calculate cumulative accuracy per participant and reaction times
AccDay2<-data.frame(SubNum=vector(), butterfly=character(), response = vector(), respFlow = character(), corrFlow = character (), accuracy=vector(), cumAcc=vector(),
                    RT=vector(), trialNum<-vector(),  listN=vector(), tfromchange=vector() , befAft<-vector(), befAft2<-vector(),
                    trialCond=vector(), image=character())
for (j in 1 : (length(files))){
  
  tryCatch({
    
  # load the files  
  day2<-read.csv(paste("data_files/", files[j], sep=""))
  
  #convert all the flowers column to character
  for (c in 17:20){
    day2[[c]]<-as.character(day2[[c]])
  }
  
  # assign the right list and the number of trials since the last change
  switchN<-c(99, 2, 3,4,5)
  
  cleanList<-vector()
  for (s in 1 : length(switchN)){
  
    currlist<-day2[day2$switch_cond==switchN[s],]
    # delete NAs
    currlist<-currlist[!is.na(currlist$switch_cond),]
    
    # tak only the congruent trials
    currlist<-currlist[currlist$trial_cond==1,]
    


    # delete butterfly if s==1
    # if (s == 1  ){
    # currlist<-currlist[currlist$obj_file!="stimuli/objects/butterfly5.jpg"  & currlist$obj_file!="stimuli/objects/butterfly2.jpg" ,]
    # 
    #  }
    currlist$tfromchange<-seq(1:nrow(currlist))
    
    # for create the variable before-after, we need to create and exception for list one, which is composed by 15 trials
      if (s==1){
    currlist$befAft<-c(rep(0, times=8), rep(1, times=7))
        
      } else {
      
    
    currlist$befAft<-rep(c(0,1), each=(nrow(currlist)/2))
      }
    currlist$befAft2<-rep(c(0, 1, 2), each = nrow(currlist)/3)

    
    if (s==1){
      currlist$listN<-0
      # report the butterfly
      currlist$butterfly<-NA
      # report the flower selected
      currlist$respFlower<-NA
      for (t in 1: nrow(currlist)){
        currlist$butterfly[t]<-substr(currlist$cuedButter[t], 9, (nchar(as.character(currlist$cuedButter[t]))-4))
        # get the selected flower
        if (nchar(as.character(currlist$warmup_resp.keys[t]))>1){
        selFlow<-getflow(currlist$warmup_resp.keys[t])
        currlist$respFlower[t]<-substr(currlist[t,selFlow], 9, nchar(currlist[t,selFlow])-4)
        } else { currlist$respFlower[t]<-NA}
      }
      currlist$response<-currlist$warmup_resp.keys
      
      # calculate accuracy
      currlist$accuracy<-currlist$warmup_resp.corr
      currlist$RT<-currlist$warmup_resp.rt
      
      
      cleanList<-rbind(cleanList, currlist)
      

      
    } else {
      
      currlist$listN<-s
      # report the butterfly
      currlist$butterfly<-NA
      # report the flower selected
      currlist$respFlower<-NA
      for (t in 1: nrow(currlist)){
        currlist$butterfly[t]<-substr(currlist$cuedButter[t], 9, (nchar(as.character(currlist$cuedButter[t]))-4))
        # get the selected flower
        # only if it is not null
        if (nchar(as.character(currlist$task_resp_3.keys[t]))>1){
        selFlow<-getflow(currlist$task_resp_3.keys[t])
        currlist$respFlower[t]<-substr(currlist[t,selFlow], 9, nchar(currlist[t,selFlow])-4)
        } else {currlist$respFlower[t]<-NA}
      }
      currlist$response<-currlist$task_resp_3.keys
      
      currlist$accuracy<-currlist$task_resp_3.corr
      currlist$RT<-currlist$task_resp_3.rt
      cleanList<-rbind(cleanList, currlist)
   
    }
  }
  
  
  SubNum<-rep(as.numeric(substr(files[j], 1,2)), times=nrow(cleanList))
  
  #task1$myownaccuracy<-as.numeric(as.character(task1$myownaccuracy))
  cleanList$cumAcc<-c(cummean(cleanList$warmup_resp.corr[cleanList$switch_cond==99]), 
                                      cummean(cleanList$task_resp_3.corr[!is.na(cleanList$task_resp_3.corr)]))
  cleanList$trialNum<-seq(1:nrow(cleanList))
  objfile<-substr(cleanList$obj_file, 17, (length(cleanList$obj_file)-4))

  # get the correct flower
  cleanList<-getcorrFlow(cleanList)
  
  partData<-data.frame(cbind(SubNum,cleanList$butterfly,cleanList$response, cleanList$respFlower, cleanList$corr_flower, cleanList$accuracy, cleanList$cumAcc, cleanList$RT, 
                             cleanList$trialNum, cleanList$listN, cleanList$tfromchange,
                             cleanList$befAft ,cleanList$befAft2, cleanList$trial_cond, objfile))
  names(partData)[c(2,3,4,5,6,7,8, 9, 10,11,12, 13, 14)]<-c("butterfly","response", "respFlower" ,"corrFlower", "accuracy",  "cumAcc", "RT","trialNum","listN",
                                       "NfromChange", "befAft", "befAft2","trialCond", "image")
  AccDay2<-rbind(AccDay2, partData)

  },error=function(e){cat("ERROR :",conditionMessage(e), "\n", j)}) 
}

count<-AccDay2 %>% group_by(trialNum)%>%
  tally()

# plot cumACC
# by participant and list N
# first convert factor as numeric
for (c in 5:10){
  AccDay2[[c]]<-as.numeric(as.character(AccDay2[[c]]))
}

AccDay2$listN<-as.factor(AccDay2$listN)

ggplot(AccDay2, aes(x=trialNum, y = cumAcc, color=listN))+ geom_line()+
  geom_vline(xintercept = c(18, 54,90, 126))+
  facet_wrap(.~SubNum)

# plot accuracy
ggplot(AccDay2, aes(x=trialNum, y = accuracy, color=listN))+ geom_line()+
  geom_vline(xintercept = c(18, 54,90, 126))+
  facet_wrap(.~SubNum)

# plot RT
# by participant
ggplot(AccDay2, aes(x=trialNum, y = RT, color=listN))+ geom_line()+
  geom_vline(xintercept = c(18, 54,90, 126))+
  facet_wrap(.~SubNum)

# print the file
write.csv(AccDay2, "output_files/day2_accuracy.csv", row.names = F)

# exclude 10
#excl<-c( 10)
#AccDay2<-AccDay2[!AccDay2$SubNum %in% excl, ]

# analyse before and after change
# first exclude the list 0
datacomp<-AccDay2[AccDay2$listN!=0,]
# rename the levels
levels(datacomp$befAft)<-c("afterCP", "beforeCP")
levels(datacomp$befAft2)<-c("afterCP", "middle", "beforeCP")

#datacomp$befAft<-relevel(datacomp$befAft, ref="beforeCP")

# change reference level
datacomp$befAft<-relevel(datacomp$befAft, ref="afterCP")
# change reference level
datacomp$befAft2<-relevel(datacomp$befAft2, ref="afterCP")
str(datacomp)
# ggplot
ggplot(datacomp, aes(befAft, accuracy))+ geom_bar(aes(befAft, accuracy, fill = befAft),
                                                    position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  #facet_grid(.~rec_session)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab("Accuracy")+
  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

ggplot(datacomp, aes(befAft2, accuracy))+ geom_bar(aes(befAft2, accuracy, fill = befAft2),
                                                  position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  #facet_grid(.~rec_session)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab("Accuracy")+
  #scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

# now reaction times
ggplot(datacomp, aes(befAft, RT))+ geom_bar(aes(befAft, RT, fill = befAft),
                                                  position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  #facet_grid(.~rec_session)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab("Reaction Time")+
  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

ggplot(datacomp, aes(befAft2, RT))+ geom_bar(aes(befAft2, RT, fill = befAft2),
                                            position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  #facet_grid(.~rec_session)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab("Reaction Time")+
  #scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

# are they significant?
accmod<-glmer(accuracy~befAft+(1|SubNum),family = binomial(), data=datacomp)
summary(accmod)

rtmod<-lmer(RT~befAft+(1|SubNum), data=datacomp)
summary(rtmod)

# do it by block
# Accuracy
ggplot(datacomp, aes(befAft, accuracy))+ geom_bar(aes(befAft, accuracy, fill = befAft),
                                                  position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  facet_grid(.~listN)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab("Accuracy")+
  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

# RT
ggplot(datacomp, aes(befAft, RT))+ geom_bar(aes(befAft, RT, fill = befAft),
                                                  position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  facet_grid(.~listN)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab("Reaction Time")+
  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")
 
ggplot(datacomp, aes(befAft2, RT))+ geom_bar(aes(befAft2, RT, fill = befAft2),
                                            position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  facet_grid(.~listN)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab("Reaction Time")+
  #scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")
