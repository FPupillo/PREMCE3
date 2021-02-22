
#############
########### recogn
rm(list=ls())
library(dplyr)
library(ggplot2)
library(lme4)
library(car)
source("helper_functions/selPart.R")

# sel files
cd<-getwd()
setwd("data_files")
files<-selPart(3)
setwd(cd)

# retrieve file
day2data<-read.csv("output_files/day2_accuracy.csv")

str(day2data)

# retrieve file for recognition 
# recogData<-data.frame(SubNum=vector(), acc=vector(), RT=vector(), 
#                       confidence=vector(), confRT<-vector(), trialType<-vector())
longDatarecog<-vector()
recogData<-vector()
for (j in 2: length(files)){
  tryCatch({
  recog<-read.csv(paste("data_files/", files[j], sep=""))
  recog <- select(recog,recog_resp.keys,recog_resp.rt,conf_resp.keys,conf_resp.rt,images,corr_ans, images)
  
  # delete Na
  recog<-recog[!is.na(recog$recog_resp.rt),]
  
  # compute accuracy
  recog$acc<-NA
  for (n in 1 :nrow(recog)){
    if (recog$recog_resp.keys[n]==recog$corr_ans[n]){
      recog$acc[n]<-1
    }else{recog$acc[n]<-0} 
  }
  
  # calculate hit, miss, rej, and FA
  for ( i in 1:nrow(recog)){
    if (recog$corr_ans[i]=="left"& recog$recog_resp.keys[i]=="left" ){
      recog$type[i]<-"HIT"
    } else if (recog$corr_ans[i]=="left"& recog$recog_resp.keys[i]=="right" ){
      recog$type[i]<-"Miss"
    } else if(recog$corr_ans[i]=="right"& recog$recog_resp.keys[i]=="right" ){
      recog$type[i]<-"CorrRej"
    } else if (recog$corr_ans[i]=="right"& recog$recog_resp.keys[i]=="left" ){
      recog$type[i]<-"FA"
    }
  }
  
  # now scoring for recognition each image
  # first subset the day2 data
  SubNum<-as.numeric(substr(files[j], 1,2))
  day2Sub<-day2data[day2data$SubNum==SubNum,]
  # delete task 0
  day2Sub<-day2Sub[day2Sub$listN!=0,]
  
  names(day2Sub)[15]<-"image"
  
  # delete all the butterflies
  day2Sub<-day2Sub[(day2Sub$image!= "butterfly1.jpg" & day2Sub$image!= "butterfly2.jpg"&
                     day2Sub$image!= "butterfly3.jpg" & day2Sub$image!= "butterfly4.jpg" &
                      day2Sub$image!= "butterfly5.jpg"), ]
  
  day2Sub$image<-as.character(day2Sub$image)
  # then substring the images in the actual file
  recog$imageName<-substr(recog$images, 17, (length(recog$images)-4))

  # take the accuracy of the recognition and add it to day2Sub
  for (i in 1: nrow(day2Sub)){
    day2Sub$recogAcc[i]<-recog$acc[recog$imageName==day2Sub$image[i]]
    
    # add confidence
    day2Sub$confidence[i]<-recog$conf_resp.keys[recog$imageName==day2Sub$image[i]]
    
  }
  
  longDatarecog<-rbind(longDatarecog, day2Sub)
  recog$SubNum<-SubNum
  recogData<-rbind(recogData, recog)
  },error=function(e){}) 
}


count<- longDatarecog %>% 
  group_by(trialNum)%>%
  tally()


# write the file
write.csv(longDatarecog, "output_files/recognitionData.csv", col.names = F)

VoI<-c("SubNum", "type")
# wide dataset
wideData<-table(recogData[,VoI])

# convert it to a data.frame
wideData<-as.data.frame.matrix(wideData)

# include row names
wideData$SubNum<-rownames(wideData)

# reorder the columns to make the id variable the first one
wideData<-wideData[, c(5,1:4)]

# get rid of the rownames
rownames(wideData) <- c()

# compute percentage HIT
wideData$HITrate<-wideData$HIT/(wideData$HIT+wideData$Miss)

# percentage false alarm
wideData$FArate<-wideData$FA/(wideData$FA+wideData$CorrRej)

# calculate dprime
library(psycho)

indices <- psycho::dprime(wideData$HIT, wideData$FA, wideData$Miss, wideData$CorrRej)
wideData<-cbind(wideData, indices)


# delete dtafrom participant 2
#longDatarecog<-longDatarecog[!longDatarecog$SubNum %in% c(5, 10), ]


# now check the effect of switch condition
# convert the variable as factor
longDatarecog$befAft<-as.factor(longDatarecog$befAft)
levels(longDatarecog$befAft)<-c("afterCP", "beforeCP")


longDatarecog$befAft
ggplot(longDatarecog, aes(befAft, recogAcc))+ geom_bar(aes(befAft, recogAcc, fill = befAft),
                                                           position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  #facet_grid(.~rec_session)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab(" % Hit")+
 # xlab("feedback")+
   scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

# is that significant?
befAftAccMod<-glmer(recogAcc~befAft+(1+befAft|SubNum), family=binomial(), data= longDatarecog)
summary(befAftAccMod)

# excluding the first list
ggplot(longDatarecog[longDatarecog$listN!=2,], aes(befAft, recogAcc))+ geom_bar(aes(befAft, recogAcc, fill = befAft),
                                                       position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  #facet_grid(.~rec_session)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab(" % Hit")+
  # xlab("feedback")+
  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

# by subject
ggplot(longDatarecog[longDatarecog$listN!=2,], aes(befAft, recogAcc))+ geom_bar(aes(befAft, recogAcc, fill = befAft),
                                                                                position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  facet_wrap(.~SubNum)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab(" % Hit")+
  # xlab("feedback")+
  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

# analyse
# null model

Null<-glmer(recogAcc~+(1|SubNum), family=binomial(), 
            data= longDatarecog[longDatarecog$listN!=2,])
summary(Null)

# add random intercept for images
imagemod<-glmer(recogAcc~+(1|SubNum)+(1|image), family=binomial(), data= longDatarecog[longDatarecog$listN!=2,])
summary(imagemod)

# compare
anova(Null, image)

imagedata<-longDatarecog %>%
  group_by ( image) %>%
  summarise(mean=mean(recogAcc, na.rm=T), sd = sd(recogAcc, na.rm=T),)

ggplot(longDatarecog, aes(image, recogAcc))+ geom_bar(aes(image, recogAcc, fill = image),
                                                      position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  #facet_wrap(.~accuracy)+
  theme(strip.text.x = element_text(size = 13))+ 
  #ylab(" % Hit")+
  # xlab("feedback")+
  #scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred", "middle" = "darkgreen"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

# analyse the effect of change point

CPrnslop<-glmer(recogAcc~befAft+(1+befAft|SubNum)+(1|image), family=binomial(), data= longDatarecog[longDatarecog$listN!=2,])
summary(CPrnslop)

anova(CP, CPrnslop)

CP<-glmer(recogAcc~befAft+(1|SubNum)+(1|image), family=binomial(), data= longDatarecog[longDatarecog$listN!=2,])
summary(CP)


### now calculate hits per accuracy (0,1)
# accuracy as factor
longDatarecog$accuracy<-as.factor(longDatarecog$accuracy)
levels(longDatarecog$accuracy)<-c("incorrect", "correct")
# ggplot
ggplot(longDatarecog, aes(accuracy, recogAcc))+ geom_bar(aes(accuracy, recogAcc, fill = accuracy),
                                                         position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  #facet_grid(.~rec_session)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab("% HIT")+
  xlab("feedback")+
  #  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

# is that significant
feedbMod<-glmer(recogAcc~accuracy + (1|SubNum), family= binomial(), 
                data= longDatarecog[longDatarecog$listN!=2,])
summary(feedbMod)

anova(Null, feedbMod)

feedbModslope<-glmer(recogAcc~accuracy + (1+accuracy|SubNum), family= binomial(), 
                     data= longDatarecog[longDatarecog$listN!=2,])

anova(feedbMod, feedbModslope)

accCPmode<-glmer(recogAcc~accuracy + befAft+ (1|SubNum), family= binomial(), 
                 data= longDatarecog[longDatarecog$listN!=2,])

anova(accCPmode, feedbMod)


# let's do it by participant
ggplot(longDatarecog, aes(accuracy, recogAcc))+ geom_bar(aes(accuracy, recogAcc, fill = accuracy),
                                                         position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  facet_wrap(.~SubNum)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab("% HIT")+
  xlab("feedback")+
  #  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

# let's see confidence ratings
ggplot(longDatarecog, aes(accuracy, confidence))+ geom_bar(aes(accuracy, confidence, fill = accuracy),
                                                           position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  #facet_grid(.~rec_session)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab("Mean Confidence")+
  xlab("feedback")+
  ylim(0, 4)+
  
  #  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

# is that significant?
confAccMod<-lmer(confidence~accuracy+(1|SubNum), data= longDatarecog)
summary(confAccMod)


# by participant
ggplot(longDatarecog, aes(accuracy, confidence))+ geom_bar(aes(accuracy, confidence, fill = accuracy),
                                                           position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  facet_wrap(.~SubNum)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab("Mean Confidence")+
  xlab("feedback")+
  #  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

# confidence ratings by accuracy by recog accuracy
ggplot(longDatarecog, aes(accuracy, confidence))+ geom_bar(aes(accuracy, confidence, fill = accuracy),
                                                           position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  facet_wrap(.~recogAcc)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab("Mean Confidence")+
  xlab("feedback")+
  #  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

# reaction times and accuracy
ggplot(longDatarecog[longDatarecog$listN!=2,], aes(RT, recogAcc))+
  geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), se=F)+aes(colour = factor(SubNum))+
  geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), colour="black", se=T)+
  theme(strip.text.x = element_text(size = 13))+
  facet_wrap(.~accuracy)


# we are looking for an interaction between feedback and before-after
ggplot(longDatarecog[longDatarecog$listN!=2,], aes(befAft, recogAcc))+ geom_bar(aes(befAft, recogAcc, fill = befAft),
                                                                                position="dodge",stat="summary", fun.y="mean", SE=T)+
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

# test the interaction
accCPint<-glmer(recogAcc~accuracy * befAft+ (1|SubNum), family= binomial, 
                 data= longDatarecog[longDatarecog$listN!=2,])

summary(accCPint)



# try to add the middle point
longDatarecog$befAft2<-as.factor(longDatarecog$befAft2)
levels(longDatarecog$befAft2)<-c("afterCP", "middle", "beforeCP")

ggplot(longDatarecog[longDatarecog$listN!=2,], aes(befAft2, recogAcc))+ geom_bar(aes(befAft2, recogAcc, fill = befAft2),
                                                                                position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  facet_wrap(.~accuracy)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab(" % Hit")+
  # xlab("feedback")+
  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred", "middle" = "darkgreen"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

# by block
ggplot(longDatarecog, aes(befAft2, recogAcc))+ geom_bar(aes(befAft2, recogAcc, fill = befAft2),
                                                                                 position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  facet_grid(listN~accuracy)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab(" % Hit")+
  # xlab("feedback")+
  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred", "middle" = "darkgreen"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

# by participant
ggplot(longDatarecog, aes(befAft2, recogAcc))+ geom_bar(aes(befAft2, recogAcc, fill = befAft2),
                                                        position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  facet_grid(SubNum~accuracy)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab(" % Hit")+
  # xlab("feedback")+
  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred", "middle" = "darkgreen"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")


# do the same but for high confidence
ggplot(longDatarecog[longDatarecog$confidence==4 | longDatarecog$confidence==5,], 
       aes(befAft2, recogAcc))+ geom_bar(aes(befAft2, recogAcc, fill = befAft2),
                                                        position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  facet_grid(listN~accuracy)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab(" % Hit")+
  # xlab("feedback")+
  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred", "middle" = "darkgreen"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

# check the images
sel<-longDatarecog[longDatarecog$listN==3 & longDatarecog$accuracy=="correct"& longDatarecog$befAft2=="afterCP",]
sel$image


# test it
accCPintmid<-glmer(recogAcc~accuracy * befAft2+ (1|SubNum), family= binomial(), 
                data= longDatarecog[longDatarecog$listN!=2,])

summary(accCPintmid)

Anova(accCPintmid)

accCPintmidnoint<-glmer(recogAcc~accuracy + befAft2+ (1|SubNum), family= binomial(), 
                   data= longDatarecog[longDatarecog$listN!=2,])

summary(accCPintmidnoint)

Anova(accCPintmidnoint)

anova(feedbMod,accCPintmidnoint )

# see if the effects stays without accuracy
accmod<-glmer(recogAcc~accuracy + (1|SubNum), family= binomial(), 
              data= longDatarecog[longDatarecog$listN!=2,])

CPmod<-glmer(recogAcc~accuracy * befAft2+ (1|SubNum), family= binomial(), 
             data= longDatarecog[longDatarecog$listN!=2,])

anova(accmod, CPmod)

# let's account for images
accuracyimagemod<-glmer(recogAcc~accuracy + (1|SubNum)+(1|image), family= binomial(), 
                data= longDatarecog[longDatarecog$listN!=2,])

accCPimagemod<-glmer(recogAcc~accuracy +befAft2+ (1|SubNum)+(1|image), family= binomial(), 
                     data= longDatarecog[longDatarecog$listN!=2,])

anova(accuracyimagemod, accCPimagemod)

# let's account for images: only block three for correct
accuracyimagemod<-glmer(recogAcc~befAft2 + (1|SubNum)+(1|image), family= binomial(), 
                        data= longDatarecog[longDatarecog$listN==3 & longDatarecog$accuracy=="correct",])

summary(accuracyimagemod)
# plot predicted
library(ggeffects)
predicted<-ggpredict(accCPimagemod, c("befAft2", "accuracy"))
plot(predicted)
#########################################################################
# get the effect by participant
# first accuracy

randeffacc<-glmer(recogAcc~accuracy + (1+accuracy|SubNum), family= binomial(), 
 data= longDatarecog[longDatarecog$listN!=2,])

betaacc<-(coef(randeffacc)$SubNum)$accuracy

# now change point
# first change the reference level to before CP
longDatarecog$befAft2<- factor(longDatarecog$befAft2, levels=c("afterCP", "middle", "beforeCP"))

randeffCP<-glmer(recogAcc~befAft2 + (1+befAft2|SubNum), family= binomial(), 
                   data= longDatarecog[longDatarecog$listN!=2,])

betaCP<-(coef(randeffCP)$SubNum)$befAft2middle

# are they correlated?
cor.test(betaacc, betaCP)

plot(betaacc, betaCP)
abline(lm(betaCP~betaacc))


# let's try the interaction
intslope<-glmer(recogAcc ~ accuracy * befAft2 + (1+accuracy * befAft2 | SubNum), family= binomial(), 
                data= longDatarecog[longDatarecog$listN!=2,])

intcoeff<-(coef(intslope)$SubNum)$`accuracycorrect:befAft2middle`

# are they correlated?
cor.test(betaacc, intcoeff)
plot(betaacc, intcoeff)
abline(lm(betaCP~betaacc))


# let's do if with middle point as reference
longDatarecog$befAft2<- factor(longDatarecog$befAft2, levels=c("middle", "afterCP", "beforeCP"))
intslope<-glmer(recogAcc ~ accuracy * befAft2 + (1+accuracy * befAft2 | SubNum), family= binomial(), 
                data= longDatarecog[longDatarecog$listN!=2,])

accMidd<-(coef(intslope)$SubNum)$`accuracycorrect`

# are they correlated?
cor.test(betaacc, intcoeff)
plot(betaacc, intcoeff)

# between accuracy sensitibvity and change point
cor.test(betaCP, intcoeff)
plot(betaCP, accMidd)



# do it only for high confidence






# now by time from change point
ggplot(longDatarecog[longDatarecog$listN!=2,], aes(NfromChange, recogAcc))+
  geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), se=F)+aes(colour = factor(SubNum))+
  geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), colour="black", se=T)+
  theme(strip.text.x = element_text(size = 13))+
  facet_wrap(.~accuracy)
  
# model it
modTrial<-glmer(recogAcc~NfromChange*accuracy+(1+ NfromChange|SubNum), family=binomial(),
                data=longDatarecog[longDatarecog$listN!=2,])

summary(modTrial)


########################################################################
# convert accuracy
longDatarecog$accuracy<-as.numeric(as.character(longDatarecog$accuracy))
mean_data <- group_by(longDatarecog, trialNum) %>%
  summarise(recogAcc = mean(recogAcc, na.rm = TRUE), accuracy = mean(accuracy, na.rm = TRUE))

mean_data <- group_by(longDatarecog, trialNum) %>%
  summarise(across(c("recogAcc", "accuracy"), list(mean)))


# plot performance by trial
ggplot(mean_data, aes(x=trialNum, y= recogAcc))+
  geom_line()+
  geom_vline(xintercept = c(18, 55,90, 126))

ggplot(longDatarecog, aes( y = recogAcc))+
  geom_line(aes(x=trialNum), color= "red")
#  geom_line(aes(y=recogAcc), color ="green")
  
ggplot(mean_data, aes( x = trialNum))+
  geom_line(aes(y=recogAcc), color= "red")+
  geom_line(aes(y=accuracy), color= "blue")+
  geom_vline(xintercept = c(18, 55,90, 126))


#stat_summary(fun.data = "mean_cl_boot", geom = "smooth")
stat_summary(fun.data ="mean_sdl", mult=1, geom = "smooth")+
  geom_vline(xintercept = c(18, 55,90, 126))

  










generalacc<-mean(recog$acc)

# only hits
AccHIT<-mean(recog$acc[recog$corr_ans=="left"])

#only confidence  
AccHIThighConf<-mean(recog$acc[recog$corr_ans=="left" & (recog$conf_resp.keys==3|recog$conf_resp.keys==4)  ])

ACCcorrrej<-mean(recog$acc[recog$corr_ans=="right"])

FA<-nrow(recog[recog$recog_resp.keys=="left" & recog$corr_ans=="right",])
FArate<-(FA/160)*100


# calculate hits and false alarms
recog$type<-NA
for ( i in 1:nrow(recog)){
  if (recog$corr_ans[i]=="left"& recog$recog_resp.keys[i]=="left" ){
    recog$type[i]<-"HIT"
  } else if (recog$corr_ans[i]=="left"& recog$recog_resp.keys[i]=="right" ){
    recog$type[i]<-"Miss"
  } else if(recog$corr_ans[i]=="right"& recog$recog_resp.keys[i]=="right" ){
    recog$type[i]<-"CorrRejRear"
  } else if (recog$corr_ans[i]=="right"& recog$recog_resp.keys[i]=="left" ){
    recog$type[i]<-"FA"
  }
}

table<-table(recog$type)

table/160

