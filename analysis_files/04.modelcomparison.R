# model comparison: compare fit
# this script compare the BIC of the different models

library(lme4)
library(lmerTest)
library(car)
library(reshape2)
library(ggplot2)
library(dplyr)
library(ggjoy)

rm(list=ls())
cd<-getwd()
# create dataset to store the bic
BicAll<-data.frame(matrix(NA, nrow=10, ncol=0))

# retrieve the different models
setwd("output_files")
names<-list.files()
# 
# modelCK<-read.csv("ParameterEstimation.modelCK.csv")
# modelObs<-read.csv("ParameterEstimation.modelobs.csv")
# modelObsALL<-read.csv("ParameterEstimation.modelObsALL.csv")
# modelfeedb0<-read.csv("ParameterEstimation.modelfeedbRW0.csv")
# modelfeedb0.3<-read.csv("ParameterEstimation.modelfeedbRW0.3.csv")
# modelfeedbQ<-read.csv("ParameterEstimation.modelfeedbRWQ.csv")
# modelfeedbCK<-read.csv("ParameterEstimation.modelfeedbRWCK.csv")
# modelfeedbObs<-read.csv("ParameterEstimation.modelfeedbRWObs.csv")

modelfeedb<-read.csv("estimated_parameters_feedb.csv")
modelObs<-read.csv("estimated_parameters_obsALL.csv")
modelBayesian<-read.csv("estimated_parameters_bayesian.csv")

BicAll$Sub<-seq(1:10)
BicAll$modelfeedb<-modelfeedb$BIC
BicAll$modelObs<-modelObs$BIC
BicAll$modelBayesian<-modelBayesian$BIC


# get a summary
summary(BicAll[2:ncol(BicAll)])

par(mfrow=c(3,3))
for (v in 2:ncol(BicAll)){
  hist(BicAll[, v], main = names(BicAll)[v])
}
par(mfrow=c(1,1))

BicAllmelt<-melt(BicAll, id.vars = "Sub")
names(BicAllmelt)<-c("Sub", "model", "BIC")

BicMod<-lmer(BIC~model+(1|Sub), data = BicAllmelt)
summary(BicMod)

# summary by model
BicAllmelt %>%
  group_by(model)   %>%
  summarize(Mean = mean(BIC, na.rm=TRUE))

# plot them
p <- ggplot(BicAllmelt, aes(model, BIC))
p+
  geom_violin()+
  geom_boxplot()+
  geom_jitter()

BicAllmelt %>%
  mutate(group = reorder(model, BIC, median)) %>%
  ggplot(aes(x=BIC, y= model, height= ..density..))+
  geom_joy(scale=0.85)

# Count for how many participants a precise model was the best fit
BicAll$BestModel<-NA
for (j in 1: nrow(BicAll)){
  index<-which(BicAll[j,]==min(BicAll[j,2:4]))
  BicAll$BestModel[j]<-names(BicAll[index])
}


table(BicAll$BestModel)

ggplot(BicAll, aes(BestModel))+geom_bar()

# now loglikelihood
LL<-data.frame(matrix(NA, nrow=10, ncol=0))

LL$Sub<-seq(1:10)

LL$modelfeedb<-modelfeedb$LL
LL$modelObs<-modelObs$LL
LL$modelBayesian<-modelBayesian$LL


LLmelt<-melt(LL, id.vars = "Sub")
names(LLmelt)<-c("Sub", "model", "LL")

LLMod<-lmer(LL~model+(1|Sub), data = LLmelt)
summary(LLMod)

# summary by model
LLmelt %>%
  group_by(model)   %>%
  summarize(Mean = mean(LL, na.rm=TRUE))

# Count for how many participants a precise model was the best fit
LL$BestModel<-NA
for (j in 1: nrow(LL)){
  index<-which(LL[j,]==max(LL[j,2:4]))
  LL$BestModel[j]<-names(LL[index])
}


table(LL$BestModel)

ggplot(LL, aes(BestModel))+geom_bar()


