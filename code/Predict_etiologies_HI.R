
########################################
### Diarrhea etiology with Bob
### J Perin 
### November 2022
########################################

#####################################################################################################################
#####################################################################################################################

rm(list=ls())
Start<- Sys.time()



#### Analysis of samples
library(reshape2)
library(R2jags)
library(doParallel)
library(coda)
library(lattice)
library(foreign)
#library(saveJAGS)
library(readstata13)

###
###  Prepare datasets for R
###

#load(file="Make_input_data_20221220.RData")
load(file="../results/Make_Input_Data_HI_20231228.RData")
table(studies$year)
names(studies)
summary(studies$n_casesnorog2/studies$cases_tested_corr)
summary(studies$n_casescampy/studies$cases_tested_corr)
par(mar=c(5,4,4,2))
hist(studies$n_casesnorog2/studies$cases_tested_corr, breaks=20)
hist(studies$n_casescampy/studies$cases_tested_corr, breaks=20)

studies$pct_noro<-studies$n_casesnorog2/studies$cases_tested_corr
studies$pct_campy<-studies$n_casescampy/studies$cases_tested_corr
studies[order(-studies$pct_noro)[1:10], c("year","pct_noro", "cases_tested_corr")]
#studies[order(-studies$pct_campy)[1:10], c("year","pct_campy", "cases_tested_corr")]
studiesC<- studies[which(!is.na(studies$pct_campy)),]
studiesC[order(studiesC$year), c("year","pct_campy", "cases_tested_corr")]
#for diarrhea envelope
#de<- read.dta("../data/child_cod_2000-2019.dta")
de<- read.dta("../data/child_cod_2000-2021.dta")

#de<- de[which(de$year==2019 & de$level=="country"),c("iso3",#"whoname","whoreg6",
#                                                     "ufive3","u5d")]

head(de)
#table(data.predict$year)
#data.predict<- data.predict[which(data.predict$year==2019),]
data.predict<- merge(data.predict, de[,c("iso3","year","ufive3","u5d")], by=c("iso3","year"))
table(data.predict$year)


###################################X
#######   PREDICTIONS     ##########
###################################X
source("functions_predict_etiologies.R")  ##  functions

#Contains object named "out"
#load("Estimate_etiologies_HI_lam2_20230209_rota.RData")
#load("Estimate_etiologies_HI_20230410.RData")
load("../results/Estimate_etiologies_HI_l150.RData")

names(data.predict)
head(data.predict[,c("rota","u5mr","year","Iage_lower_lt24m","Iage_upper_24to59m")])
head(studies[,c("rota","u5mr","year","Iage_lower_lt24m","Iage_upper_24to59m")])



#check ranges/scales
for(i in out$param$VXF){
  print(i)
  print(summary(studies[,i]))
  print(summary(data.predict[,i]))
}

#convergence summary
summ<- as.data.frame(out$output$BUGSoutput$summary)
summary(summ$Rhat)
load("../results/Import.RData")
summary(dat$casestotal)
summary(rowSums(dat[,paste0("cases",etiologies)], na.rm=T))
summary(dat$casesnorog2)

#plot(dat$sample_size, dat$casesnorog2)
#function(MO, STUP, NP=500, IDV=c("isocode","year"), PPR=c(0, 0))
#iterations saved ,  chains, parameters
dim(out$output$BUGSoutput$sims.array)
p21 <- f.ci2(out, data.predict, nset=1200, id=c("iso3","year"))
dim(p21[[1]])
HI.draws<- p21$estF
dim(data.predict)
length(unique(data.predict$iso3))
#studies[which(studies[,vxr] %in% data.predict[,vxr]),vxr]
#studies[which(studies[,vxr] %in% data.predict[,vxr]),"study_id"]
dim(p21[[1]])
head(p21[[1]])
#rownames(p21[[1]])
#head(data.predict)

####################################
#######   WHO REGIONS   ############
####################################
#codes<- read.dta13("../../other_inputs/codelist region.dta")
#head(codes)
#codes$isocode<- codes$iso3


library(arrayhelpers)








head(p21[[1]][,,1])

NSim<- dim(p21[[1]])[3]
library(plyr)
############################################################

est<-apply(p21[[1]], c(1,2), FUN=mean)

Eest<- as.data.frame(est)
head(Eest)
Eest$iso3<- data.predict$iso3
Eest$year<- data.predict$year
table(Eest$year)
Eest$whoreg6<- data.predict$whoreg6
Eest$whoname<- data.predict$whoname
Eest$ufive3<- data.predict$ufive3
Eest$rota<- data.predict$rota
Eest$u5mr<- data.predict$u5mr
Eest$High.income<- data.predict$High.income
head(Eest)
table(Eest$year)
table(Eest$High.income)
unique(Eest$High.income)
Eest<- Eest[which(Eest$High.income==1),]
table(Eest$year)
summary(Eest)
head(Eest)

dim(data.predict)
# assuming cholera is zero
Eest$n_caseschol<- 0
save(Eest, HI.draws, file="../results/Predict_etiologies_HI.RData")
head(Eest)
