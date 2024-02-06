
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
#load(file="Make_Input_Data_20230109.RData")
#load(file="Make_Input_Data_20230410.RData")
# added regions
load(file="../results/Make_Input_Data_20231228.RData")
table(data.predict$year)
table(studies$year)
dim(studies)
table(studies$I_Africa)
table(studies$I_Eur)
table(studies$I_Emr)
table(studies$I_Sear)
table(studies$I_Amr)
table(studies$I_Wpr)
#for diarrhea envelope
#/Users/jamieperin/Documents/Projects/COD/COD_2022/Under five 2022/childcod2000-2021_openaccess
de<- read.dta("../data/child_cod_2000-2021.dta")
#de<- de[which(de$year==2019 & de$level=="country"),c("iso3",#"whoname","whoreg6",
#                                                     "ufive3","u5d")]
table(de$year)
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

#load("Estimate_etiologies_lam2_sd04_20230206.RData")
#load("Estimate_etiologies_lam2_sd04_20230410.RData")

# With RE, lambda 325
#load("Estimate_etiologies_region_20230726_l325.RData")

# With no RE, lambda 125
lam<- 75
load(paste0("../results/Estimate_etiologies_l",lam,".RData"))
summary(out$output$BUGSoutput$summary)
summary(studies)
cor(studies$year, studies$rota)
names(data.predict)
head(data.predict[,c("rota","u5mr","Iage_lower_lt24m","Iage_upper_24to59m")])
head(studies[,c("rota","u5mr","Iage_lower_lt24m","Iage_upper_24to59m")])



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
head(out$output$BUGSoutput$sims.array[1,,])

p21 <- f.ci2(out, data.predict, nset=500, id=c("iso3","year"),PRAN=F)
names(p21)
dim(p21[[1]])
dim(p21[[2]])
dim(data.predict)
LMIC.draws<- p21$estF
dim(p21$estF)





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
ets<-colnames(est)

dim(est)
names(p21)
dim(p21$estF)
Eest<- as.data.frame(est)
dim(Eest)
rownames(Eest)
head(Eest)
Eest$iso3<- data.predict$iso3
Eest$year<- data.predict$year
Eest$I_Amr<- data.predict$I_Amr

############################################################
# post hoc adjustment, to have zero cholera in the Americas
ets
head(Eest[,ets])
Eest$n_caseschol<- ifelse(Eest$I_Amr==1,0, Eest$n_caseschol)
tempS<- rowSums(Eest[,ets])
summary(tempS)
for(et in ets){
  #rescale pro-rata
  Eest[,et]<- Eest[,et] / tempS
}
# end post-hoc
############################################################


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
Eest<- Eest[which(Eest$High.income==0),]
table(Eest$year)
names(Eest)
dim(data.predict)
save(Eest, LMIC.draws, file=paste0("../results/Predict_etiologies.RData"))

head(Eest)
table(Eest$year)
