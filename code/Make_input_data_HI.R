
rm(list=ls())
load("../results/Import.RData")
etiologies
sm.et<- c("epectyp","etecst","chol","crypto","sapo")
#removing etiologies with little data, < 10 studies
etiologies<- etiologies[!etiologies %in% sm.et]

#install.packages("readstata13", lib="/users/jperin/Rpackages", repos="https://cloud.r-project.org")

library(lattice)
library(coda)
#library(R2jags)
library(nnet)
library(plyr)
library(foreign)
library(data.table)
library(readstata13)
library(openxlsx)

# To make for Jags model, including bootstrap finding lambda
#
# data.predict - national covariates for prediction
# deaths - dataset of reported causes of death by study with missclassification matrix
# studies - dataset of studies
#
# vdr - causes
# vdt - causes
# vid - "country" "studyid" "year" "period" (for neonates)
# vxf - covariates included in lasso
# vxr - "isocode"
#
#
#
#

#########################################################################################################
#inp.int <- read.dta("data/combined master study database_04Sep2015_final_intermediate_v12.dta")
#inp.int <- read.dta13("data/combined master study database_29 Oct 2019_final.dta")
inp.int<- dat
names(inp.int)[grep("rep", names(inp.int))]

#nc<- read.dta13("../data/U5_mcee_covariates_2020320_dy.dta")
nc<- read.dta13("../data/vavr_national_covariates_2023 02 14.dta")
names(nc)
nc$u5pop
summary(nc$year)
summary(nc$rota)
#summary(nc[,"_5q0"])
#nc$u5mr<- nc[,"_5q0"]


# borrow rota from national covariate
inp.int$rota<- NULL
inp.int<- merge(inp.int, nc[,c("iso3","year", "rota","u5mr")], by=c("iso3", "year") , all.x=T)
inp.int[which(is.na(inp.int$rota)),c("refid","iso3","year")]



inp.int$rota<- ifelse(inp.int$iso3=="YUG",0, inp.int$rota)
inp.int$u5mr<- ifelse(inp.int$iso3=="YUG",3, inp.int$u5mr)
#inp.int[which(is.na(inp.int$rota)),c("sample_size")]
inp.int<- inp.int[which(!is.na(inp.int$rota)),]

table(inp.int$iso3=="ITA")

# assuming median # of cases for studies with missing number of cases
names(inp.int)
summary(inp.int$cases_tested)
#causes<- paste0("cases", etiologies)
dim(inp.int)
#inp.int$cases_tested_corr<- ifelse(is.na(inp.int$cases_tested), inp.int$sample_size, inp.int$cases_tested)
inp.int$cases_tested_corr<- inp.int$cases_tested
summary(inp.int$cases_tested_corr)
# cap at 1000
inp.int$cases_tested_corr<- ifelse(inp.int$cases_tested_corr >1000, 1000, inp.int$cases_tested_corr)
inp.int[which(is.na(inp.int$cases_tested_corr)),c("iso3","refid","cases_tested","strata_id")]

# THIS SHOULD NOT BE MISSING
#inp.int[which(inp.int$refid=="R2022_22760"),c("iso3","refid","cases_tested","sample_size","strata_id")]
#inp.int<- inp.int[which(!is.na(inp.int$cases_tested_corr)),]




# mark those that are from high income countries
br<- read.xlsx("../data/UNICEF Region.xlsx")
br$iso3<- br$ISO.3.Code
table(br$UNICEF.region)
br$High.income<- ifelse(grepl("High", br$UNICEF.region),1,0)
table(br$High.income)
inp.int<- merge(inp.int, br[, c("iso3","High.income")], by="iso3", all.x = T)
table(inp.int$High.income, useNA="always")
inp.int[which(is.na(inp.int$High.income)), c("iso3","year","cases_tested",
                                            "refid","strata_id")]
inp.int$High.income<- ifelse(inp.int$iso3=="YUG",0, inp.int$High.income)
inp.int$High.income<- ifelse(inp.int$iso3=="TWN",1, inp.int$High.income)
#table(br$iso3,br$High.income)
table(inp.int$year<2000, inp.int$High.income)
inp.int<- inp.int[which(inp.int$year>1999),]
#assuming these are all high income
#inp.int$High.income<- ifelse(is.na(inp.int$High.income),1,inp.int$High.income)
table(inp.int$iso3, inp.int$High.income)
tapply(inp.int$cases_tested_corr, inp.int$High.income, FUN=sum, na.rm=T)
inp.int[order(-inp.int$cases_tested_corr)[1:20],c("iso3","cases_tested","High.income")]
#inp.int$controls_tested_corr<- ifelse(
#  is.na(inp.int$controls_tested), inp.int$sample_size, inp.int$controls_tested)
#summary(inp.int$controls_tested_corr)
#inp.int[which(is.na(inp.int$controls_tested_corr)),
#        c("cases_tested","controls_tested_corr","controls_tested","sample_size","controlsrota"
#)]



# Removing high income countries
table(inp.int$High.income)
inp.int<- inp.int[which(inp.int$High.income==1),]


# Calculate number of cases for each etiology
e<- etiologies[1]
for(e in etiologies) {
  cat(e,"\n")
  inp.int[, paste0("n_cases",e)]<- inp.int[,paste0("cases",e)] /100 * inp.int$cases_tested_corr
#  inp.int[, paste0("n_controls",e)]<- inp.int[,paste0("controls",e)] /100 * inp.int$controls_tested_corr
}

causes<- paste0("n_cases", etiologies)
summary(inp.int[, causes])




#################################################################################
#Covariates for multicause modeling
summary(inp.int$rota)
summary(nc$rota)
table(inp.int$age_lower, inp.int$age_upper, useNA="always")
table(inp.int$age_lower, useNA="always")
table(inp.int$age_upper, useNA="always")
inp.int[which(is.na(inp.int$age_lower)),c("refid","age_upper","age_lower", "cases_tested")]
inp.int<- inp.int[which(!is.na(inp.int$age_lower)),]
inp.int$Iage_lower_lt24m<- ifelse(inp.int$age_lower<24,1,0)
inp.int$Iage_upper_24to59m<- ifelse(inp.int$age_upper>=24 & inp.int$age_upper<60,1,0)


#names(inp)
inp.int$tot.org   <- inp.int$cases_tested_corr
inp.int$sum_etiologies<-rowSums(inp.int[,causes], na.rm=TRUE)

xt<- which(inp.int$sum_etiologies > inp.int$cases_tested_corr)
inp.int[xt, c("refid", paste0("cases", etiologies),"cases_tested", "cases_tested_corr","sum_etiologies")]
#inp.int<- inp.int[-xt,]

################################################################
# Number of deaths/ cases, to resolve rounding and potential,
# specify "other" causes
# reweighting
table(inp.int$iso3)

inp.int$n_casesother<- inp.int$cases_tested_corr - inp.int$sum_etiologies
causes<- c(causes, "n_casesother")

#can introduce function of total number, e.g. sqrt
inp.int$tot   <-     inp.int$cases_tested_corr
#inp.int$other.org<- inp.int$other
cmiss<- rep(0, dim(inp.int)[[1]])
temp.tot<- rep(0, dim(inp.int)[[1]])
c<- causes[1]
for(c in causes) {
  cat(c,"\n")
  inp.int[,c] <- round(inp.int[,c]/ inp.int$tot.org * inp.int$tot)
  temp.tot<- temp.tot + ifelse(is.na(inp.int[,c]),0, inp.int[,c])
  cmiss<- cmiss+ ifelse(is.na(inp.int[,c]),1,0)
}

length(etiologies)
inp.int$cmiss<- cmiss

inp.int[which(inp.int$cmiss ==11), c("iso3","refid",
                                     paste0("cases", etiologies))]
#write.dta(inp.int[which(inp.int$cmiss ==11), c("iso3","refid",
#                                               paste0("cases", etiologies),"cases_tested","sample_size")], file="Single_etiology_studies_20221119.dta")

#write.csv(table(cmiss), file="Make_input_data_misssing.csv")

#hist(inp.int$rota, main="Rotavirus vaccine coverage", xlab="")
#hist(inp.int$cases_tested, main="Number of cases tested", xlab="")
diff<- inp.int$tot - inp.int$tot.org
summary(diff)
inp.int$tot <- temp.tot


#####################################################################################
#If any causes are missing, then "other" is also missing
#table(cmiss)
inp.int$n_casesother<- ifelse(cmiss==0, inp.int$n_casesother, NA)




#####################################################################################





#age_lower	1 = <24m; 2 = >=24m
#age_upper	1 = <24m; 2 = 24-<60m; 3 = >60m


nc$Iage_lower_lt24m<-1
nc$Iage_upper_24to59m<-1

#inp.int$Iageupper_2 <- ifelse(inp.int$age_ub_m==2,1,0)
#inp.int$Iagelower_1 <-ifelse(inp.int$age_lb_m==1,1,0)

#inp.int$int<-1
#inp.int$int.std<-1
covs<-c('rota','u5mr',"year","Iage_lower_lt24m", "Iage_upper_24to59m")



for(i in covs){
    cat(i)
cat("\n\n\nsummary(inp.int[,covs])\n")
    print(summary(inp.int[,i]))
cat("\n\n\nsummary(nc[,covs])\n")
    print(summary(nc[,i]))
}





#################################################################################

summary(inp.int[covs])
summary(inp.int[,causes])
summary(inp.int[,"tot"])
inp<- inp.int






#inp$refid
######### ! END  Pn vamcm data


S <- nrow(inp)  # Number of Studies
K <- length(covs)    # number of explanatory variables (INCLUDING  the constant=1)
C <- length(causes)    # Number of causes of death

length(unique(inp$refid))
# study/country variables
# The first column is a constant of 1s
X <- as.matrix(inp[,covs])
inp$n<- inp$tot
inp$sid<- 1:nrow(inp) #as.numeric(as.factor(inp$refid))
inp$isocode<- inp$iso3
#inp$rG<- inp$country
inp$natrep<- 0
#table(inp$natrep)
#table(inp$refid)
#dw<- inp[,c("sid",causes,"n", covs,"isocode","natrep")]
inp$ran_id<- ifelse(inp$natrep==1, as.character(inp$isocode), as.character(inp$refid))
#table(inp$ran_id, inp$natrep, useNA="always")


dw<- inp[,c("sid",causes,"n", covs,"ran_id","refid","cases_tested","bibliography",
            "cases_tested_corr","iso3", causes,"age_lower","age_upper")]
head(dw)


#### Create OBSERVED deaths with a matrix per study

### Creating Matrix of L*K for each study
### where K is the number of TRUE categories and
### L= number of OBSERVED categories in that study


#head(dw)
### Create a matrix for each study and rbind them. Columns are
### true causes, rows are observed causes

#dw$sid
summary(dw$n)
C
S
K
names(dw)
dw[which(dw$sid==15),c(causes,"cases_tested_corr")]
dw[which(dw$sid==472),c(causes,"cases_tested_corr", "refid")] #paste0("cases", etiologies))]

causes
sid<- 472
for(sid in 1:S){
    cat("\n\n",sid)
  nobs<- sum(!is.na(dw[sid,causes]))  #number of observed causes
  N<- dw$n[sid]  #total number of deaths
  print(N)
#  print(dw[sid,c("sid",causes,"n")])
cat(" nobs: ", nobs,"\n")
  if(nobs>0) { # if at least one etiology tested
    D0<- as.data.frame(matrix( rep(0, nobs*C), nrow=nobs, ncol=C)  )
    print(D0)
  D0$n<- NA
  L<-1
    for(c in 1:C) {
        cat("\n")
    if(!is.na(dw[sid,c+1])) {
      D0[L,c] <- 1
      D0[L,"n"]<- dw[sid, causes[c]]  #number of deaths for observed cause
      L<- L+1
      print(D0)
    }
  }
  if(sum(D0$n) != N  | sum(colSums(D0[,1:C])) < C ) {
    Dother<-  as.data.frame( t( 1 - colSums(D0[,1:C]) ) ) #unobserved causes put into other
    Dother$n<- N - sum(D0$n)
    D0<- rbind(D0, Dother)
  }
  }

  if(nobs==0) { # if no etiologies tested
    D0<- as.data.frame(matrix( rep(1, C), nrow=1, ncol=C)  )
    D0$n<- N
  }

  D0$sid<-sid
  D0$g<- 1:nrow(D0)
  print(D0)

  ifelse(sid==1, dc <- D0, dc <- rbind(dc, D0))
}
dc$last<- 1:nrow(dc)
names(dc)[1:C] <- causes #c(paste("c",c(1:C),sep="")) #,"n","sid","g","last")
dw$sid<- 1:nrow(dw)


natrep<- unique(dw$isocode[which(dw$natrep==1)])
dw$N<- dw$n #totdeaths
head(dw)
head(dc, n=20)
## Merge with country variable data for analysis
dws <- merge(dw[,c("sid",covs,"ran_id","N","iso3","cases_tested_corr","cases_tested","refid","bibliography",
                   causes,"age_lower","age_upper")] , aggregate(dc[,c("g","last")], list(sid=dc$sid), max),
             by="sid")
dws$first <- dws$last - dws$g + 1
dim(dws)
head(dws)
####  for "REPORTED" classification of deaths (LONG format)

### check
sum(table(dc$n))
sum(dc$n)
sum(inp$tot)
summary(aggregate(dc$n, list(dc$sid), sum)[,2] - inp$tot)
sum(inp$tot)
head(inp[,c("sid",causes,"tot")])



## Data
#jd.1 <- list(GM=as.matrix(dc), XM=as.matrix(dws[,-(1:2)]), S=S, K=K, C=C)

#jm.1 <- jags(data=jd.1, parameters.to.save=jp.1, model.file="model1.txt",
#             n.iter=4000, n.chains=4, n.burnin=2000, n.thin=1)


#vamcm.natcov$envelope<- vamcm.natcov$envelope_aids_measles_free
#vamcm.natcov$isocode<- vamcm.natcov$iso3


cases <- dc
studies <- dws
data.predict <- nc #vamcm.natcov
data.predict$ran_id<- data.predict$iso3

vdr<- vdt<- causes
sm.et<- c("n_caseschol","n_casesepectyp","n_casesetecst",
          "n_casescrypto","n_casessapo")
vdt<- vdt[which(!vdt %in% sm.et)]
#vid<- c("country","study_id","year")
vxf<- covs
vxr<- "ran_id"

print("names(data.predict)")
print(names(data.predict))

print("names(cases)")
names(cases)
print(head(cases))

print("names(studies)")
names(studies)
print(head(studies))


cat("\n\n causes: vxf \n")
print(vdt)


cat("\n\n covs: vxf \n")
print(vxf)
cat("\n\n summary(vxf)\n")
print(summary(studies[,vxf]))

cat("\n\nstudies[,vxr]\n")
print(studies[,vxr])

cat("\n\n\n\n dim(data.predict)\n")
print(dim(data.predict[,c(vxr,vxf)]))

cat("\n\n summary(data.predict)\n")
print(summary(data.predict[,c(vxr,vxf)]))

# this exclude Taiwan and Puerto Rico
temp<- merge(data.predict, br[, c("iso3","High.income")], by="iso3")
dim(temp)
dim(data.predict)
data.predict<- temp


cases[order(cases$n),]

save(data.predict, cases, studies, natrep,
     vdr, vdt, #vid,
     vxf, vxr, file="../results/Make_input_data_HI_20231228.RData")
names(studies)
dim(inp.int)
summary(cases)
head(cases)

dat<- inp.int
out<- NULL
for(et in etiologies){
  cat(et,"\n")
  print(summary(dat[, paste0("cases", et)]/dat$cases_tested))
  lout<- data.frame(et, mean.cases = mean(dat[, paste0("cases", et)], na.rm=T),
                    count.study.cases = sum(!is.na(dat[, paste0("cases", et)])) #,
                    #cases.tested = mean(dat[, paste0("cases", et)], na.rm=T),
                  #  mean.conts = mean(dat[, paste0("controls", et)], na.rm=T),
                  #  count.study.conts = sum(!is.na(dat[, paste0("controls", et)]))
  )
  out<- rbind(out, lout)
}
out
#write.csv(out, file="Import_summary_HI.csv")


