
#############################
# admin for Amy
#setwd("H:/MCEE/analysis")
rm(list=ls())
Start.time<- Sys.time()
options(width=100)

#############################
#### Analysis of samples
library(sirt)
library(R2jags)
library(doParallel)
library(coda)
library(lattice)

# Africa, South Asia, East Asia, Latin America

## Load data
load(file="../results/Make_Input_Data_20231228.RData")
#load(file="Make_Input_Data_20230831.RData")

length(unique(studies$refid))
sum(studies$cases_tested_corr)
sum(studies$cases_tested)

vxf #covariates
table(studies$whoreg6)
summary(studies$year)
vxf<- c("rota", 
        "gni",
        "I_Africa", "I_Amr","I_Sear","I_Wpr","I_Emr",
        "Iage_lower_lt24m","Iage_upper_24to59m")
vxr #<- "refid" #Random effect identifier
vdt #<- paste0("cases", etiologies)
summary(studies[, vxf])
table(studies$I_Africa, useNA="always")

source("functions_etiologies.R")

s<-27
cases[which(cases$sid==s), c(vdt,"n")]
studies[which(studies$sid==s), c(vdt,"cases_tested_corr")]

  lam<- 75
  sd<- 0.07
  
  Start<- Sys.time()
  print(Start)
  cat("\n\n LAMBDA ",lam,"  \n\n")
  vxr<- "iso3"
  
  out<- f.e1(PARA=1,        #1 - use parallel computing, 0 - use serial computing
             STUD=studies,  #Input study data
             DEAT=cases,   #Deaths from input studies (in misclassification matrix)
             VDT=vdt,       #Causes
             VXF=vxf,       #Covariates
             VXR=vxr,       #Random Effect identifier
             MODL="LASSO_etiologies.txt", #Text file with JAGS model defintion
             LAMB=lam,      #Lambda value for Lasso
             RSDL=sd,       #For prior of standard deviation of random effects, maximum of SD
             QUAD=0,        #NULL if no quadratic terms desired
             CHAS=4,        #Number of MCMC chains
             ITER=10000,      #Number of MCMC iterations
             BURN=5000,        #Number of iterations for burn-in of MCMC
             THIN=1,        #Thining factor for MCMC
             SEED=123,      #random seed for jags.parallel (for jags use set.seed() first)
             PRIN=TRUE,     #Print computation stage
             SAMC=TRUE,     #T/F save MCMC samples?
             SAVX=TRUE,     #Save explanatory variables
             SADE=TRUE,     #Save death matrix
             NAME=NULL      #Optional model names
             )


  print(Sys.time() - Start)

  #convergence statistics
  class(out$output$BUGSoutput)
  #summ <-  sirt::mcmc.list.descriptives( as.mcmc(out$output$BUGSoutput) )
  #warnings()
  summ<-as.data.frame(out$output$BUGSoutput$summary)
  names(summ)
  print(summary(summ$Rhat))

  #hist(summ$Rhat, main=paste("Lambda resampled, sd max = ",sd))
  head(summ[order(-summ$Rhat),])
  head(summ[order(-summ$mean),])
  table(summ$Rhat>1.1)
  head(summ, n=50)
  vxf
  eb0   <- out$output$BUGSoutput$median$B # fixed effects
  cat("\n\n BETA estimate\n\n")
  print(eb0)

  save(out,
       #summ, jm.r0,
       #lambda, sd,
       #studies, data.predict, deaths,
       #vxf, #predictors
       #vdt, #causes
       file=paste0("../results/Estimate_etiologies_l",lam,".RData"))
  head(summ[order(-summ$mean),])
  head(summ[order(-summ$Rhat),])
  table(summ$Rhat>1.1)
  print(Sys.time() - Start)
vdt
vxf


Sys.time() - Start.time
