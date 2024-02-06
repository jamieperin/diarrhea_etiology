
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



## Load data
load(file="../results/Make_Input_Data_HI_20231228.RData")
table(table(studies[,vxr]))
summary(as.vector(table(studies[,vxr])))
dim(studies)
length(unique(studies[,vxr]))

# covariates
vxf<- c( "year",
        "Iage_lower_lt24m","Iage_upper_24to59m")
vdt #<- paste0("cases", etiologies)
vdt


source("functions_etiologies.R")

table(table(studies$ran_id))
lam<- 150
sd<- 0.04

  Start<- Sys.time()
  print(Start)
  cat("\n\n LAMBDA ",lam," , sd = ", sd, "  \n\n")


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
             ITER=15000,      #Number of MCMC iterations
             BURN=10000,        #Number of iterations for burn-in of MCMC
             THIN=1,        #Thining factor for MCMC
             SEED=718,      #random seed for jags.parallel (for jags use set.seed() first)
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
  head(summ[order(-summ$Rhat),], n=30)
  table(summ$Rhat>1.1)
#  head(summ, n=50)
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
       file=paste0("../results/Estimate_etiologies_HI_l", lam,".RData"))


vdt



Sys.time() - Start.time
