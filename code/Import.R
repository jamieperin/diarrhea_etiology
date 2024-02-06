
rm(list=ls())
library(readstata13)
dat<- read.dta13("../data/DEfull_4Jan24withscalars.dta")

table(dat$iso3)
length(unique(dat$refid))
table(dat$refid)
table(dat$iso3)
summary(dat$collection_start)
summary(dat$collection_end)
table(dat$year)
summary(dat$age_lower)
table(dat$age_upper)
dim(dat)
names(dat)
table(dat$urban)
summary(dat$sample_size)
summary(dat$cases_tested)
summary(dat$vaccine_cov)

# qualitative
table(dat$vaccine_cov)
length(unique(dat$iso3))
summary(dat$year)
summary(dat$cases_tested)
summary(dat$casesadeno40)
summary(dat$casescampy)
summary(dat$cases_tested)


head(dat[, c("cases_tested", "caseschol")])

head(dat[which(is.na(dat$cases_tested)), c("cases_tested","caseschol", "casesshig")])

summary(dat[, c("caseschol")])
summary(dat$cases_tested)
table( is.na(dat$cases_tested))

etiologies<- 	c("epectyp",	#	enteropathogenic Escherichia coli (EPEC) typical
"etecst", #		enterotoxigenic Escherichia coli (ETEC) ST (+/- LT)
"salm", #		Salmonella spp. (excluding Salmonella typhi)
"shig",	#	Shigella spp., 
"campy",	#		Campylobacter spp.
"chol",	#		Vibrio cholerae O1 and O139
"crypto",	#		Cryptosporidium spp.
"rota",	  #	  Rotavirus
"norog2",	# 		human Caliciviruses: norovirus G2
"sapo",	#		human Caliciviruses: sapovirus
"astro",	#		astrovirus
"adeno40")	#

dat[which(is.na(dat$cases_tested)), paste0("cases", etiologies)]


summary(dat[, paste0("cases", etiologies)])

summary(dat[,paste0("cases", etiologies)])

library(dplyr)
etiologies
# use rota as reference
etiologies<- etiologies[c(8,1:7, 9:12)]

#dat$denom<- ifelse(is.na(inp.int$cases_tested), 198, inp.int$cases_tested)
names(dat)
dat[which(is.na(dat$cases_tested)),c("refid", "age_lower","age_upper",
                                     "cases_tested","iso3")]


summary(dat$pathogen_num)

hist(dat$year, breaks=30, xlab="",main="Study year")

hist(dat$cases_tested, main="Number of cases tested", breaks=30, xlab="cases_tested") #, useNA="always")
hist(dat$cases_tested[which(dat$cases_tested<1000)], main="Number of cases tested", breaks=30) #, useNA="always")

summary(rowSums(dat[, paste0("cases", etiologies)], na.rm=T))
dat$casestotal<- rowSums(dat[, paste0("cases", etiologies)], na.rm=T)
table(dat$casestotal>=100)

#Scale to be 100% or less
for(et in etiologies) {
  dat[,paste0("cases",et)] <- ifelse(dat$casestotal >100, 
                                     dat[,paste0("cases",et)]/dat$casestotal * 100, dat[,paste0("cases",et)])
}

summary(rowSums(dat[, paste0("cases", etiologies)], na.rm=T))

dim(dat)
table(dat$pathogen_num)
head(dat$pathogen_num)

summary(dat$casesadeno40)
summary(dat$casesadeno40/ dat$cases_tested)
summary(dat$cases_tested)
head(dat[,c("refid", "cases_tested", "casesadeno40")])


temp<- dat %>% filter(is.na(cases_tested) )
temp<- temp[,c("refid", "cases_tested", paste0("cases",etiologies))] 
#temp %>% View()
out<- NULL
dat<- dat[which(!is.na(dat$age_lower)),]




for(et in etiologies){
 cat(et,"\n") 
print(summary(dat[, paste0("cases", et)]/dat$cases_tested))
lout<- data.frame(et, mean.cases = mean(dat[, paste0("cases", et)], na.rm=T),
                  count.study.cases = sum(!is.na(dat[, paste0("cases", et)])) #, 
                  #cases.tested = mean(dat[, paste0("cases", et)], na.rm=T),
#                  mean.conts = mean(dat[, paste0("controls", et)], na.rm=T),
#                  count.study.conts = sum(!is.na(dat[, paste0("controls", et)]))
                  )
out<- rbind(out, lout)
}
out

dat$count_pathogens<-0
for(et in etiologies){
  dat$count_pathogens<- ifelse(!is.na(dat[,paste0("cases",et)]), 
                               dat$count_pathogens+1, dat$count_pathogens) 
}
table(dat$count_pathogens)

save(dat, etiologies, file="../results/Import.RData")

