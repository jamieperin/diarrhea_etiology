
rm(list=ls())
library(ggplot2)
library(dplyr)
library(arrayhelpers)
library(RColorBrewer)
library(viridis)
library(foreign)

# for vdt (etiologies)
#load(file="Make_Input_Data_20230410.RData")
load(file="../results/Make_Input_Data_20231228.RData")

# HI countries
load("../results/Predict_etiologies_HI.RData")
eh<- Eest
table(eh$year)

lam<-50
# LMIC 
#load("Predict_etiologies_lam2_sd04_20230410.RData")
#for(lam in c(50, 75, 100, 125, 150)) {
for(lam in c(75)) {
load(paste0("../results/Predict_etiologies.RData"))
el<- Eest
table(el$year)

sm.et<-names(el)[!names(el) %in% names(eh)]
for(e in sm.et) eh[,e]<-NA
Eest<- rbind(eh, el)
dim(Eest)
table(Eest$year)
summary(Eest$n_casesrota)
names(Eest)
table(Eest$year)
head(Eest)

causes<- vdt #names(Eest)[1:13]
summary(rowSums(Eest[,causes]))

#convert to number of deaths
for(e in causes) {
  Eest[,e]<- Eest[,e] * Eest$ufive3
}
e2<- aggregate(Eest[,c(causes, "ufive3")], list(Eest$year), FUN=sum, na.rm=T)
head(e2)
names(e2)[1]<- "year"

# go back to percent
e2
for(e in causes) {
  e2[,e]<- e2[,e]/e2$ufive3
}
rowSums(e2[,causes])  

ad<-reshape::melt(e2[,c("year",causes)], id="year")
ad<- ad %>% filter(year %in% c(2000, 2021))
table(ad$year)
head(ad)
ad$variable<- gsub("n_cases","",as.character(ad$variable))
table(ad$variable)
ad<- ad %>% mutate(cause = case_when(variable=="rota" ~ "Rotavirus",
                                     variable=="adeno40" ~ "Adenovirus",
                                     variable=="astro" ~ "Astrovirus",
                                     variable=="campy"~"Campylobacter",
                                     variable=="chol"~"Cholera",
                                     variable=="crypto"~"Cryptosporidium",
                                     variable=="epectyp"~"EPEC",
                                     variable=="etecst"~"ETEC",
                                     variable=="norog2"~"Norovirus",
                                     variable=="other"~"Other",
                                     variable=="salm"~"Salmonella",
                                     variable=="sapo"~"Sapovirus",
                                     variable =="shig" ~"Shigella",
                                     TRUE~variable))

library(RColorBrewer)
library(viridis)
#display.brewer.all()
#coln<-brewer.pal(8,"YlOrRd")
cols<-c(brewer.pal(5,"Set1"), brewer.pal(9,"Set3"))


dim(ad)


#display.brewer.all()
#coln<-brewer.pal(8,"YlOrRd")
cols<-c(brewer.pal(5,"Set1"), brewer.pal(9,"Set3"))
#cols<- viridis(12)
#cols<- cols[c(13,11:1)]
unique(ad$cause)
ad$cause.f<- factor(ad$cause, levels=c("Rotavirus" ,      "EPEC" ,           "ETEC",            
                                       "Salmonella",      "Shigella",        "Campylobacter",  
                                       "Cholera",         "Cryptosporidium", "Norovirus" ,      
                                       "Sapovirus",       "Astrovirus",      "Adenovirus","Other"))
#ad$st<- factor(ad$Group.1, levels=c("Very low","Low","Moderate","Moderate/High","High"))
head(ad)
unique(ad$strata)
#ad$st.f<- as.factor(ad$strata) #, levels=c("3","50% coverage","98% coverage"))
table(ad$cause.f, useNA="always")
frac  <- ggplot(data=ad) +
  geom_bar(aes(x=year, y=value, fill = cause.f),stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(2000, 2021)) +
  ggtitle("")+ xlab("")+ #xlab("") + 
  theme_bw() + scale_fill_manual(values = c(cols))+
  labs(fill="") +ylab("Diarrhea fraction (%)")+ggtitle(paste0("Lambda ",lam))
#print(frac)
########################################################




# rename
# clarify names
names(Eest)<- gsub("n_cases","ndeaths_", names(Eest))
etiologies<- names(Eest)[grep("ndeaths_", names(Eest))]

for(e in etiologies) {
  #cat(e,"\n")
  Eest[,gsub("ndeaths_","prob_", e)]<- Eest[,e] / Eest$ufive3
}
}
write.dta(Eest, file="../results/Etiologies_2000_2021.dta")
