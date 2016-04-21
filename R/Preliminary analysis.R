#Analysing breeding schedules (bs) data from Ceuta, Maio, Tuzla and Mad
#20/04/2016
#by Cristina Carmona-Isunza

#1st log -20/04/2016 Create script

#-----------------------------------------------
#------------import files--------------------
setwd("F:/Plovers/3rd Chapter/Exploratory_results/output")

csvfiles <- list.files(path = ".", pattern='*\\.csv$', all.files=TRUE)
csvfiles

import.list <- lapply(csvfiles, read.csv, header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))

working.list <- import.list[c(4,5)]#bs_data_per_indiv.csv

names(working.list) <- c("season","indiv")
attach(working.list)
#detach(working.list)
#--------------------------------------------------------------
#0. Prepare Data:-----------
library(plyr)
str(indiv, list.len=300)
cols<-c(122,124)
indiv[,cols]<-apply(indiv[,cols], 2, function(x) as.Date(x, "%Y-%m-%d"))

indiv$pop.year <- paste(indiv$population, indiv$year, sep="-")
indiv<-ddply(indiv, c("pop.year"), transform, bs.start.std= scale(bs.start))

indiv<-ddply(indiv, c("pop.year"), transform, bs.end.std=scale(bs.end))

indiv<-ddply(indiv, c("pop.year"), transform, bs.length.std=scale(bs.length))

indiv$pop.sp.sex<-ifelse(indiv$population %in% "Tuzla", 
                               paste(indiv$population, indiv$species, indiv$field_sex_focal, sep="-"),
                               paste(indiv$population, indiv$species, indiv$mol_sex_focal, sep="-"))
indiv$sex<-ifelse(indiv$population %in% "Tuzla", indiv$field_sex_focal, indiv$mol_sex_focal)
indiv$pop.sp.field_sex<-paste(indiv$population, indiv$species, indiv$field_sex_focal, sep="-")

indiv$pop.sp.year<-paste(indiv$pop.sp, indiv$year, sep="-")

#Add breeding season length to each year
unique(indiv$pop.sp.year)

indiv$season.length <- season$breedingseasonlength[match(indiv$pop.sp.year, season$pop.sp.year)]

#If an individual stayed longer than the breeding season length, then change its length to the breeding season lenght

indiv[indiv$bs.length > indiv$season.length,]
indiv$corrected.bs.length <- ifelse(indiv$bs.length > indiv$season.length, indiv$season.length, indiv$bs.length)

indiv<-ddply(indiv, c("pop.year"), transform, corrected.bs.length.std=scale(corrected.bs.length))

#Monogamous and polygamous pops
indiv$ms <- ifelse(indiv$pop.sp %in% c("Ceuta-SP", "Tuzla-KP", "Madagascar-KiP"), "Pol", "Mon")


#_-------------------------------------------------------------------------------------
# Breeding Schedule length
#1. Data structure:--------
  #a) Histograms:---------------
hist(indiv$corrected.bs.length.std)
table(indiv$bs.length.std)

qqnorm(indiv$corrected.bs.length.std)
qqline(indiv$corrected.bs.length.std,lty=2)

  #b) Normality----------
shapiro.test(indiv$corrected.bs.length.std)
ks.test(indiv$corrected.bs.length.std, pnorm)


#2. Explore relations:------------
attach(indiv)
names(indiv)
indiv$year<- as.factor(indiv$year)
indiv$pop.sp<-as.factor(indiv$pop.sp)
indiv$ms<-as.factor(indiv$ms)

males.mol<-indiv[grep(indiv$pop.sp.sex, pattern="M$"),]
females.mol<-indiv[grep(indiv$pop.sp.sex, pattern="F$"),]
both<-rbind(males.mol, females.mol)



pairs(~ bs.start.std + bs.end.std + bs.length+total.nests.peryear, data=females.mol, panel=panel.smooth)
pairs(~ bs.start.std + bs.end.std + bs.length+total.nests.peryear, data=males.mol, panel=panel.smooth)

lm.1<-lm(corrected.bs.length.std~pop.sp + bs.start.std + total.nests.peryear + sex+  ms + ms:sex+pop.sp:sex, data=both)
summary(lm.1)


lm.m<-lm(corrected.bs.length.std~ms+pop.sp + bs.start.std + total.nests.peryear+year, data=males.mol)
summary(lm.m)

library(HH)
vif(lm.1)

lm.m1<-update(lm.m, .~.-year,.)
anova(lm.m1, lm.m)
lm.m2<-update(lm.m1,.~.-pop.sp,.)
anova(lm.m1,lm.m2)
summary(lm.m1)
lm.m3<-update(lm.m1, .~.-bs.start.std,.)
anova(lm.m1, lm.m3)
lm.m4<-update(lm.m1, .~.-total.nests.peryear,.)
anova(lm.m1,lm.m4)

print(levels(males.mol$year))




