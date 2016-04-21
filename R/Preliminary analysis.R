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

working.list <- import.list[4]

names(working.list) <- c("indiv")
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


indiv$pop.sp.sex<-ifelse(indiv$population %in% "Tuzla", 
                               paste(indiv$population, indiv$species, indiv$field_sex_focal, sep="-"),
                               paste(indiv$population, indiv$species, indiv$mol_sex_focal, sep="-"))
indiv$sex<-ifelse(indiv$population %in% "Tuzla", indiv$field_sex_focal, indiv$mol_sex_focal)
indiv$pop.sp.field_sex<-paste(indiv$population, indiv$species, indiv$field_sex_focal, sep="-")

#_-------------------------------------------------------------------------------------
# Breeding Schedule length
#1. Data structure:--------
  #a) Histograms:---------------
hist(indiv$bs.length)
table(indiv$bs.length)

qqnorm(indiv$bs.length)
qqline(indiv$bs.length,lty=2)

  #b) Normality----------
shapiro.test(indiv$bs.length)
ks.test(indiv$bs.length, pnorm)


#2. Explore relations:------------
attach(indiv)
names(indiv)
males.mol<-indiv[grep(indiv$pop.sp.sex, pattern="M$"),]
females.mol<-indiv[grep(indiv$pop.sp.sex, pattern="F$"),]
both<-rbind(males.mol, females.mol)

pairs(~ bs.start.std + bs.end.std + bs.length+total.nests.peryear, data=females.mol, panel=panel.smooth)
pairs(~ bs.start.std + bs.end.std + bs.length+total.nests.peryear, data=males.mol, panel=panel.smooth)

lm.1<-lm(bs.length~pop.sp + bs.start.std + total.nests.peryear + sex + pop.sp:sex, data=both)
summary(lm.1)

lm.m<-lm(bs.length~pop.sp + bs.start.std + total.nests.peryear, data=males.mol)
summary(lm.m)

