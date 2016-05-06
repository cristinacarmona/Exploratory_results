#Analysing breeding schedules (bs) data (start and end) from Ceuta, Maio, Tuzla and Mad
#05/05/2016
#by Cristina Carmona-Isunza

#1st log - 05/05/2016 Create code, based on length of breedins schedules


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

# indiv$pop.sp.sex<-ifelse(indiv$population %in% "Tuzla", 
#                                paste(indiv$population, indiv$species, indiv$field_sex_focal, sep="-"),
#                                paste(indiv$population, indiv$species, indiv$mol_sex_focal, sep="-"))

indiv$sex<-ifelse(indiv$population %in% "Tuzla", indiv$field_sex_focal, indiv$mol_sex_focal)

ind<-which(is.na(indiv$sex))
indiv[ind,]

indiv$sex.available<-ifelse(indiv$mol_sex_focal %in% "M"| indiv$mol_sex_focal %in% "F", indiv$mol_sex_focal, indiv$field_sex_focal)

#indiv$pop.sp.field_sex<-paste(indiv$population, indiv$species, indiv$field_sex_focal, sep="-")

indiv$pop.sp.year<-paste(indiv$pop.sp, indiv$year, sep="-")

#Add breeding season length to each year
unique(indiv$pop.sp.year)

indiv$season.length <- season$breedingseasonlength[match(indiv$pop.sp.year, season$pop.sp.year)]

#If an individual stayed longer than the breeding season length, then change its length to the breeding season lenght

indiv[indiv$bs.length > indiv$season.length,]
indiv$corrected.bs.length <- ifelse(indiv$bs.length > indiv$season.length, indiv$season.length, indiv$bs.length)

indiv<-ddply(indiv, c("pop.year"), transform, corrected.bs.length.std=scale(corrected.bs.length))
ind<-which(is.na(indiv$corrected.bs.length.std))
indiv[ind,]

#Monogamous and polygamous pops
indiv$ms <- ifelse(indiv$pop.sp %in% c("Ceuta-SP", "Tuzla-KP", "Madagascar-KiP"), "Pol", "Mon")
indiv$asr[indiv$pop.sp %in% "Ceuta-SP"]<- 0.608
indiv$asr[indiv$pop.sp %in% "Tuzla-KP"]<-0.585
indiv$asr[indiv$pop.sp %in% "Madagascar-MP"]<-0.421
indiv$asr[indiv$pop.sp %in% "Maio-KP"]<-0.469
indiv$asr[indiv$pop.sp %in% "Madagascar-WfP"]<-0.429
indiv$asr[indiv$pop.sp %in% "Madagascar-KiP"]<-0.386



males.mol<-indiv[indiv$sex %in% "M",]
females.mol<-indiv[indiv$sex %in% "F",]
both.mol<-rbind(males.mol, females.mol)

males.av<-indiv[indiv$sex.available %in% "M",]
females.av<-indiv[indiv$sex.available %in% "F",]
both.av<-rbind(males.av,females.av)

both.mol$sex <- as.factor(both.mol$sex)
both.av$sex.available<-as.factor(both.av$sex.available)

#restrict to individuals ringed one year before focal year:
names(both)
length(both.mol$year) #4215
length(both.av$year) #4464

#USE available sex FIRST 04/05/2016
both1<-both.av[both.av$year > both.av$year.cr,]
length(which(!is.na(both1$year))) #1445 but there were NAs insterted....so total 1357
unique(both1$pop.year)
table(both1$pop.year)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Breeding Schedule start------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#1. Data structure:--------
both<-both1
summary(both$bs.start.std)
both$bs.start.std.plus7 <- both$bs.start.std + 4
summary(both$bs.start.std.plus7) 

summary(both$bs.end.std)
both$bs.end.std.plus5<-both$bs.end.std + 5
summary(both$bs.end.std.plus5)

#a) Histograms:---------------
hist(both$bs.start.std)
hist(both$bs.end.std)


require(MASS)
library(car)
qqnorm(both$bs.start.std)
qqline(both$bs.start.std,lty=2)

#outliers?
boxplot(both$bs.start.std)
indiv

#log norm dist?
qqp(both$bs.start.std.plus7, "lnorm") #needs newer version of R
#see: http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

#norm dist?
qqp(both$bs.start.std, "norm")


#b) Normality----------
shapiro.test(both$bs.start.std)
ks.test(both$bs.start.std, pnorm)


#2. Explore relations:------------
pairs(~ bs.start.std + bs.end.std + bs.length+total.nests.peryear + ms, data=both, panel=panel.smooth)
boxplot(bs.start.std~ms, data=both)
boxplot(bs.start.std~interaction(sex.available,ms), data=both)




boxplot(bs.end.std~interaction(sex.available, ms), data=both)


