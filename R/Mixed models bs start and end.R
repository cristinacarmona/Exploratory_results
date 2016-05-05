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