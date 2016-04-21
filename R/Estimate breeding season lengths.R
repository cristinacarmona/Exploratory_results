#Calculate start and end of Breeding season for each pop each year

#based on script sent by Pippa 21/04/2016

#------------import files--------------------
setwd("F:/Plovers/3rd Chapter/Exploratory_results/input")

csvfiles <- list.files(path = ".", pattern='*\\.csv$', all.files=TRUE)
csvfiles

import.list <- lapply(csvfiles, read.csv, header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))

working.list <- import.list[3]

names(working.list) <- c("both")
attach(working.list)
#detach(working.list)
#-------------------------------------------------------------
identified<-both
str(identified)

identified$layingdate <- as.Date(identified$layingdate, "%d/%m/%Y")
identified$hatching_date.r <- as.Date(identified$hatching_date.r, "%d/%m/%Y")

identified$pop.sp.year<-paste(identified$population, identified$species, identified$year, sep="-")

#####-----3. Add additional data columns required for project-----#####

#####-----a) Length of breading season-----#####

#For each year find the earliest layingdate#
minimumlayingdate <- aggregate(layingdate ~ pop.sp.year, identified, function(x) min(x, na.rm=TRUE))
minimumlayingdate
str(minimumlayingdate)

colnames(minimumlayingdate) <- c("year", "minlayingdate")
minimumlayingdate


#For each year find the earliest layingdate - 10#
minimumlayingdate$minlayingdate.10 <- minimumlayingdate$layingdate - 10
minimumlayingdate

#For each year find the latest layingdate#
maximumlayingdate <- aggregate(layingdate ~ pop.sp.year, identified, function(x) max(x, na.rm=TRUE))
str(maximumlayingdate)

maximumlayingdate
maximumlayingdate$max.hd.est<-maximumlayingdate$layingdate +25

#For each year find the latest hatchdate#

maxihatchdate <- aggregate(hatching_date.r ~ pop.sp.year, identified, function(x) max(x, na.rm=TRUE))
str(maxihatchdate) #No hatch dates for 2011 and 2012, should use maximum layingdate + 25 to estimate maximum hatchdate

#Estimate maximum hatchdate using layingdate#
maxihatchdate$maxhatchdate.est <- maximumlayingdate$max.hd.est[match(maxihatchdate$pop.sp.year, maximumlayingdate$pop.sp.year)]


maxihatchdate$max_hd <- apply(maxihatchdate[,c(2,3)], 1, max, na.rm=T)

maxihatchdate$max_hd <- as.Date(maxihatchdate$max_hd, "%Y-%m-%d")

#For each year find the latest hatchdate + 25#

maximumlayingdate$max_hatchingdate.r<-maxihatchdate$max_hd[match(maximumlayingdate$pop.sp.year, maxihatchdate$pop.sp.year)]

maximumlayingdate$max_hd <- apply(maximumlayingdate[,c(3,4)],1, max, na.rm=T)

breedingseason <- cbind(minimumlayingdate, maximumlayingdate)

breedingseason$max_hd <- as.Date(breedingseason$max_hd, "%Y-%m-%d")
breedingseason$maxhatchdate.25 <- breedingseason$max_hd +25

#Calculate length of breeding season (minlayingdate-10 till maxhatchdate +25)#
breedingseason$breedingseasonlength <- (breedingseason$maxhatchdate.25 - breedingseason$minlayingdate.10)
breedingseason

str(breedingseason)
breedingseason$breedingseasonlength <- as.numeric(breedingseason$breedingseasonlength)
str(breedingseason)


#--------------write file-----------------------------------------------
  
write.csv(breedingseason, "F:/Plovers/3rd Chapter/Exploratory_results/output/bs_breedingseasonlengths.csv")
