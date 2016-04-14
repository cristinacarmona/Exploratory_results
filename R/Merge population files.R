#Merge breeding schedules data extracted for each population into one file
#03/03/2016, author: Cristina Carmona-Isunza

#14/04/2016 added modifications to Mad file


#Read files
  setwd("F:/Plovers/3rd Chapter/Exploratory_results/input")
  csvfiles <- list.files(path = ".", pattern='*\\.csv$', all.files=TRUE)
  csvfiles
  
  df.list <- lapply(csvfiles, read.csv, header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))
  ls()
  

  working.list <- df.list
  names(working.list) <- c("all","ceuta","ceuta.maio","mad","maio","tuzla")
  
  attach(working.list)
  
#---------------Add column with population name----------
ceuta$population <- "Ceuta"
head(ceuta)
str(ceuta)
ceuta$nest<-as.character(ceuta$nest)
ceuta$nest.id<-paste(ceuta$population, ceuta$year, ceuta$species, ceuta$site,ceuta$nest, sep="-" )

maio$population <- "Maio"
head(maio)
str(maio)
maio$nest <- as.character(maio$nest)
maio$nest.id <- paste(maio$population, maio$year, maio$species, maio$site, maio$nest, sep="-")

mad$population <- "Madagascar"
str(mad)
names(mad)
mad$nest.id <- paste(mad$population, mad$year, mad$species, mad$site, mad$nest, sep="-")

str(mad[!is.na(mad$hatching_date.r),]) #216, after ifelse 497
mad$hatching_date.r<-ifelse(is.na(mad$hatching_date.r) & !is.na(mad$hatch_date), mad$hatch_date, mad$hatching_date.r)
colnames(mad)[38]<-"hatch"
mad$hatching_date.r<-as.Date(mad$hatching_date.r, "%d/%m/%Y")

ind<-which(is.na(mad$layingdate))
mad$estimated.ld<-as.Date(mad$estimated.ld)
mad$estimated.ld[ind]<-mad$hatching_date.r[ind]-25
mad$layingdate<-as.Date(mad$layingdate)
mad$layingdate[ind]<-mad$estimated.ld[ind]


tuzla$population <- "Tuzla"
str(tuzla)
tuzla$nest<-as.character(tuzla$nest)
tuzla$nest.id <- paste(tuzla$population, tuzla$year, tuzla$species, tuzla$site, tuzla$nest, sep="-")

#-----------------------------------------Merge
#merge datasets
library(gtools)

all.pops<-smartbind(ceuta, maio, mad, tuzla)
str(all.pops)

write.csv(all.pops, "F:/Plovers/3rd Chapter/Exploratory_results/output/allpops_bschedule_14April2016.csv")



