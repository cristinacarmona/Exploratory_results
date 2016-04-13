#Merge breeding schedules data extracted for each population into one file
#03/03/2016, author: Cristina Carmona-Isunza

#Read files
  setwd("F:/Plovers/3rd Chapter/Exploratory_results/input")
  csvfiles <- list.files(path = ".", pattern='*\\.csv$', all.files=TRUE)
  csvfiles
  
  df.list <- lapply(csvfiles, read.csv, header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))
  ls()
  

  working.list <- df.list
  names(working.list) <- c("ceuta","ceuta.maio","mad","maio","tuzla")
  
  attach(working.list)
  
#---------------Add column with population name----------
ceuta$population <- "Ceuta"
head(ceuta)
maio$population <- "Maio"
head(maio)
mad$population <- "Madagascar"
tuzla$population <- "Tuzla"

#-----------------------------------------Merge
#merge datasets
library(gtools)

all.pops<-smartbind(ceuta, maio, mad, tuzla)

write.csv(all.pops, "F:/Plovers/3rd Chapter/Exploratory_results/output/allpops_bschedule_13April2016.csv")



