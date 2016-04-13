#Merge breeding schedules data extracted for each population into one file
#03/03/2016, author: Cristina Carmona-Isunza

#Read files
  setwd("F:/Plovers/3rd Chapter/Exploratory results/input")
  csvfiles <- list.files(path = ".", pattern='*\\.csv$', all.files=TRUE)
  csvfiles
  
  df.list <- lapply(csvfiles, read.csv, header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))
  ls()
  

  working.list <- df.list
  names(working.list) <- c("ceuta","maio")
  
  attach(working.list)
  
#---------------Add column with population name----------
ceuta$population <- "Ceuta"
head(ceuta)
maio$population <- "Maio"
head(maio)



#-----------------------------------------Merge
#merge datasets
library(gtools)

all.pops<-smartbind(ceuta, maio)

write.csv(all.pops, "F:/Plovers/3rd Chapter/Exploratory results/output/ceuta_maio_bschedule.csv")



