#Code to extract preliminary results of Breeding schedule using Maio and Ceuta
#03/03/2016 Author: Cristina Carmona-Isunza

#1st log: Create code up to 1.Change data structure, start debugging
#2nd log: 04/03/2016 debugg 1. and start 2. Calculation of duration of breeding schedule
#         issue1: need to add fate of each nest? some individuals bred up to 4 times in one year in Maio
#         issue2: Ceuta needs to have specific mol_sex (as Luke had it you cannot distinguish if an adult was mol sexed or not)
#         issue3: Ceuta has to use first.res.bs and last.res.bs to omit censuses made outside of the breeding season

#------------import files--------------------
setwd("F:/Plovers/3rd Chapter/Exploratory results/input")

csvfiles <- list.files(path = ".", pattern='*\\.csv$', all.files=TRUE)
csvfiles

import.list <- lapply(csvfiles, read.csv, header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))

working.list <- import.list

names(working.list) <- c("ceuta","both0","maio")
attach(working.list)
#--------------------------------------------------------------
both<-both0

names(both)
head(both)
str(both)

#-------------------------------------------------
#0. Prepare data, Ceuta needs re-naming variables first.res.bs and last.res.bs

both[both$population %in% "Ceuta",c("first.res.bs", "first.res")]
both[both$population %in% "Ceuta",c("last.res.bs", "last.res")]

both$first.res[both$population%in%"Ceuta"]<-both$first.res.bs[both$population%in% "Ceuta"]
both$last.res[both$population %in% "Ceuta"] <- both$last.res.bs[both$population%in%"Ceuta"]

str(both)
names(both)
cols <- c(26,27,30,31,55,56)
both[,cols]<-lapply(both[,cols], as.Date, "%d/%m/%Y") #change columns to dates

str(both[,c("first.res", "last.res","first.cap.r","last.cap.r","first.bf.adult.r", "last.bf.adult.r")])

both$first.d.seen2 <- apply(both[,c("first.res", "last.res","first.cap.r","last.cap.r","first.bf.adult.r", "last.bf.adult.r")],1,function (x) min(x, na.rm=T))

both$last.d.seen2 <- apply(both[,c("first.res", "last.res","first.cap.r","last.cap.r","first.bf.adult.r", "last.bf.adult.r")],1,function (x) max(x, na.rm=T))

both[, c("first.d.seen","first.d.seen2")]
head(both)
tail(both)
#---------------------------------------------------
#1. Change data structure------------- 
#Each row should be one individual with all it's breeding events per year

both$year.ring <- paste(both$year, both$ring, sep="-")

head(both[order(both$year.ring),])

both$last.d.seen2 <- as.Date(both$last.d.seen2, "%Y-%m-%d")
both$first.d.seen2 <- as.Date(both$first.d.seen2, "%Y-%m-%d")
both$layingdate <- as.Date(both$layingdate, "%d/%m/%Y")
both$found_date.r <- as.Date(both$found_date.r, "%d/%m/%Y")
both$end_date.r <- as.Date(both$end_date.r, "%d/%m/%Y")

both$ld.minus10<-both$layingdate-10

str(both, list.len=500)
#Easier to extract earliest and latest date per individual per year...
#Use similar for loop used to extract brood fate dates in extract_breedingschedule.R:


for(i in 1:length(both$year)){  #for loop brood fates adults
  print(i)
  options(warn=1)
  ind <- which(both$year.ring[i] == both$year.ring)
  group.nests<-both[ind,]
  
  both$total.nests.peryear[i] <- length(group.nests$year)
  both$earliest.seen[i] <- as.Date(min(group.nests$first.d.seen2, na.rm=T), "%d/%m/%Y")
  both$latest.seen[i] <- as.Date(max(group.nests$last.d.seen2, na.rm=T), "%d/%m/%Y")
  both$min.ldminus10[i] <- as.Date(min(group.nests$ld.minus10, na.rm=T), "%d/%m/%Y")
  both$earliest.founddate[i] <- as.Date(min(group.nests$found_date.r, na.rm=T), "%d/%m/%Y")
  both$latest.enddate[i] <- as.Date(max(group.nests$end_date.r, na.rm=T), "%d/%m/%Y")
  both$min.layingdate[i] <- as.Date(min(group.nests$layingdate, na.rm=T), "%d/%m/%Y")
}

#change to dates:
both2<-both
cols <- c(101:106)
both2[,cols]<-lapply(both2[,cols], as.Date, origin="1970-01-01") #change columns to dates
str(both2, list.len = 999)

#-----------debug------

tail(both[order(both$year.ring),])

tail(both[order(both$year.ring) & both$total.nests.peryear>1,], n=-528)

ind<-both$nest.id[both$total.nests.peryear>1]

head(both[both$nest.id %in% ind,])
both[both$ring %in% "802122903",]
#------------------------

#-------------explore issue1 (fate or other variables from several nests needed?)---------
table(both$total.nests.peryear)
aggregate(both$total.nests.peryear, by=list(both$population), table)

#--------------------------------------------------------
#---------------------------------------------------------

#2. Calculate duration of breeding schedule----
#Definition of breeding schedule:
#     The overall time spent in the breeding area of each focal individual in a specific breeding season. 
both<-both2
names(both)
ind <- which(duplicated(both$year.ring) | duplicated(both$year.ring, fromLast=TRUE))
indiv <- both[-ind, c("year.ring","ring","field_sex_focal","mol_sex_focal","year.cr","year.mr","mate_ring", "field_sex_mate","mol_sex_mate","earliest.seen","latest.seen",
                      "min.ldminus10","population","fate",
                      "total.nests.peryear","min.layingdate","earliest.founddate","latest.enddate")#,"nest.id","nest")
              ]

str(both) #2114
str(indiv) #1489

#   2a. Start and end
for(i in 1:length(indiv$year.ring)){  #for loop brood fates adults
  print(i)
  options(warn=1)
  
  bs.start.df <- indiv[i,c("earliest.seen","min.ldminus10","earliest.founddate")]
  indiv$bs.start[i] <- apply(bs.start.df, 1, function(x) min(x, na.rm=T))
  indiv$bs.start.which[i] <- colnames(bs.start.df)[apply(bs.start.df,1, function(x) which(x==min(x, na.rm=T)))]
  
  bs.end.df <- indiv[i, c("latest.seen","latest.enddate","min.ldminus10","earliest.founddate")]
  indiv$bs.end[i] <- apply(bs.end.df, 1, function(x) max(x, na.rm=T))
  indiv$bs.end.which[i] <- colnames(bs.end.df)[apply(bs.end.df, 1, function(x) which(x==max(x, na.rm=T)))]
}

indiv$bs.start<-as.Date(indiv$bs.start, origin="1970-01-01")
indiv$bs.end<-as.Date(indiv$bs.end, origin="1970-01-01")

#-------------develop 2a.-----------
head(indiv)
# as.Date(min(indiv$earliest.seen[1], indiv$min.ldminus10[1], indiv$earliest.founddate[1], na.rm=T), "%d/%m/%_Y")
# 
# colnames(indiv[,c("earliest.seen","min.ldminus10","earliest.founddate")])[which.min(indiv[,c("earliest.seen","min.ldminus10","found_date.r")])]
# 
# x<-indiv[1,c("earliest.seen","min.ldminus10","earliest.founddate")]
# min(indiv[1,c("earliest.seen","min.ldminus10","earliest.founddate")])
# apply(x, 1, min)
# str(x)
# str(bs.start.df)
# 
# colnames(x)[apply(x, 1, function(x) which(x==min(x)))]

#----------debug 2a.-----
library(date)
indiv.chr<- indiv
indiv.chr$min.ldminus10 <- as.character(indiv.chr$min.ldminus10)
indiv.chr$earliest.seen <- as.character(indiv.chr$earliest.seen)
indiv.chr$latest.seen <- as.character(indiv.chr$latest.seen)
indiv.chr$nest <- as.numeric(indiv.chr$nest)
indiv.chr$found_date.r <- as.character(indiv.chr$found_date.r)

str(indiv.chr)

indiv.chr[is.na(indiv.chr$min.ldminus10),]
indiv.chr[is.na(indiv.chr$earliest.seen),] #mainly nests that disappeared
indiv.chr[is.na(indiv.chr$latest.seen),]#mainly nests that disappeared

indiv.chr[is.na(indiv.chr$found_date.r)& indiv.chr$nest>0,]#positive nests with no founddate...ignore the ones with no layingdate?
str(indiv.chr[is.na(indiv.chr$found_date.r)& indiv.chr$nest>0,])#16 observations

#2015-CA3076 does not have any date
both[both$year.ring %in% "2015-CA3076",]

#case indiv 785 has no date available
indiv[785,] #2010-CA2656
both[both$year.ring %in% "2010-CA2656",] #comments: Iwan's nest 2010-S-105/made appropriate changes in all my data (not std files)
both[both$nest.id %in% "2010-S-24",]

#----------------------------------------------------------
#   2b. Duration
str(indiv)
indiv$bs.length <- as.numeric(indiv$bs.end - indiv$bs.start)

str(indiv[indiv$bs.length ==0,]) #seen once and layingdates not known, only bred once in a year = 12 obs



#-------------------------------
#-------debug 2b.-------------
both[both$year.ring %in% "2007-CA1513",] #Sexes in Ceuta had X instead of NA...corrected

indiv[indiv$bs.length >120,]

both[both$ring %in% "CA1520",]
#----------------------------


#explore issue2: conflicting sexes--------------

u1<-indiv[indiv$mol_sex_focal == indiv$mol_sex_mate & !is.na(indiv$year.ring),]
u1<-u1[!is.na(u1$year.ring),]

u2<-both[both$mol_sex_focal == both$mol_sex_mate,]
u2<-u2[!is.na(u2$year.ring),]
#---------------------------------------------


#plot males vs females length of bs ONLY WITH mol_sex

#how many males and females with mol_sex?
table(indiv$mol_sex_focal) 
# F   M    NA
# 507 597 473
length(indiv$mol_sex_focal) #1489

indiv2<-indiv[!is.na(indiv$mol_sex_focal),]
indiv<-indiv2

indiv$pop.molsex <- paste(indiv$population, indiv$mol_sex_focal, sep="-")
unique(indiv$pop.molsex)
indiv$pop.molsex<-factor(indiv$pop.molsex)

#histogram
hist(indiv$bs.length)

indiv[indiv$bs.length>100,]

#-----------------
#-----BOXPLOT---------------
setwd("F:/Plovers/3rd Chapter/Exploratory results/plots")
tiff("test2.tiff", width=100, height=120, units="mm", res=500)
par(mfrow=c(1,1))
boxplot(bs.length~pop.molsex, data=indiv,                                                                            
        #horizontal = T,
        names = c("", "", "", ""),
        ylab="",                  
        at =c(1,2, 4,5), #specify position of each boxplot on x
        par(mar = c(7, 4, 4, 2)+ 0.1),    #enlarge plot's window default is c(5, 4, 4, 2) + 0.1.
        par(cex.lab=0.75), 
        par(cex.axis=0.7), 
        yaxt='n', 
        ylim=c(0,350),                                                                
        boxwex=0.3, outwex=0.3,
        outcex=0.5)                                                                                                                    
mtext("   Ceuta (SP)                          Maio (KP)",                                                                       
      side = 1, line = 1.5, cex = 0.75)                                                                                                
y<-seq(0,350, 20)
axis(2, at=y,las=1, cex.axis=0.7) 
                                                                                                                         
mtext("Length of breeding shcedules (days)", side=2, line=2.5, cex=0.75)
#mtext("schedules (days)", side=2, line=2, cex=0.75)
mtext("Females", side=1, line=0.5, at=1, las=1,cex=0.6)
mtext("Males", side=1, line=0.5, at=2, las=1, cex=0.6)
mtext("Females", side=1, line=0.5, at=4, las=1,cex=0.6)
mtext("Males", side=1, line=0.5, at=5, las=1, cex=0.6)
dev.off()



