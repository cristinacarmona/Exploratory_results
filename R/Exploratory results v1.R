#Code to extract preliminary results of Breeding schedule using Maio and Ceuta
#03/03/2016 Author: Cristina Carmona-Isunza

#1st log: Create code up to 1.Change data structure, start debugging
#2nd log: 04/03/2016 debugg 1. and start 2. Calculation of duration of breeding schedule
#         issue1: need to add fate of each nest? some individuals bred up to 4 times in one year in Maio
#         issue2: Ceuta needs to have specific mol_sex (as Luke had it you cannot distinguish if an adult was mol sexed or not)
#         issue3: Ceuta has to use first.res.bs and last.res.bs to omit censuses made outside of the breeding season
#3rd log: 05/03/2016 Modified Ceuta's first.res and last.res, created boxplot of length of breeding schedules using
#         only individuals with mol_sex
#4th log: 08/03/2016 Create mean plots with SD, SE and CI95%
#5th log: 09/03/2016 Std dates were wrongly calculated, corrected this.
                     #Added violin plots for bs.start and bs.end

#------------import files--------------------
setwd("F:/Plovers/3rd Chapter/Exploratory_results/input")

csvfiles <- list.files(path = ".", pattern='*\\.csv$', all.files=TRUE)
csvfiles

import.list <- lapply(csvfiles, read.csv, header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))

working.list <- import.list[1]

names(working.list) <- c("both")
attach(working.list)
#--------------------------------------------------------------
both0<-both
both<-both0

names(both)
head(both)
str(both)

#-------------------------------------------------
#omit next part, Ceuta's file was modified to include this already
#0. Prepare data, Ceuta needs re-naming variables first.res.bs and last.res.bs-----
# 
# both[both$population %in% "Ceuta",c("first.res.bs", "first.res","first.d.seen")]
# 
# ind<-which(both$population %in% "Ceuta" & both$first.res.bs != both$first.res)
# both[ind,c("first.res.bs", "first.res","first.d.seen")]
# 
# both[both$population %in% "Ceuta",c("last.res.bs", "last.res")]
# 
# both$first.res[both$population%in%"Ceuta"]<-both$first.res.bs[both$population%in% "Ceuta"]
# both$last.res[both$population %in% "Ceuta"] <- both$last.res.bs[both$population%in%"Ceuta"]
# 
# str(both)
# names(both)
# cols <- c(26,27,30,31,55,56)
# both[,cols]<-lapply(both[,cols], as.Date, "%d/%m/%Y") #change columns to dates
# 
# str(both[,c("first.res", "last.res","first.cap.r","last.cap.r","first.bf.adult.r", "last.bf.adult.r")])
# 
# both$first.d.seen2 <- apply(both[,c("first.res", "last.res","first.cap.r","last.cap.r","first.bf.adult.r", "last.bf.adult.r")],1,function (x) min(x, na.rm=T))
# 
# both$last.d.seen2 <- apply(both[,c("first.res", "last.res","first.cap.r","last.cap.r","first.bf.adult.r", "last.bf.adult.r")],1,function (x) max(x, na.rm=T))
# 
# both[, c("first.d.seen","first.d.seen2")]
# head(both)
# tail(both)
#---------------------------------------------------
#1. Change data structure------------- 
#Each row should be one individual with all it's breeding events per year

both$year.ring <- ifelse(is.na(both$ring) & !is.na(both$code), paste(both$year, both$code, sep="-"), paste(both$year, both$ring, sep="-"))

head(both[order(both$year.ring),])

both$last.d.seen <- as.Date(both$last.d.seen, "%Y-%m-%d")
both$first.d.seen <- as.Date(both$first.d.seen, "%Y-%m-%d")
both$layingdate <- as.Date(both$layingdate, "%d/%m/%Y")
both$found_date.r <- as.Date(both$found_date.r, "%d/%m/%Y")
both$end_date.r <- as.Date(both$end_date.r, "%d/%m/%Y")

both$ld.minus10<-both$layingdate-10

str(both, list.len=500)
#Easier to extract earliest and latest date per individual per year...
#Use similar for loop used to extract brood fate dates in extract_breedingschedule.R:


for(i in 1:length(both$year)){  #for loop brood fates adults
  print(i)
  #options(warn=1)
  options(warn=0)
  ind <- which(both$year.ring[i] == both$year.ring)
  group.nests<-both[ind,]
  
  both$total.nests.peryear[i] <- length(group.nests$year)
  both$earliest.seen[i] <- min(group.nests$first.d.seen2, na.rm=T)
  both$latest.seen[i] <- max(group.nests$last.d.seen2, na.rm=T)
  both$min.ldminus10[i] <- min(group.nests$ld.minus10, na.rm=T)
  both$earliest.founddate[i] <- min(group.nests$found_date.r, na.rm=T)
  both$latest.enddate[i] <- max(group.nests$end_date.r, na.rm=T)
  both$min.layingdate[i] <- min(group.nests$layingdate, na.rm=T)
}

#change to dates:
both2<-both
names(both2)
cols <- c(101,104, 106:111)
both2[,cols]<-lapply(both2[,cols], as.Date, origin="1970-01-01") #change columns to dates
str(both2, list.len = 999)
str(both, list.len = 999)
#-----------debug------

tail(both2[order(both2$year.ring),])

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

#   2a. Start and end---------------------
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

str(indiv)
#indiv$bs.start<-as.Date(indiv$bs.start, origin="1970-01-01")
#indiv$bs.end<-as.Date(indiv$bs.end, origin="1970-01-01")
indiv$bs.start<-as.Date(indiv$bs.start, "%Y-%m-%d")
indiv$bs.end<-as.Date(indiv$bs.end, "%Y-%m-%d")


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
# library(date)
# indiv.chr<- indiv
# indiv.chr$min.ldminus10 <- as.character(indiv.chr$min.ldminus10)
# indiv.chr$earliest.seen <- as.character(indiv.chr$earliest.seen)
# indiv.chr$latest.seen <- as.character(indiv.chr$latest.seen)
# indiv.chr$nest <- as.numeric(indiv.chr$nest)
# indiv.chr$found_date.r <- as.character(indiv.chr$found_date.r)
# 
# str(indiv.chr)
# 
# indiv.chr[is.na(indiv.chr$min.ldminus10),]
# indiv.chr[is.na(indiv.chr$earliest.seen),] #mainly nests that disappeared
# indiv.chr[is.na(indiv.chr$latest.seen),]#mainly nests that disappeared
# 
# indiv.chr[is.na(indiv.chr$found_date.r)& indiv.chr$nest>0,]#positive nests with no founddate...ignore the ones with no layingdate?
# str(indiv.chr[is.na(indiv.chr$found_date.r)& indiv.chr$nest>0,])#16 observations
# 
#2015-CA3076 does not have any date
#both[both$year.ring %in% "2015-CA3076",]

#case indiv 785 has no date available
# indiv[785,] #2010-CA2656
# both[both$year.ring %in% "2010-CA2656",] #comments: Iwan's nest 2010-S-105/made appropriate changes in all my data (not std files)
# both[both$nest.id %in% "2010-S-24",]
# 
#----------------------------------------------------------
#   2b. Duration------------------------
str(indiv)
indiv$bs.length <- as.numeric(indiv$bs.end - indiv$bs.start)

str(indiv[indiv$bs.length ==0,]) #seen once and layingdates not known, only bred once in a year = 9 obs



#-------------------------------
#-------debug 2b.-------------
# both[both$year.ring %in% "2007-CA1513",] #Sexes in Ceuta had X instead of NA...corrected
# 
# indiv[indiv$bs.length >120,]
# 
# both[both$ring %in% "CA1520",]
#----------------------------


#explore issue2: conflicting sexes--------------

u1<-indiv[indiv$mol_sex_focal == indiv$mol_sex_mate & !is.na(indiv$year.ring),]
u1<-u1[!is.na(u1$year.ring),]

u2<-both[both$mol_sex_focal == both$mol_sex_mate,]
u2<-u2[!is.na(u2$year.ring),]
#---------------------------------------------

#----------------------------------------------------
#plot males vs females length of bs ONLY WITH mol_sex (in Ceuta these is not True for all...)

#Restrict sample:
#plus sample needs to be restricted to individuals ringed in previous years only

head(indiv)
indiv$year<-substr(indiv$year.ring, 1,4)
table(indiv$year)
# 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 
# 154  175  135  187  243   86  162  119  117  111

ind<-which(indiv$year.cr < indiv$year)
ringed.prev<-indiv[ind,]
str(ringed.prev) #654

#Use mol_sex---------------------------
#how many males and females with mol_sex?
table(indiv$mol_sex_focal) #without restricting to indiv ringed in previous year
# F   M    NA
# 507 597 473
length(indiv$mol_sex_focal) #1489
indiv2<-indiv[!is.na(indiv$mol_sex_focal),] #only indiv with mol_sex
#indiv<-indiv2

str(indiv2)#1104
str(indiv)#1489

indiv$pop.molsex <- paste(indiv$population, indiv$mol_sex_focal, sep="-")
unique(indiv$pop.molsex)
indiv$pop.molsex<-factor(indiv$pop.molsex)
#--------------

table(ringed.prev$mol_sex_focal) #restricting to indiv ringed in previous year
# F   M     NA
# 212 267   175
length(ringed.prev$mol_sex_focal) #654

ringed.prev2<-ringed.prev[!is.na(ringed.prev$mol_sex_focal),] #only indiv with mol_sex
ringed.prev2$pop.molsex <- paste(ringed.prev2$population, ringed.prev2$mol_sex_focal, sep="-")
unique(ringed.prev2$pop.molsex)
ringed.prev2$pop.molsex<-factor(ringed.prev2$pop.molsex)

#Use field_sex and ringed.prev2------------
ringed.prev
ringed.prev$pop.fieldsex <- paste(ringed.prev$population, ringed.prev$field_sex_focal, sep="-")
ringed.prev$pop.fieldsex<-factor(ringed.prev$pop.fieldsex)

table(ringed.prev$field_sex_focal)
# F   M     NA
# 302 352    0
length(ringed.prev$field_sex_focal)#654

table(ringed.prev$pop.fieldsex)
# Ceuta-F Ceuta-M  Maio-F  Maio-M 
# 88     166     214     186 

#-----------------------------
#histogram
hist(indiv$bs.length)
hist(ringed.prev2$bs.length)
hist(ringed.prev$bs.length)

indiv[indiv$bs.length>100,]

#-----------------
#-----BOXPLOT---------------
max(indiv$bs.length)#170
table(indiv$pop.molsex) #A) sample sizes of sample for indiv with mol sex and ringed anytime
# Ceuta-F Ceuta-M  Maio-F  Maio-M 
# 264     379     243     218 

max(ringed.prev2$bs.length)#140
table(ringed.prev2$pop.molsex) #B) sample sizes of sample for indiv with mol sex and ringed at least in the prev year
# Ceuta-F Ceuta-M  Maio-F  Maio-M 
# 88     166     124     101 

max(ringed.prev$bs.length)#140
table(ringed.prev$pop.fieldsex) #C) sample sizes of sample for indiv with fieldsex and ringed at least in the prev year
# Ceuta-F Ceuta-M  Maio-F  Maio-M 
# 88     166     214     186 


#A) plot for indiv with mol sex and ringed anytime
setwd("F:/Plovers/3rd Chapter/Exploratory results/plots")
tiff("test3.tiff", width=100, height=120, units="mm", res=500)
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
        ylim=c(0,200),                                                                
        boxwex=0.3, outwex=0.3,
        outcex=0.5)                                                                                                                    
mtext("   Ceuta (SP)                          Maio (KP)",                                                                       
      side = 1, line = 1.5, cex = 0.75)                                                                                                
y<-seq(0,200, 20)
axis(2, at=y,las=1, cex.axis=0.7) 
                                                                                                                         
mtext("Length of breeding shcedules (days)", side=2, line=2.5, cex=0.75)
#mtext("schedules (days)", side=2, line=2, cex=0.75)
mtext("Females", side=1, line=0.5, at=1, las=1,cex=0.6)
mtext("Males", side=1, line=0.5, at=2, las=1, cex=0.6)
mtext("Females", side=1, line=0.5, at=4, las=1,cex=0.6)
mtext("Males", side=1, line=0.5, at=5, las=1, cex=0.6)
#add sample sizes on top
mtext("264", side= 3, line = 0.5, at=1, cex=0.6)
mtext("379", side =3, line = 0.5, at=2, cex=0.6)
mtext("243", side=3, line=0.5, at=4, cex=0.6)
mtext("218", side=3, line=0.5, at=5, cex=0.6)
dev.off()

##B) plot for indiv with mol sex and ringed at least the previous year
setwd("F:/Plovers/3rd Chapter/Exploratory results/plots")
tiff("test1ringedprev.tiff", width=100, height=120, units="mm", res=500)
par(mfrow=c(1,1))
boxplot(bs.length~pop.molsex, data=ringed.prev2,                                                                            
        #horizontal = T,
        names = c("", "", "", ""),
        ylab="",                  
        at =c(1,2, 4,5), #specify position of each boxplot on x
        par(mar = c(7, 4, 4, 2)+ 0.1),    #enlarge plot's window default is c(5, 4, 4, 2) + 0.1.
        par(cex.lab=0.75), 
        par(cex.axis=0.7), 
        yaxt='n', 
        ylim=c(0,150),                                                                
        boxwex=0.3, outwex=0.3,
        outcex=0.5)                                                                                                                    
mtext("   Ceuta (SP)                          Maio (KP)",                                                                       
      side = 1, line = 1.5, cex = 0.75)                                                                                                
y<-seq(0,160, 20)
axis(2, at=y,las=1, cex.axis=0.7) 

mtext("Length of breeding shcedules (days)", side=2, line=2.5, cex=0.75)
#mtext("schedules (days)", side=2, line=2, cex=0.75)
mtext("Females", side=1, line=0.5, at=1, las=1,cex=0.6)
mtext("Males", side=1, line=0.5, at=2, las=1, cex=0.6)
mtext("Females", side=1, line=0.5, at=4, las=1,cex=0.6)
mtext("Males", side=1, line=0.5, at=5, las=1, cex=0.6)
#add sample sizes on top
mtext("88", side= 3, line = 0.5, at=1, cex=0.6)
mtext("166", side =3, line = 0.5, at=2, cex=0.6)
mtext("124", side=3, line=0.5, at=4, cex=0.6)
mtext("101", side=3, line=0.5, at=5, cex=0.6)
dev.off()


##c) plot for indiv with mol sex and ringed at least the previous year
setwd("F:/Plovers/3rd Chapter/Exploratory results/plots")
tiff("ringedprev_fieldsex.tiff", width=100, height=120, units="mm", res=500)
par(mfrow=c(1,1))
boxplot(bs.length~pop.fieldsex, data=ringed.prev,                                                                            
        #horizontal = T,
        names = c("", "", "", ""),
        ylab="",                  
        at =c(1,2, 4,5), #specify position of each boxplot on x
        par(mar = c(7, 4, 4, 2)+ 0.1),    #enlarge plot's window default is c(5, 4, 4, 2) + 0.1.
        par(cex.lab=0.75), 
        par(cex.axis=0.7), 
        yaxt='n', 
        ylim=c(0,150),                                                                
        boxwex=0.3, outwex=0.3,
        outcex=0.5)                                                                                                                    
mtext("   Ceuta (SP)                          Maio (KP)",                                                                       
      side = 1, line = 1.5, cex = 0.75)                                                                                                
y<-seq(0,160, 20)
axis(2, at=y,las=1, cex.axis=0.7) 

mtext("Length of breeding shcedules (days)", side=2, line=2.5, cex=0.75)
#mtext("schedules (days)", side=2, line=2, cex=0.75)
mtext("Females", side=1, line=0.5, at=1, las=1,cex=0.6)
mtext("Males", side=1, line=0.5, at=2, las=1, cex=0.6)
mtext("Females", side=1, line=0.5, at=4, las=1,cex=0.6)
mtext("Males", side=1, line=0.5, at=5, las=1, cex=0.6)
#add sample sizes on top
mtext("88", side= 3, line = 0.5, at=1, cex=0.6)
mtext("166", side =3, line = 0.5, at=2, cex=0.6)
mtext("214", side=3, line=0.5, at=4, cex=0.6)
mtext("186", side=3, line=0.5, at=5, cex=0.6)
dev.off()



#------------------------------------------------------
#--------------Plot start and end...standardized-----

#Standardize start and end: (using ALL data) WRONG...needs to scale per pop and per year
#indiv$bs.start.std[indiv$population %in% "Ceuta"] <- scale(indiv$bs.start[indiv$population %in% "Ceuta"])
#indiv$bs.end.std[indiv$population %in% "Ceuta"] <- scale(indiv$bs.end[indiv$population%in% "Ceuta"])

#indiv$bs.start.std[indiv$population %in% "Maio"] <- scale(indiv$bs.start[indiv$population %in% "Maio"])
#indiv$bs.end.std[indiv$population %in% "Maio"] <- scale(indiv$bs.end[indiv$population %in% "Maio"])

library(plyr)
indiv$year<-substr(indiv$year.ring, 0,4)
indiv$pop.year <- paste(indiv$population, indiv$year, sep="-")
indiv<-ddply(indiv, c("pop.year"), transform, bs.start.std= scale(bs.start))
indiv<-ddply(indiv, c("pop.year"), transform, bs.end.std=scale(bs.end))
names(indiv)

hist(indiv$bs.end.std[indiv$population %in% "Ceuta"])
hist(indiv$bs.start.std[indiv$population %in% "Ceuta"])

hist(indiv$bs.start.std[indiv$pop.year %in% "Maio-2015"])


#restrict sample again:
#Mol sex
ind<-which(indiv$year.cr < indiv$year)
ringed.prev3<-indiv[ind,]
str(ringed.prev3) #654

ringed.prev3<-ringed.prev3[!is.na(ringed.prev3$mol_sex_focal),] #only indiv with mol_sex
ringed.prev3$pop.molsex <- paste(ringed.prev3$population, ringed.prev3$mol_sex_focal, sep="-")
unique(ringed.prev3$pop.molsex)
ringed.prev3$pop.molsex<-factor(ringed.prev3$pop.molsex)

#field sex
ringed.prev4<-indiv[ind,]
ringed.prev4$pop.fieldsex <- paste(ringed.prev4$population, ringed.prev4$field_sex_focal, sep="-")
ringed.prev4$pop.fieldsex <- factor(ringed.prev4$pop.fieldsex)

table(ringed.prev4$pop.fieldsex)
# Ceuta-F Ceuta-M  Maio-F  Maio-M 
# 88     166     214     186 

#BOXPLOT
max(ringed.prev4$bs.start.std)

dev.off()
setwd("F:/Plovers/3rd Chapter/Exploratory results/plots")
tiff("bsstart_fieldsex.tiff", width=200, height=120, units="mm", res=500)
par(mfcol=c(1,2), #fill out by columns instead of mfrow
    cex=0.6,
    oma=c(1.5,2,1,1),
    mgp=c(2,0.6,0),
    tcl=-0.25)
boxplot(ringed.prev4$bs.start.std~ringed.prev4$pop.fieldsex, 
        horizontal=T,
        ylim=c(-3.0,3.5),
        names = c("", "", "", ""),
        ylab="",                  
        at =c(1,2, 4,5), #specify position of each boxplot on x
        par(mar = c(6, 4, 2, 0.5)+ 0.1),    #enlarge plot's window default is c(5, 4, 4, 2) + 0.1.
        par(cex.lab=0.75), 
        par(cex.axis=0.7), 
        yaxt='n',
        xaxt="n",
        boxwex=0.5, outwex=0.5,
        outcex=0.5)                                                                                                                    
                                                                                               
y<-seq(-3.0,3.5, 1)
axis(1, at=y,las=1, cex.axis=0.7) 

mtext("Breeding schedule start (standardized date)", side=1, line=2.5, cex=0.75)
#mtext("schedules (days)", side=2, line=2, cex=0.75)
mtext("Females", side=2, line=0.5, at=1, las=1,cex=0.6)
mtext("Males", side=2, line=0.5, at=2, las=1, cex=0.6)
mtext("Ceuta (SP)", side = 2, line = 4.0, at=1.5,cex = 0.75) 
                                                                        
      
mtext("Females", side=2, line=0.5, at=4, las=1,cex=0.6)
mtext("Males", side=2, line=0.5, at=5, las=1, cex=0.6)
mtext("Maio (KP)", side = 2, line = 4.0, at=4.5,cex = 0.75)

         
boxplot(ringed.prev4$bs.end.std~ringed.prev4$pop.fieldsex, 
        horizontal=T,
        ylim=c(-3.0,3.5),
        names = c("", "", "", ""),
        ylab="",                  
        at =c(1,2, 4,5), #specify position of each boxplot on x
        par(mar = c(6, 0.5, 2, 2)+ 0.1),    #enlarge plot's area default is c(5, 4, 4, 2) + 0.1.
        par(cex.lab=0.75), 
        par(cex.axis=0.7), 
        yaxt='n',
        xaxt="n",
        boxwex=0.5, outwex=0.5,
        outcex=0.5)                                                                                                                    

y<-seq(-3.0,3.5, 1)
axis(1, at=y,las=1, cex.axis=0.7) 

mtext("Breeding schedule end (standardized date)", side=1, line=2.5, cex=0.75)
#add sample sizes on left
mtext("88", side= 4, line = 0.5, at=1, las=1, cex=0.6)
mtext("166", side =4, line = 0.5, at=2, las=1,cex=0.6)
mtext("214", side=4, line=0.5, at=4, las=1,cex=0.6)
mtext("186", side=4, line=0.5, at=5, las=1,cex=0.6)
dev.off()

#----------------------------------------------------------------
#Violin plot
library(vioplot)

vioplot(ringed.prev4$bs.start.std[ringed.prev4$pop.fieldsex %in% "Ceuta-F"],
        ringed.prev4$bs.start.std[ringed.prev4$pop.fieldsex %in% "Ceuta-M"],
        ringed.prev4$bs.start.std[ringed.prev4$pop.fieldsex %in% "Maio-F"],
        ringed.prev4$bs.start.std[ringed.prev4$pop.fieldsex %in% "Maio-M"],
        col="grey", horizontal=T, border="black", lty=1, lwd=1, rectCol="black", 
        colMed="white", pchMed=19, add=FALSE, wex=1, 
        drawRect=TRUE)

#try with non std dates
# ringed.prev4$origin.jd<-paste(ringed.prev4$year,"01-01", sep="-")
# str(ringed.prev4$bs.start)
# 
# for(i in 1:length(ringed.prev4$year)){
#   ringed.prev4$juliand.bsstart[i]<-julian(ringed.prev4$bs.start[i], origin=as.Date(ringed.prev4$origin.jd[i]))  
# }
# 
# vioplot(ringed.prev4$juliand.bsstart[ringed.prev4$pop.fieldsex %in% "Ceuta-F"],
#         ringed.prev4$juliand.bsstart[ringed.prev4$pop.fieldsex %in% "Ceuta-M"],
#         ringed.prev4$juliand.bsstart[ringed.prev4$pop.fieldsex %in% "Maio-F"],
#         ringed.prev4$juliand.bsstart[ringed.prev4$pop.fieldsex %in% "Maio-M"],
#         col="grey", horizontal=T, border="black", lty=1, lwd=1, rectCol="black", 
#         colMed="white", pchMed=19, add=FALSE, wex=1, 
#         drawRect=TRUE)

#-----
vioplot(ringed.prev4$bs.end.std[ringed.prev4$pop.fieldsex %in% "Ceuta-F"],
        ringed.prev4$bs.end.std[ringed.prev4$pop.fieldsex %in% "Ceuta-M"],
        ringed.prev4$bs.end.std[ringed.prev4$pop.fieldsex %in% "Maio-F"],
        ringed.prev4$bs.end.std[ringed.prev4$pop.fieldsex %in% "Maio-M"],
        col="grey", horizontal=T, border="black", lty=1, lwd=1, rectCol="black", 
         colMed="white", pchMed=19, add=FALSE, wex=1, 
         drawRect=TRUE)

#---------------------------------------------------------------------------

#Plot Means and sd or se?....better to plot CI

se <- function(a) sd(a[!is.na(a)])/sqrt(length(a[!is.na(a)]))

#length
means.bslength <- aggregate(ringed.prev4$bs.length, by=list(ringed.prev4$pop.fieldsex), mean)
se.bs <- aggregate(ringed.prev4$bs.length, by=list(ringed.prev4$pop.fieldsex), se)
sd.bs <- aggregate(ringed.prev4$bs.length, by=list(ringed.prev4$pop.fieldsex), sd)
n.bs <- aggregate(ringed.prev4$bs.length, by=list(ringed.prev4$pop.fieldsex), length)
merge.bs <- merge(means.bslength, sd.bs, by=c("Group.1"))
merge.bs2<-merge(merge.bs, se.bs, by=c("Group.1"))
merge.bs3<-merge(merge.bs2,n.bs, by=c("Group.1"))
colnames(merge.bs3) <- c("pop.sex","mean.bslength", "sd", "se", "n")

merge.bs3$pop.sex <- as.numeric(merge.bs3$pop.sex)

dev.off()
plot(merge.bs3$pop.sex, merge.bs3$mean.bslength, 
     ylim=c(0,100), 
     xlim=c(1,4),
     type="n")
points(merge.bs3$pop.sex, merge.bs3$mean.bslength)
arrows(merge.bs3$pop.sex, merge.bs3$mean.bslength-merge.bs3$sd, 
       merge.bs3$pop.sex, merge.bs3$mean.bslength+merge.bs3$sd, 
       angle = 90, #ASR CI
       length=0.2,
       col="black",
       lty=1,
       code=3)

#bs.start
#Calculate Confidence intervals for normal distribution
error.ci <- function(x) (qnorm(0.975)*sd(x))/sqrt(length(x))
#left <- a-error
#right <- a+error

#check if bs.start is normal? Yes in both Ceuta and Maio
hist(ringed.prev4$bs.start.std[ringed.prev4$population %in% "Ceuta"])
shapiro.test(ringed.prev4$bs.start.std[ringed.prev4$population %in% "Ceuta"])
qqnorm(ringed.prev4$bs.start.std[ringed.prev4$population %in% "Ceuta"])
qqline(ringed.prev4$bs.start.std[ringed.prev4$population %in% "Ceuta"],lty=2)   #If sample is normally distributed, then the line should be straight.

hist(ringed.prev4$bs.start.std[ringed.prev4$population %in% "Maio"])
shapiro.test(ringed.prev4$bs.start.std[ringed.prev4$population %in% "Maio"])
qqnorm(ringed.prev4$bs.start.std[ringed.prev4$population %in% "Maio"])
qqline(ringed.prev4$bs.start.std[ringed.prev4$population %in% "Maio"],lty=2)   #If sample is normally distributed, then the line should be straight.
#-----

#calculate mean, sd, se and CI95%
means.bsstart <- aggregate(ringed.prev4$bs.start.std, by=list(ringed.prev4$pop.fieldsex), mean)
se.bs <- aggregate(ringed.prev4$bs.start.std, by=list(ringed.prev4$pop.fieldsex), se)
sd.bs <- aggregate(ringed.prev4$bs.start.std, by=list(ringed.prev4$pop.fieldsex), sd)
n.bs <- aggregate(ringed.prev4$bs.start.std, by=list(ringed.prev4$pop.fieldsex), length)
error.ci95<-aggregate(ringed.prev4$bs.start.std, by=list(ringed.prev4$pop.fieldsex), error.ci)
upper.ci <- means.bsstart$x+error.ci95$x
lower.ci <- means.bsstart$x-error.ci95$x
merge.bs.start <- merge(means.bsstart, sd.bs, by=c("Group.1"))
merge.bs2.start<-merge(merge.bs.start, se.bs, by=c("Group.1"))
merge.bs3.start<-merge(merge.bs2.start,n.bs, by=c("Group.1"))


colnames(merge.bs3.start) <- c("pop.sex","mean.bsstart", "sd", "se", "n")

#add confidence intervals to dataframe
merge.bs4.start<-cbind(merge.bs3.start,upper.ci,lower.ci)

merge.bs4.start$pop.sex <- as.numeric(merge.bs4.start$pop.sex)




dev.off()
plot(merge.bs4.start$pop.sex, merge.bs4.start$mean.bsstart, 
     ylim=c(-1,1.0), 
     xlim=c(1,4),
     type="n")
points(merge.bs4.start$pop.sex, merge.bs4.start$mean.bsstart)
arrows(merge.bs4.start$pop.sex, merge.bs4.start$upper.ci, 
       merge.bs4.start$pop.sex, merge.bs4.start$lower.ci, 
       angle = 90, #ASR CI
       length=0.2,
       col="black",
       lty=1,
       code=3)

#---------------------------------
#bs.end
#Calculate Confidence intervals for normal distribution
error.ci <- function(x) (qnorm(0.975)*sd(x))/sqrt(length(x))
#left <- a-error
#right <- a+error

#check if bs.start is normal? Yes in both Ceuta and Maio
hist(ringed.prev4$bs.end.std[ringed.prev4$population %in% "Ceuta"])
shapiro.test(ringed.prev4$bs.end.std[ringed.prev4$population %in% "Ceuta"])
qqnorm(ringed.prev4$bs.end.std[ringed.prev4$population %in% "Ceuta"])
qqline(ringed.prev4$bs.end.std[ringed.prev4$population %in% "Ceuta"],lty=2)   #If sample is normally distributed, then the line should be straight.

hist(ringed.prev4$bs.end.std[ringed.prev4$population %in% "Maio"])
shapiro.test(ringed.prev4$bs.end.std[ringed.prev4$population %in% "Maio"])
qqnorm(ringed.prev4$bs.end.std[ringed.prev4$population %in% "Maio"])
qqline(ringed.prev4$bs.end.std[ringed.prev4$population %in% "Maio"],lty=2)   #If sample is normally distributed, then the line should be straight.
#-----

#calculate mean, sd, se and CI95%
means.bsend <- aggregate(ringed.prev4$bs.end.std, by=list(ringed.prev4$pop.fieldsex), mean)
se.bs <- aggregate(ringed.prev4$bs.end.std, by=list(ringed.prev4$pop.fieldsex), se)
sd.bs <- aggregate(ringed.prev4$bs.end.std, by=list(ringed.prev4$pop.fieldsex), sd)
n.bs <- aggregate(ringed.prev4$bs.end.std, by=list(ringed.prev4$pop.fieldsex), length)
error.ci95<-aggregate(ringed.prev4$bs.end.std, by=list(ringed.prev4$pop.fieldsex), error.ci)
upper.ci <- means.bsend$V1+error.ci95$V1
lower.ci <- means.bsend$V1-error.ci95$V1
merge.bs.end <- merge(means.bsend, sd.bs, by=c("Group.1"))
merge.bs2.end<-merge(merge.bs.end, se.bs, by=c("Group.1"))
merge.bs3.end<-merge(merge.bs2.end,n.bs, by=c("Group.1"))


colnames(merge.bs3.end) <- c("pop.sex","mean.bsend", "sd", "se", "n")

#add confidence intervals to dataframe
merge.bs4.end<-cbind(merge.bs3.end,upper.ci,lower.ci)

merge.bs4.end$pop.sex <- as.numeric(merge.bs4.end$pop.sex)




dev.off()
plot(merge.bs4.end$pop.sex, merge.bs4.end$mean.bsend, 
     ylim=c(-1,1.0), 
     xlim=c(1,4),
     type="n")
points(merge.bs4.end$pop.sex, merge.bs4.end$mean.bsend)
arrows(merge.bs4.end$pop.sex, merge.bs4.end$upper.ci, 
       merge.bs4.end$pop.sex, merge.bs4.end$lower.ci, 
       angle = 90, #ASR CI
       length=0.2,
       col="black",
       lty=1,
       code=3)