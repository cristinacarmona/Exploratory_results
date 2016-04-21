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

#6th log: 13/04/2016: halfway through code using ALL pops
#7th log: 14/04/2016: Trying to solve issue1
#8th log: 15/04/2016: Mol sexes of tuzla???

#------------import files--------------------
setwd("F:/Plovers/3rd Chapter/Exploratory_results/input")

csvfiles <- list.files(path = ".", pattern='*\\.csv$', all.files=TRUE)
csvfiles

import.list <- lapply(csvfiles, read.csv, header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))

working.list <- import.list[3]

names(working.list) <- c("both")
attach(working.list)
#detach(working.list)
#--------------------------------------------------------------
both0<-both
both<-both0

names(both)
head(both)
str(both)#5143 obs

#--------function to convert dates to juliandates:---------------
datetojulian=function(x,year)    #x has to be a date in the date format %d-%m-%Y
{
  origin<-as.Date(paste(year,'-01-01', sep=""))
  juliandate<-(x-origin)+1
}

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
both$ring[both$population %in% "Ceuta"] %in% both$ring[both$population %in% "Maio"] #non of the rings in Ceuta are in Maio ,/

both$year.ring <- ifelse(is.na(both$ring) & !is.na(both$code), paste(both$year, both$code, sep="-"), paste(both$year, both$ring, sep="-"))

both$pop.sp <- paste(both$population, both$species, sep="-")
both$nest.id <- paste(both$pop.sp, both$year, both$site, both$nest, sep="-")

head(both[order(both$year.ring),])

both$last.d.seen <- as.Date(both$last.d.seen, "%d/%m/%Y")
both$first.d.seen <- as.Date(both$first.d.seen, "%d/%m/%Y")
both$layingdate <- as.Date(both$layingdate, "%d/%m/%Y")
both$found_date.r <- as.Date(both$found_date.r, "%d/%m/%Y")
both$end_date.r <- as.Date(both$end_date.r, "%d/%m/%Y")

both$ld.minus10<-both$layingdate-10

str(both, list.len=500)

#Restrict dataset to omit cases with no layingdate nor found_date---------
both1<-both[!is.na(both$layingdate) | !is.na(both$found_date.r), ] #102 obs that need to be omitted
str(both1) #5041 ,/ correct number of observations omitting 102 obs with no laying date nor founddate

#And delete duplicated nests:


#------------------------------------------------
#1.a) Variables relevant to all nests: clutch_size and no_chicks
both1$clutch_size1<-NA
both1$clutch_size2<-NA
both1$clutch_size3<-NA
both1$clutch_size4<-NA
both1$fate1<-NA
both1$fate2<-NA
both1$fate3<-NA
both1$fate4<-NA
both1$no_chicks1<-NA
both1$no_chicks2<-NA
both1$no_chicks3<-NA
both1$no_chicks4<-NA

      #1.a.i) Use layingdate to order all breeding events of individuals:

both1$jd.idate<- ifelse(!is.na(both1$layingdate), datetojulian(both1$layingdate, both1$year), datetojulian(both1$found_date.r, both1$year)) #julian date of initial date of a nest (using either laying_date or found_date)

#-----------------
i<-which(both1$year.ring %in% "1999-4497")
i<-1

for(i in 1:length(both1$year)){  #for loop brood fates adults
  print(i)
  #options(warn=1)
  options(warn=0)
  ind <- which(both1$year.ring[i] == both1$year.ring)
  group.nests<-both1[ind,]
  group.nests<-group.nests[order(group.nests$jd.idate),]
  both1$total.nests.peryear[i] <- length(group.nests$year)
  
  if(length(group.nests$year)==1){
    both1$ordinal.nest.peryear[i]<-1
    both1$clutch_size1[i]<-group.nests$clutch_size[1]
    both1$no_chicks1[i]<-group.nests$no_chicks[1]
    both1$fate1[i]<-group.nests$fate[1]
    
    both1$earliest.seen[i] <- min(group.nests$first.d.seen, na.rm=T)
    both1$latest.seen[i] <- max(group.nests$last.d.seen, na.rm=T)
    both1$min.ldminus10[i] <- min(group.nests$ld.minus10, na.rm=T)
    both1$earliest.founddate[i] <- min(group.nests$found_date.r, na.rm=T)
    both1$latest.enddate[i] <- max(group.nests$end_date.r, na.rm=T)
    both1$min.layingdate[i] <- min(group.nests$layingdate, na.rm=T)
  }else{
    if(length(group.nests$year)==2){
      both1$ordinal.nest.peryear[i]<-ifelse(both1$nest.id[i] %in% group.nests$nest.id[1],1,2)
      both1$clutch_size1[i]<-group.nests$clutch_size[1]
      both1$clutch_size2[i]<-group.nests$clutch_size[2]
      both1$no_chicks1[i]<-group.nests$no_chicks[1]
      both1$no_chicks2[i]<-group.nests$no_chicks[2]
      both1$fate1[i]<-group.nests$fate[1]
      both1$fate2[i]<-group.nests$fate[2]
      
      both1$earliest.seen[i] <- min(group.nests$first.d.seen, na.rm=T)
      both1$latest.seen[i] <- max(group.nests$last.d.seen, na.rm=T)
      both1$min.ldminus10[i] <- min(group.nests$ld.minus10, na.rm=T)
      both1$earliest.founddate[i] <- min(group.nests$found_date.r, na.rm=T)
      both1$latest.enddate[i] <- max(group.nests$end_date.r, na.rm=T)
      both1$min.layingdate[i] <- min(group.nests$layingdate, na.rm=T)
      
      }else{
        if(length(group.nests$year)==3){
        both1$ordinal.nest.peryear[i]<-ifelse(both1$nest.id[i] %in% group.nests$nest.id[1], 1,
                                              ifelse(both1$nest.id[i]%in%group.nests$nest.id[2],2,3))
        both1$clutch_size1[i]<-group.nests$clutch_size[1]
        both1$clutch_size2[i]<-group.nests$clutch_size[2]
        both1$clutch_size3[i]<-group.nests$clutch_size[3]
        both1$no_chicks1[i]<-group.nests$no_chicks[1]
        both1$no_chicks2[i]<-group.nests$no_chicks[2]
        both1$no_chicks3[i]<-group.nests$no_chicks[3]
        both1$fate1[i]<-group.nests$fate[1]
        both1$fate2[i]<-group.nests$fate[2]
        both1$fate3[i]<-group.nests$fate[3]
        
        both1$earliest.seen[i] <- min(group.nests$first.d.seen, na.rm=T)
        both1$latest.seen[i] <- max(group.nests$last.d.seen, na.rm=T)
        both1$min.ldminus10[i] <- min(group.nests$ld.minus10, na.rm=T)
        both1$earliest.founddate[i] <- min(group.nests$found_date.r, na.rm=T)
        both1$latest.enddate[i] <- max(group.nests$end_date.r, na.rm=T)
        both1$min.layingdate[i] <- min(group.nests$layingdate, na.rm=T)
        
        }else{
          if(length(group.nests$year)==4){
          both1$ordinal.nest.peryear[i]<-ifelse(both1$nest.id[i] %in% group.nests$nest.id[1], 1,
                                                ifelse(both1$nest.id[i]%in%group.nests$nest.id[2],2,
                                                       ifelse(both1$nest.id[i]%in%group.nests$nest.id[3],3,4)))
          both1$clutch_size1[i]<-group.nests$clutch_size[1]
          both1$clutch_size2[i]<-group.nests$clutch_size[2]
          both1$clutch_size3[i]<-group.nests$clutch_size[3]
          both1$clutch_size4[i]<-group.nests$clutch_size[4]
          both1$no_chicks1[i]<-group.nests$no_chicks[1]
          both1$no_chicks2[i]<-group.nests$no_chicks[2]
          both1$no_chicks3[i]<-group.nests$no_chicks[3]
          both1$no_chicks4[i]<-group.nests$no_chicks[4]
          both1$fate1[i]<-group.nests$fate[1]
          both1$fate2[i]<-group.nests$fate[2]
          both1$fate3[i]<-group.nests$fate[3]
          both1$fate4[i]<-group.nests$fate[4]
          
          both1$earliest.seen[i] <- min(group.nests$first.d.seen, na.rm=T)
          both1$latest.seen[i] <- max(group.nests$last.d.seen, na.rm=T)
          both1$min.ldminus10[i] <- min(group.nests$ld.minus10, na.rm=T)
          both1$earliest.founddate[i] <- min(group.nests$found_date.r, na.rm=T)
          both1$latest.enddate[i] <- max(group.nests$end_date.r, na.rm=T)
          both1$min.layingdate[i] <- min(group.nests$layingdate, na.rm=T)
          }
        }
      }
  }
}

#----------------------------------------------
#Easier to extract earliest and latest date per individual per year...
#Use similar for loop used to extract brood fate dates in extract_breedingschedule.R:
# 
# 
# for(i in 1:length(both$year)){  #for loop brood fates adults
#   print(i)
#   #options(warn=1)
#   options(warn=0)
#   ind <- which(both$year.ring[i] == both$year.ring)
#   group.nests<-both[ind,]
#   
#   #both$total.nests.peryear[i] <- length(group.nests$year)
#   both$earliest.seen[i] <- min(group.nests$first.d.seen2, na.rm=T)
#   both$latest.seen[i] <- max(group.nests$last.d.seen2, na.rm=T)
#   both$min.ldminus10[i] <- min(group.nests$ld.minus10, na.rm=T)
#   both$earliest.founddate[i] <- min(group.nests$found_date.r, na.rm=T)
#   both$latest.enddate[i] <- max(group.nests$end_date.r, na.rm=T)
#   both$min.layingdate[i] <- min(group.nests$layingdate, na.rm=T)
# }

#change to dates:
both2<-both1
names(both2)
cols <- c(115:120)
both2[,cols]<-lapply(both2[,cols], as.Date, origin="1970-01-01") #change columns to dates
str(both2, list.len = 999)
str(both, list.len = 999)

#-----------debug------
unique(both1$year.ring) #4465 nests with ordinal.nest.peryear ==1 should be equal to this number:
un.ids<-both2[both2$ordinal.nest.peryear==1,]
str(both2[both2$ordinal.nest.peryear==1,]) #4469....4 more...why? ,/ corrected
setdiff(unique(both1$year.ring), unique(un.ids$year.ring)) #SAME IDS....but there are some repeated?? Duplicated nests in file

ind<-which(duplicated(un.ids$year.ring)|duplicated(un.ids$year.ring, fromLast=T))
un.ids[ind,"nest.id"] #none after corrections
#some of the duplicated nests were duplicated because the mate's ring was equal to the focal's ring. 
#Deleted duplicate ids by hand in file v3 15/04/2016
# [1] "Maio-KP-2008-S--16"   "Maio-KP-2008-S--18"   "Maio-KP-2010-S-66"    "Maio-KP-2008-S--16"   "Maio-KP-2008-S--18"   "Maio-KP-2010-S-66"    "Tuzla-KP-1996-B-1276"
# [8] "Tuzla-KP-1996-B-1276" (Deleted one)

head(both2[both2$ordinal.nest.peryear==1,])



ind<-sample(rownames(un.ids),10)
un.ids[ind,]

# tail(both2[order(both2$year.ring),])
# 
# tail(both[order(both$year.ring) & both$total.nests.peryear>1,], n=-528)
# 
# ind<-both$nest.id[both$total.nests.peryear>1]
# 
# head(both[both$nest.id %in% ind,])
# both[both$ring %in% "802122903",]
#--------------------------------------------------------
#---------------------------------------------------------

#2. Calculate duration of breeding schedule----
#Definition of breeding schedule:
#     The overall time spent in the breeding area of each focal individual in a specific breeding season. 
indiv<-un.ids
names(indiv)
str(indiv) #4465

#   2a. Start and end---------------------
i<-4429
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

str(indiv[indiv$bs.length ==0,]) #seen once and layingdates not known, only bred once in a year = 8 obs



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
head(u1)
u1<-u1[!is.na(u1$population),] #89 cases where mol_sex_focal is equal to mol_sex_mate
str(u1)
table(u1$population)
# Ceuta Madagascar       Maio 
# 5         55         29 

#flag these cases:
indiv$sex.reliable<-ifelse(indiv$year.ring %in% u1$year.ring, "conflicting sex", NA)
str(indiv[indiv$sex.reliable %in% "conflicting sex",]) #89 ,/

#------------------------------------------------------------------------
#--------------write file-----------------------------------------------

write.csv(indiv, "F:/Plovers/3rd Chapter/Exploratory_results/output/bs_data_per_indiv.csv")
#----------------------------------------------------
#---------------------------------------------
#----------------------------------------------------
#Preliminary figures for Lab meeting and Sex determination Conflict, March-April 2016------
#----------------------------------------------------
#----------------------------------------------------
#----------------------------------------------------
#plot males vs females length of bs ONLY WITH mol_sex (in Ceuta these is not True for all...)

#Restrict sample:
#plus sample needs to be restricted to individuals ringed in previous years only

head(indiv)
#indiv$year<-substr(indiv$year.ring, 1,4)
table(indiv$year)
# 1996 1997 1998 1999 2000 2004 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 
# 208  344  185  355   46  147  197  224  162  246  369  103  298  499  389  693 

ind<-which(indiv$year.cr < indiv$year)
ringed.prev<-indiv[ind,]
str(ringed.prev) #654, 1357 obs

#Use mol_sex---------------------------
#how many males and females with mol_sex?
table(indiv$mol_sex_focal, useNA="always") #without restricting to indiv ringed in previous year
# ?          f          F          M no mol_sex          X       <NA> 
#   12         12       1441       1489         16          5       1490 

length(indiv$mol_sex_focal) #1489
indiv2<-indiv[!is.na(indiv$mol_sex_focal),] #only indiv with mol_sex
#indiv<-indiv2

str(indiv2)#2975
str(indiv)#1489

indiv$pop.sp.molsex <- paste(indiv$population, indiv$species,indiv$mol_sex_focal, sep="-")
unique(indiv$pop.molsex)
indiv$pop.sp.molsex<-factor(indiv$pop.sp.molsex)
table(indiv$pop.sp.molsex)
#--------------

table(ringed.prev$mol_sex_focal, useNA="always") #restricting to indiv ringed in previous year
# ?          f          F          M      no mol_sex     X       <NA> 
# 5          1        498        522          6          1        324 
length(ringed.prev$mol_sex_focal) #654

#this won't work as Tuzla has no mol_sexes
#ringed.prev2<-ringed.prev[!is.na(ringed.prev$mol_sex_focal),] #only indiv with mol_sex
#ringed.prev2$pop.molsex <- paste(ringed.prev2$population, ringed.prev2$mol_sex_focal, sep="-")
unique(ringed.prev2$pop.molsex)
ringed.prev2$pop.molsex<-factor(ringed.prev2$pop.molsex)

#Use field_sex and ringed.prev2------------
ringed.prev
ringed.prev$pop.sp.fieldsex <- paste(ringed.prev$population,ringed.prev$species, ringed.prev$field_sex_focal, sep="-")
ringed.prev$pop.sp.fieldsex<-factor(ringed.prev$pop.sp.fieldsex)

table(ringed.prev$field_sex_focal)
# F   M     NA
# 302 352    0
length(ringed.prev$field_sex_focal)#654

table(ringed.prev$pop.sp.fieldsex)
# Ceuta-F Ceuta-M  Maio-F  Maio-M 
# 88     166     214     186 
# Ceuta-SP-F       Ceuta-SP-M       Tuzla-KP-F       Tuzla-KP-M  Madagascar-MP-F  Madagascar-MP-M        Maio-KP-F 
# 150              203              138              122               20               21              252 
# Maio-KP-M Madagascar-WfP-F Madagascar-WfP-M Madagascar-KiP-F Madagascar-KiP-M 
# 221               56               53               55               66 

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
# Ceuta-F      Ceuta-M Madagascar-F Madagascar-M       Maio-F       Maio-M   Tuzla-F   Tuzla-M 
# 150          203          131          140          252          221          138          122 

# #A) plot for indiv with mol sex and ringed anytime
# setwd("F:/Plovers/3rd Chapter/Exploratory results/plots")
# tiff("test3.tiff", width=100, height=120, units="mm", res=500)
# par(mfrow=c(1,1))
# boxplot(bs.length~pop.molsex, data=indiv,                                                                            
#         #horizontal = T,
#         names = c("", "", "", ""),
#         ylab="",                  
#         at =c(1,2, 4,5), #specify position of each boxplot on x
#         par(mar = c(7, 4, 4, 2)+ 0.1),    #enlarge plot's window default is c(5, 4, 4, 2) + 0.1.
#         par(cex.lab=0.75), 
#         par(cex.axis=0.7), 
#         yaxt='n', 
#         ylim=c(0,200),                                                                
#         boxwex=0.3, outwex=0.3,
#         outcex=0.5)                                                                                                                    
# mtext("   Ceuta (SP)                          Maio (KP)",                                                                       
#       side = 1, line = 1.5, cex = 0.75)                                                                                                
# y<-seq(0,200, 20)
# axis(2, at=y,las=1, cex.axis=0.7) 
#                                                                                                                          
# mtext("Length of breeding shcedules (days)", side=2, line=2.5, cex=0.75)
# #mtext("schedules (days)", side=2, line=2, cex=0.75)
# mtext("Females", side=1, line=0.5, at=1, las=1,cex=0.6)
# mtext("Males", side=1, line=0.5, at=2, las=1, cex=0.6)
# mtext("Females", side=1, line=0.5, at=4, las=1,cex=0.6)
# mtext("Males", side=1, line=0.5, at=5, las=1, cex=0.6)
# #add sample sizes on top
# mtext("264", side= 3, line = 0.5, at=1, cex=0.6)
# mtext("379", side =3, line = 0.5, at=2, cex=0.6)
# mtext("243", side=3, line=0.5, at=4, cex=0.6)
# mtext("218", side=3, line=0.5, at=5, cex=0.6)
# dev.off()
# 
# ##B) plot for indiv with mol sex and ringed at least the previous year
# setwd("F:/Plovers/3rd Chapter/Exploratory results/plots")
# tiff("test1ringedprev.tiff", width=100, height=120, units="mm", res=500)
# par(mfrow=c(1,1))
# boxplot(bs.length~pop.molsex, data=ringed.prev2,                                                                            
#         #horizontal = T,
#         names = c("", "", "", ""),
#         ylab="",                  
#         at =c(1,2, 4,5), #specify position of each boxplot on x
#         par(mar = c(7, 4, 4, 2)+ 0.1),    #enlarge plot's window default is c(5, 4, 4, 2) + 0.1.
#         par(cex.lab=0.75), 
#         par(cex.axis=0.7), 
#         yaxt='n', 
#         ylim=c(0,150),                                                                
#         boxwex=0.3, outwex=0.3,
#         outcex=0.5)                                                                                                                    
# mtext("   Ceuta (SP)                          Maio (KP)",                                                                       
#       side = 1, line = 1.5, cex = 0.75)                                                                                                
# y<-seq(0,160, 20)
# axis(2, at=y,las=1, cex.axis=0.7) 
# 
# mtext("Length of breeding shcedules (days)", side=2, line=2.5, cex=0.75)
# #mtext("schedules (days)", side=2, line=2, cex=0.75)
# mtext("Females", side=1, line=0.5, at=1, las=1,cex=0.6)
# mtext("Males", side=1, line=0.5, at=2, las=1, cex=0.6)
# mtext("Females", side=1, line=0.5, at=4, las=1,cex=0.6)
# mtext("Males", side=1, line=0.5, at=5, las=1, cex=0.6)
# #add sample sizes on top
# mtext("88", side= 3, line = 0.5, at=1, cex=0.6)
# mtext("166", side =3, line = 0.5, at=2, cex=0.6)
# mtext("124", side=3, line=0.5, at=4, cex=0.6)
# mtext("101", side=3, line=0.5, at=5, cex=0.6)
# dev.off()


##c) plot for indiv with field sex and ringed at least the previous year
setwd("F:/Plovers/3rd Chapter/Exploratory results/plots")
tiff("ringedprev_fieldsex.tiff", width=100, height=120, units="mm", res=500)
par(mfrow=c(1,1))

#reorder factor levels:
print(levels(ringed.prev$pop.sp.fieldsex))
ringed.prev$pop.sp.fieldsex <- factor(ringed.prev$pop.sp.fieldsex, levels(ringed.prev$pop.sp.fieldsex)[c(1,2,11,12,5,6,9,10,7,8,3,4)])
#plot:
boxplot(bs.length~pop.sp.fieldsex, data=ringed.prev,                                                                            
        #horizontal = T,
        names = c("", "", "", "", "","","","","","","",""),
        ylab="",                  
        at =c(1,2, 4,5, 7,8, 10,11, 13,14, 16,17), #specify position of each boxplot on x
        par(mar = c(7, 4, 4, 2)+ 0.1),    #enlarge plot's window default is c(5, 4, 4, 2) + 0.1.
        par(cex.lab=0.75), 
        par(cex.axis=0.7), 
        yaxt='n', 
        ylim=c(0,150),                                                                
        boxwex=0.3, outwex=0.3,
        outcex=0.5)                                                                                                                                                                                                      
y<-seq(0,160, 20)
axis(2, at=y,las=1, cex.axis=0.7) 

mtext("Length of breeding shcedules (days)", side=2, line=2.5, cex=0.75)
#mtext("schedules (days)", side=2, line=2, cex=0.75)
mtext("F", side=1, line=0.5, at=1, las=1,cex=0.6)
mtext("M", side=1, line=0.5, at=2, las=1, cex=0.6)
mtext("Ceuta (SP)", side = 1, line = 1.5,at=1.5, cex = 0.50)

mtext("F", side=1, line=0.5, at=4, las=1,cex=0.6)
mtext("M", side=1, line=0.5, at=5, las=1, cex=0.6)
mtext("Tuzla (KP)", side = 1, line = 1.5,at=4.5, cex = 0.50)

mtext("F", side=1, line=0.5, at=7, las=1,cex=0.6)
mtext("M", side=1, line=0.5, at=8, las=1, cex=0.6)
mtext("Mad (MP)", side = 1, line = 1.5,at=7.5, cex = 0.50)

mtext("F", side=1, line=0.5, at=10, las=1,cex=0.6)
mtext("M", side=1, line=0.5, at=11, las=1, cex=0.6)
mtext("Maio (KP)", side = 1, line = 1.5,at=10.5, cex = 0.50)

mtext("F", side=1, line=0.5, at=13, las=1,cex=0.6)
mtext("M", side=1, line=0.5, at=14, las=1, cex=0.6)
mtext("Mad (WfP)", side = 1, line = 1.5,at=13.5, cex = 0.50)

mtext("F", side=1, line=0.5, at=16, las=1,cex=0.6)
mtext("M", side=1, line=0.5, at=17, las=1, cex=0.6)
mtext("Mad (KiP)", side = 1, line = 1.5,at=16.5, cex = 0.50)

#add sample sizes on top
# Ceuta-SP-F       Ceuta-SP-M       Tuzla-KP-F       Tuzla-KP-M  Madagascar-MP-F  Madagascar-MP-M        Maio-KP-F 
# 150              203              138              122               20               21              252 
# Maio-KP-M Madagascar-WfP-F Madagascar-WfP-M Madagascar-KiP-F Madagascar-KiP-M 
# 221               56               53               55               66 
mtext(c("150","203","138","122","20","21","252","221","56","53","55","66"), side= 3, line = 0.5, 
      at=c(1,2,4,5,7,8,10,11,13,14,16,17), cex=0.6)
dev.off()

#---------------------------------------------------------
#Try plotting mol_sex when available and field_sex in Tuzla ONLY
ringed.prev$pop.sp.sex<-ifelse(ringed.prev$population %in% "Tuzla", 
                               paste(ringed.prev$population, ringed.prev$species, ringed.prev$field_sex_focal, sep="-"),
                               paste(ringed.prev$population, ringed.prev$species, ringed.prev$mol_sex_focal, sep="-"))

table(ringed.prev$pop.sp.sex)

factors.pop.sp.sex<-factor(ringed.prev$pop.sp.sex)
factors.wanted<-levels(factors.pop.sp.sex)[c(3,4,20,21,10,11,16,17,13,14,7,8)]

plot.data<-ringed.prev[ringed.prev$pop.sp.sex %in% factors.wanted,]
plot.data$pop.sp.sex<-factor(plot.data$pop.sp.sex,levels(plot.data$pop.sp.sex)[c(1,2,11,12,5,6,9,10,7,8,3,4)])
print(levels(plot.data$pop.sp.sex))

table(plot.data$pop.sp.sex)
# Ceuta-SP-F       Ceuta-SP-M       Tuzla-KP-F       Tuzla-KP-M  Madagascar-MP-F  Madagascar-MP-M        Maio-KP-F 
# 142              181              138              122               17               17              241 
# Maio-KP-M Madagascar-WfP-F Madagascar-WfP-M Madagascar-KiP-F Madagascar-KiP-M 
# 220               54               49               44               55 

#---------------plot:
setwd("F:/Plovers/3rd Chapter/Exploratory_results/plots")
tiff("ringedprev_molsex(tuzla fieldsex).tiff", width=120, height=100, units="mm", res=500)
par(mfrow=c(1,1))

#reorder factor levels:

#plot:
boxplot(bs.length~pop.sp.sex, data=plot.data,                                                                            
        #horizontal = T,
        names = c("", "", "", "", "","","","","","","",""),
        ylab="",                  
        at =c(1,2, 4,5, 7,8, 10,11, 13,14, 16,17), #specify position of each boxplot on x
        par(mar = c(7, 4, 4, 2)+ 0.1),    #enlarge plot's window default is c(5, 4, 4, 2) + 0.1.
        par(cex.lab=0.75), 
        par(cex.axis=0.7), 
        yaxt='n', 
        ylim=c(0,150),                                                                
        boxwex=0.3, outwex=0.3,
        outcex=0.5)                                                                                                                                                                                                      
y<-seq(0,160, 20)
axis(2, at=y,las=1, cex.axis=0.7) 

mtext("Length of breeding shcedules (days)", side=2, line=2.5, cex=0.75)
#mtext("schedules (days)", side=2, line=2, cex=0.75)
mtext("F", side=1, line=0.5, at=1, las=1,cex=0.6)
mtext("M", side=1, line=0.5, at=2, las=1, cex=0.6)
mtext("Ceuta (SP)", side = 1, line = 1.5,at=1.5, cex = 0.50)

mtext("F", side=1, line=0.5, at=4, las=1,cex=0.6)
mtext("M", side=1, line=0.5, at=5, las=1, cex=0.6)
mtext("Tuzla (KP)", side = 1, line = 1.5,at=4.5, cex = 0.50)

mtext("F", side=1, line=0.5, at=7, las=1,cex=0.6)
mtext("M", side=1, line=0.5, at=8, las=1, cex=0.6)
mtext("Mad (MP)", side = 1, line = 1.5,at=7.5, cex = 0.50)

mtext("F", side=1, line=0.5, at=10, las=1,cex=0.6)
mtext("M", side=1, line=0.5, at=11, las=1, cex=0.6)
mtext("Maio (KP)", side = 1, line = 1.5,at=10.5, cex = 0.50)

mtext("F", side=1, line=0.5, at=13, las=1,cex=0.6)
mtext("M", side=1, line=0.5, at=14, las=1, cex=0.6)
mtext("Mad (WfP)", side = 1, line = 1.5,at=13.5, cex = 0.50)

mtext("F", side=1, line=0.5, at=16, las=1,cex=0.6)
mtext("M", side=1, line=0.5, at=17, las=1, cex=0.6)
mtext("Mad (KiP)", side = 1, line = 1.5,at=16.5, cex = 0.50)

#add sample sizes on top
# Ceuta-SP-F       Ceuta-SP-M       Tuzla-KP-F       Tuzla-KP-M  Madagascar-MP-F  Madagascar-MP-M        Maio-KP-F 
# 142              181              138              122               17               17              241 
# Maio-KP-M Madagascar-WfP-F Madagascar-WfP-M Madagascar-KiP-F Madagascar-KiP-M 
# 220               54               49               44               55 
mtext(c("142","181","138","122","17","17","241","220","54","49","44","55"), side= 3, line = 0.5, 
      at=c(1,2,4,5,7,8,10,11,13,14,16,17), cex=0.5)
dev.off()











#------------------------------------------------------
#--------------Plot start and end...standardized-----

#Standardize start and end: (using ALL data) WRONG...needs to scale per pop and per year
#indiv$bs.start.std[indiv$population %in% "Ceuta"] <- scale(indiv$bs.start[indiv$population %in% "Ceuta"])
#indiv$bs.end.std[indiv$population %in% "Ceuta"] <- scale(indiv$bs.end[indiv$population%in% "Ceuta"])

#indiv$bs.start.std[indiv$population %in% "Maio"] <- scale(indiv$bs.start[indiv$population %in% "Maio"])
#indiv$bs.end.std[indiv$population %in% "Maio"] <- scale(indiv$bs.end[indiv$population %in% "Maio"])

library(plyr)
#indiv$year<-substr(indiv$year.ring, 0,4)
indiv$pop.year <- paste(indiv$population, indiv$year, sep="-")
indiv<-ddply(indiv, c("pop.year"), transform, bs.start.std= scale(bs.start))
indiv<-ddply(indiv, c("pop.year"), transform, bs.end.std=scale(bs.end))
names(indiv)

hist(indiv$bs.end.std[indiv$population %in% "Ceuta"])
hist(indiv$bs.start.std[indiv$population %in% "Ceuta"])

hist(indiv$bs.start.std[indiv$population %in% "Tuzla"])
hist(indiv$bs.end.std[indiv$population %in% "Tuzla"])

hist(indiv$bs.start.std[indiv$population %in% "Maio"])
hist(indiv$bs.end.std[indiv$population %in% "Maio"])


#restrict sample again:
#Mol sex
ind<-which(indiv$year.cr < indiv$year)
ringed.prev3<-indiv[ind,]
str(ringed.prev3) #1357

ringed.prev3$pop.sp.sex<-ifelse(ringed.prev3$population %in% "Tuzla", 
                               paste(ringed.prev3$population, ringed.prev3$species, ringed.prev3$field_sex_focal, sep="-"),
                               paste(ringed.prev3$population, ringed.prev3$species, ringed.prev3$mol_sex_focal, sep="-"))

table(ringed.prev3$pop.sp.sex)

factors.pop.sp.sex<-factor(ringed.prev3$pop.sp.sex)
factors.wanted<-levels(factors.pop.sp.sex)[c(3,4,20,21,10,11,16,17,13,14,7,8)]

plot.data<-ringed.prev3[ringed.prev3$pop.sp.sex %in% factors.wanted,]
plot.data$pop.sp.sex<-factor(plot.data$pop.sp.sex)
plot.data$pop.sp.sex<-factor(plot.data$pop.sp.sex,levels(plot.data$pop.sp.sex)[c(1,2,11,12,5,6,9,10,7,8,3,4)])

print(levels(plot.data$pop.sp.sex))

table(plot.data$pop.sp.sex)

# Ceuta-SP-F       Ceuta-SP-M       Tuzla-KP-F       Tuzla-KP-M  Madagascar-MP-F  Madagascar-MP-M        Maio-KP-F 
# 142              181              138              122               17               17              241 
# Maio-KP-M Madagascar-WfP-F Madagascar-WfP-M Madagascar-KiP-F Madagascar-KiP-M 
# 220               54               49               44               55 



# #BOXPLOT
# max(ringed.prev4$bs.start.std)
# 
# dev.off()
# setwd("F:/Plovers/3rd Chapter/Exploratory results/plots")
# tiff("bsstart_fieldsex.tiff", width=200, height=120, units="mm", res=500)
# par(mfcol=c(1,2), #fill out by columns instead of mfrow
#     cex=0.6,
#     oma=c(1.5,2,1,1),
#     mgp=c(2,0.6,0),
#     tcl=-0.25)
# boxplot(ringed.prev4$bs.start.std~ringed.prev4$pop.fieldsex, 
#         horizontal=T,
#         ylim=c(-3.0,3.5),
#         names = c("", "", "", ""),
#         ylab="",                  
#         at =c(1,2, 4,5), #specify position of each boxplot on x
#         par(mar = c(6, 4, 2, 0.5)+ 0.1),    #enlarge plot's window default is c(5, 4, 4, 2) + 0.1.
#         par(cex.lab=0.75), 
#         par(cex.axis=0.7), 
#         yaxt='n',
#         xaxt="n",
#         boxwex=0.5, outwex=0.5,
#         outcex=0.5)                                                                                                                    
#                                                                                                
# y<-seq(-3.0,3.5, 1)
# axis(1, at=y,las=1, cex.axis=0.7) 
# 
# mtext("Breeding schedule start (standardized date)", side=1, line=2.5, cex=0.75)
# #mtext("schedules (days)", side=2, line=2, cex=0.75)
# mtext("Females", side=2, line=0.5, at=1, las=1,cex=0.6)
# mtext("Males", side=2, line=0.5, at=2, las=1, cex=0.6)
# mtext("Ceuta (SP)", side = 2, line = 4.0, at=1.5,cex = 0.75) 
#                                                                         
#       
# mtext("Females", side=2, line=0.5, at=4, las=1,cex=0.6)
# mtext("Males", side=2, line=0.5, at=5, las=1, cex=0.6)
# mtext("Maio (KP)", side = 2, line = 4.0, at=4.5,cex = 0.75)
# 
#          
# boxplot(ringed.prev4$bs.end.std~ringed.prev4$pop.fieldsex, 
#         horizontal=T,
#         ylim=c(-3.0,3.5),
#         names = c("", "", "", ""),
#         ylab="",                  
#         at =c(1,2, 4,5), #specify position of each boxplot on x
#         par(mar = c(6, 0.5, 2, 2)+ 0.1),    #enlarge plot's area default is c(5, 4, 4, 2) + 0.1.
#         par(cex.lab=0.75), 
#         par(cex.axis=0.7), 
#         yaxt='n',
#         xaxt="n",
#         boxwex=0.5, outwex=0.5,
#         outcex=0.5)                                                                                                                    
# 
# y<-seq(-3.0,3.5, 1)
# axis(1, at=y,las=1, cex.axis=0.7) 
# 
# mtext("Breeding schedule end (standardized date)", side=1, line=2.5, cex=0.75)
# #add sample sizes on left
# mtext("88", side= 4, line = 0.5, at=1, las=1, cex=0.6)
# mtext("166", side =4, line = 0.5, at=2, las=1,cex=0.6)
# mtext("214", side=4, line=0.5, at=4, las=1,cex=0.6)
# mtext("186", side=4, line=0.5, at=5, las=1,cex=0.6)
# dev.off()
# 
# #----------------------------------------------------------------
# #Violin plot
library(vioplot)

setwd("F:/Plovers/3rd Chapter/Exploratory_results/plots")
tiff("ringedprev_molsex(tuzla fieldsex)_start.tiff", width=100, height=150, units="mm", res=500)
par(mfrow=c(1,1))

boxplot(plot.data$bs.start.std~plot.data$pop.sp.sex,
                horizontal=T,
                ylim=c(-3.0,3.5),
                names = c("", "", "", "","","","","","","","",""),
                ylab="",
        at =c(1,2, 4,5, 7,8, 10,11, 13,14, 16,17), #specify position of each boxplot on x
                par(mar = c(5, 4, 4, 2)+ 0.1),    #enlarge plot's area default is c(5, 4, 4, 2) + 0.1.
                par(cex.lab=0.75),
                par(cex.axis=0.7),
                yaxt='n',
                xaxt="n",
                type="n",
                boxwex=0.5, outwex=0.5,
                outcex=0.5, border="white")
y<-seq(-3.0,3.5, 1)
axis(1, at=y,las=1, cex.axis=0.7)
x<-c(1,2, 4,5, 7,8, 10,11, 13,14, 16,17)
axis(2, at=x, las=1, cex.axis=0.7, labels=c("","","","","","","","","","","",""))

vioplot(ringed.prev3$bs.start.std[ringed.prev3$pop.sp.sex %in% "Ceuta-SP-F"],
        ringed.prev3$bs.start.std[ringed.prev3$pop.sp.sex %in% "Ceuta-SP-M"],
        ringed.prev3$bs.start.std[ringed.prev3$pop.sp.sex %in% "Tuzla-KP-F"],
        ringed.prev3$bs.start.std[ringed.prev3$pop.sp.sex %in% "Tuzla-KP-M"],
        ringed.prev3$bs.start.std[ringed.prev3$pop.sp.sex %in% "Madagascar-MP-F"],
        ringed.prev3$bs.start.std[ringed.prev3$pop.sp.sex %in% "Madagascar-MP-M"],
        ringed.prev3$bs.start.std[ringed.prev3$pop.sp.sex %in% "Maio-KP-F"],
        ringed.prev3$bs.start.std[ringed.prev3$pop.sp.sex %in% "Maio-KP-M"],
        ringed.prev3$bs.start.std[ringed.prev3$pop.sp.sex %in% "Madagascar-WfP-F"],
        ringed.prev3$bs.start.std[ringed.prev3$pop.sp.sex %in% "Madagascar-WfP-M"],
        ringed.prev3$bs.start.std[ringed.prev3$pop.sp.sex %in% "Madagascar-KiP-F"],
        ringed.prev3$bs.start.std[ringed.prev3$pop.sp.sex %in% "Madagascar-KiP-M"],
        at =c(1,2, 4,5, 7,8, 10,11, 13,14, 16,17),
        names=c("","","","","","","","","","","",""),
        col="white", horizontal=T, #border="black",
        lty=1, lwd=1, #rectCol="black", 
        colMed="black", pchMed=20, add=T, wex=1, 
        drawRect=TRUE, cex=0.6)
mtext("Start of breeding shcedule (Standardized date)", side=1, line=1.8, cex=0.75)
#mtext("schedules (days)", side=2, line=2, cex=0.75)
mtext("F", side=2, line=0.7, at=1, las=1,cex=0.6)
mtext("M", side=2, line=0.7, at=2, las=1, cex=0.6)
mtext("Ceuta (SP)", side = 2, line = 1.5,at=1.5, cex = 0.50, adj=1,las=1)

mtext("F", side=2, line=0.7, at=4, las=1,cex=0.6)
mtext("M", side=2, line=0.7, at=5, las=1, cex=0.6)
mtext("Tuzla (KP)", side = 2, line = 1.5,at=4.5, cex = 0.50,adj=1,las=1)

mtext("F", side=2, line=0.7, at=7, las=1,cex=0.6)
mtext("M", side=2, line=0.7, at=8, las=1, cex=0.6)
mtext("Mad (MP)", side = 2, line = 1.5,at=7.5, cex = 0.50,adj=1,las=1)

mtext("F", side=2, line=0.7, at=10, las=1,cex=0.6)
mtext("M", side=2, line=0.7, at=11, las=1, cex=0.6)
mtext("Maio (KP)", side = 2, line = 1.5,at=10.5, cex = 0.50,adj=1,las=1)

mtext("F", side=2, line=0.7, at=13, las=1,cex=0.6)
mtext("M", side=2, line=0.7, at=14, las=1, cex=0.6)
mtext("Mad (WfP)", side = 2, line = 1.5,at=13.5, cex = 0.50,adj=1, las=1)

mtext("F", side=2, line=0.7, at=16, las=1,cex=0.6)
mtext("M", side=2, line=0.7, at=17, las=1, cex=0.6)
mtext("Mad (KiP)", side = 2, line = 1.5,at=16.5, cex = 0.50,adj=1, las=1)

dev.off()
#----------------------------
#try with ggplot
library(ggplot2)
ggplot(plot.data, aes(x=pop.sp.sex, y=bs.start.std)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred", orientation="horizontal")+
  geom_boxplot(width=0.1) + theme_minimal()


#--------------------------------
#-----END.date
setwd("F:/Plovers/3rd Chapter/Exploratory_results/plots")
tiff("ringedprev_molsex(tuzla fieldsex)_end.tiff", width=100, height=150, units="mm", res=500)
par(mfrow=c(1,1))

boxplot(plot.data$bs.start.std~plot.data$pop.sp.sex,
        horizontal=T,
        ylim=c(-3.0,3.5),
        names = c("", "", "", "","","","","","","","",""),
        ylab="",
        at =c(1,2, 4,5, 7,8, 10,11, 13,14, 16,17), #specify position of each boxplot on x
        par(mar = c(5, 4, 4, 2)+ 0.1),    #enlarge plot's area default is c(5, 4, 4, 2) + 0.1.
        par(cex.lab=0.75),
        par(cex.axis=0.7),
        yaxt='n',
        xaxt="n",
        type="n",
        boxwex=0.5, outwex=0.5,
        outcex=0.5, border="white")
y<-seq(-3.0,3.5, 1)
axis(1, at=y,las=1, cex.axis=0.7)
x<-c(1,2, 4,5, 7,8, 10,11, 13,14, 16,17)
axis(2, at=x, las=1, cex.axis=0.7, labels=c("","","","","","","","","","","",""))

vioplot(ringed.prev3$bs.end.std[ringed.prev3$pop.sp.sex %in% "Ceuta-SP-F"],
        ringed.prev3$bs.end.std[ringed.prev3$pop.sp.sex %in% "Ceuta-SP-M"],
        ringed.prev3$bs.end.std[ringed.prev3$pop.sp.sex %in% "Tuzla-KP-F"],
        ringed.prev3$bs.end.std[ringed.prev3$pop.sp.sex %in% "Tuzla-KP-M"],
        ringed.prev3$bs.end.std[ringed.prev3$pop.sp.sex %in% "Madagascar-MP-F"],
        ringed.prev3$bs.end.std[ringed.prev3$pop.sp.sex %in% "Madagascar-MP-M"],
        ringed.prev3$bs.end.std[ringed.prev3$pop.sp.sex %in% "Maio-KP-F"],
        ringed.prev3$bs.end.std[ringed.prev3$pop.sp.sex %in% "Maio-KP-M"],
        ringed.prev3$bs.end.std[ringed.prev3$pop.sp.sex %in% "Madagascar-WfP-F"],
        ringed.prev3$bs.end.std[ringed.prev3$pop.sp.sex %in% "Madagascar-WfP-M"],
        ringed.prev3$bs.end.std[ringed.prev3$pop.sp.sex %in% "Madagascar-KiP-F"],
        ringed.prev3$bs.end.std[ringed.prev3$pop.sp.sex %in% "Madagascar-KiP-M"],
        at =c(1,2, 4,5, 7,8, 10,11, 13,14, 16,17),
        names=c("","","","","","","","","","","",""),
        col="white", horizontal=T, #border="black",
        lty=1, lwd=1, #rectCol="black", 
        colMed="black", pchMed=20, add=T, wex=1, 
        drawRect=TRUE, cex=0.6)
mtext("End of breeding shcedule (Standardized date)", side=1, line=1.8, cex=0.75)
#mtext("schedules (days)", side=2, line=2, cex=0.75)
mtext("F", side=2, line=0.7, at=1, las=1,cex=0.6)
mtext("M", side=2, line=0.7, at=2, las=1, cex=0.6)
mtext("Ceuta (SP)", side = 2, line = 1.5,at=1.5, cex = 0.50, adj=1,las=1)

mtext("F", side=2, line=0.7, at=4, las=1,cex=0.6)
mtext("M", side=2, line=0.7, at=5, las=1, cex=0.6)
mtext("Tuzla (KP)", side = 2, line = 1.5,at=4.5, cex = 0.50,adj=1,las=1)

mtext("F", side=2, line=0.7, at=7, las=1,cex=0.6)
mtext("M", side=2, line=0.7, at=8, las=1, cex=0.6)
mtext("Mad (MP)", side = 2, line = 1.5,at=7.5, cex = 0.50,adj=1,las=1)

mtext("F", side=2, line=0.7, at=10, las=1,cex=0.6)
mtext("M", side=2, line=0.7, at=11, las=1, cex=0.6)
mtext("Maio (KP)", side = 2, line = 1.5,at=10.5, cex = 0.50,adj=1,las=1)

mtext("F", side=2, line=0.7, at=13, las=1,cex=0.6)
mtext("M", side=2, line=0.7, at=14, las=1, cex=0.6)
mtext("Mad (WfP)", side = 2, line = 1.5,at=13.5, cex = 0.50,adj=1, las=1)

mtext("F", side=2, line=0.7, at=16, las=1,cex=0.6)
mtext("M", side=2, line=0.7, at=17, las=1, cex=0.6)
mtext("Mad (KiP)", side = 2, line = 1.5,at=16.5, cex = 0.50,adj=1, las=1)

dev.off()
#---------------------------------------------------------------------------

