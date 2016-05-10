#Analysing breeding schedule length (bs) data from Ceuta, Maio, Tuzla and Mad
#20/04/2016
#by Cristina Carmona-Isunza

#1st log -20/04/2016 Create script
#2nd log - 21/04/2016 Explore relations
#3rd log - 22/04/2016 Explore problems in models as some coef are not calculated because of singularities
#4th log - 26/04/2016 Establish best model: gaussian (after exploring gamma)
#5th log - 27/04/2016 Used gaussian model...its better
#6th log - 04/05/2016 Re-analysed data using available sex (fieldsex or molsex), subsetted data to use only individuals ringed the year previous to the focal year
#7th log - 05/05/2016 Ran analysis using fieldsex for Tuzla and molsex for the rest of the pops, results are different...

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
#  Years used:
#  Ceuta 2007-2012
#  Madagascar 2012-2015, 2010
#  Maio 2008-2015
# Tuzla 1997-2000,2004
# 
# Ceuta-2007      Ceuta-2008      Ceuta-2009      Ceuta-2010      Ceuta-2011 
# 91              53              73              65              43 
# Ceuta-2012 Madagascar-2010 Madagascar-2012 Madagascar-2013 Madagascar-2014 
# 28               3              10              56              55 
# Madagascar-2015       Maio-2008       Maio-2009       Maio-2010       Maio-2011 
# 147              15              34              57               9 
# Maio-2012       Maio-2013       Maio-2014       Maio-2015      Tuzla-1997 
# 66              92              93             107              79 
# Tuzla-1998      Tuzla-1999      Tuzla-2000      Tuzla-2004 
# 48             100              21              12 

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Breeding Schedule length------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#1. Data structure:--------
both<-both1
summary(both$corrected.bs.length.std)
ind<-which(is.na(both$corrected.bs.length.std))
both[ind,] #no NAs ,/
summary(both$bs.start.std)
both$corrected.bs.length.std.plus3<-both$corrected.bs.length.std + 3
both$bs.start.std.plus7 <- both$bs.start.std + 4
summary(both$bs.start.std.plus7) 

 #a) Histograms:---------------
hist(indiv$corrected.bs.length.std)
table(indiv$bs.length.std)

require(MASS)
library(car)
qqnorm(indiv$corrected.bs.length.std)
qqline(indiv$corrected.bs.length.std,lty=2)

#log norm dist?
qqp(both$corrected.bs.length.std.plus3, "lnorm") #needs newer version of R
#see: http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

#norm dist?
qqp(both$corrected.bs.length.std, "norm")
qqp(both$corrected.bs.length.std.plus3)
both[is.na(both$corrected.bs.length.std.plus3),]

#gamma? gamma seems to do the best fit
gamma <- fitdistr(both$corrected.bs.length.std.plus3, "gamma")
qqp(both$corrected.bs.length.std.plus3, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

#poisson?
poisson <- fitdistr(both$corrected.bs.length.std.plus3, "Poisson")
qqp(both$corrected.bs.length.std.plus3, "pois", poisson$estimate)

  #b) Normality----------
shapiro.test(indiv$corrected.bs.length.std)
ks.test(indiv$corrected.bs.length.std, pnorm)


#2. Explore relations:------------
attach(indiv)
names(indiv)
names(both)

pairs(~ bs.start.std + bs.end.std + bs.length+total.nests.peryear, data=both, panel=panel.smooth)

boxplot(bs.length.std~interaction(sex.available, ms),data=both,
        ylab="Std length of breeding schedule")


plot(corrected.bs.length.std~bs.start.std, data=both, cex=0.5)#, col=as.factor(pop.sp))
abline(lm(corrected.bs.length.std[pop.sp %in% "Maio-KP"]~bs.start.std[pop.sp %in% "Maio-KP"], data=both), col=26)
abline(lm(corrected.bs.length.std[pop.sp %in% "Ceuta-SP"]~bs.start.std[pop.sp %in% "Ceuta-SP"], data=both), col=27)
abline(lm(corrected.bs.length.std[pop.sp %in% "Tuzla-KP"]~bs.start.std[pop.sp %in% "Tuzla-KP"], data=both), col=1)
abline(lm(corrected.bs.length.std[pop.sp %in% "Madagascar-KiP"]~bs.start.std[pop.sp %in% "Madagascar-KiP"], data=both),col=4)
abline(lm(corrected.bs.length.std[pop.sp %in% "Madagascar-WfP"]~bs.start.std[pop.sp %in% "Madagascar-WfP"], data=both), col=29)
abline(lm(corrected.bs.length.std[pop.sp %in% "Madagascar-MP"]~bs.start.std[pop.sp %in% "Madagascar-MP"], data=both), col=30)

legend(-3.5,6.9,lty=c(1,1,1,1,1,1), col=c(26,27,1,4,29,30), c("Maio-KP","Ceuta-SP","Tuzla-KP","Madagascar-KiP","Madagascar-WfP","Madagascar-MP"), cex=0.5, bty="n")

plot(corrected.bs.length.std[pop.sp %in% "Madagascar-MP"]~bs.start.std[pop.sp %in% "Madagascar-MP"], data=both, cex=0.5)


boxplot(corrected.bs.length.std~interaction(sex.available, pop.sp),data=both,
        ylab="Std length of breeding schedule")

#3. Try Mixed models
#explore random data structure
#create id to solve NAs in ring:
both$id <- ifelse(is.na(both$ring), both$code, both$ring)
both$id <- as.factor(both$id)
both$year <- as.factor(both$year)
both$species<-as.factor(both$species)
both$ms <- as.factor(both$ms)
both$population<-as.factor(both$population)
both$sex.available<-as.factor(both$sex.available)


library(ggplot2)
ggplot(both[both$species %in% "KP",],aes(x=bs.start.std,y=corrected.bs.length.std.plus3))+geom_point(aes(colour=sex.available))+
  geom_line(aes(group=id),alpha=0.3)+scale_y_log10() #from http://stats.stackexchange.com/questions/64555/providing-starting-values-for-a-generalized-linear-mixed-model-with-glmmpql
   #explore repetitions per species, all seem to have several repeated breeding events

contrasts(both$population)

#-----------------------
library(lme4)
library(nlme)

#USING SEX.AVAILABLE
mm1<-lme(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex.available +pop.sp + pop.sp:sex.available+pop.sp:bs.start.std, random = ~1|id, method="ML", na.action=na.omit, data=both) #residual plot does not look good, needs change of error structure

mm.reml<-update(mm1, method="REML")


#model the random factors. Using stepwise deletions and REML-fitted models I found the optimal structure of the random component of the model: 
mm2<-update(mm.reml, random=~1|species/id)
anova(mm.reml,mm2)
mm4<-update(mm.reml, random=~1|population/id) #simpler version of random effects is better ~1|species/id
anova(mm.reml,mm4)
anova(mm2,mm4)
mm6<-update(mm.reml, random=~1|population/species/id)
anova(mm.reml,mm6)
anova(mm2,mm6)
mm5<-update(mm.reml, random=~1|pop.sp/id) #blend pop & sp in one factor
anova(mm5,mm.reml)
anova(mm5, mm2)
anova(mm5, mm4)#same as species/id, still random effects  ~1|species/id is better
AIC(mm.reml,mm2,mm4,mm6,mm5)
# df      AIC
# mm.reml 21 3315.201
# mm2     22 3315.217
# mm4     22 3315.138
# mm6     23 3317.138
# mm5     22 3315.217

#Model with random effects defined: (mm.r)
mm.r<-lme(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex.available +pop.sp + pop.sp:sex.available+pop.sp:bs.start.std, random = ~1|id, method="ML", na.action=na.omit, data=both)

summary(mm.r)
c<-coef(mm.r)
head(c)

#-------model validation (based on Thomas, Vaughan and Lello book): and Pinhero and Bates book for mixed models
#another way of testing this: #assumption: distribution of residuals is normal
m<-mm.r
par(mfrow=c(1,1))
sresid<-residuals(m)
plot(fitted(m), sresid, xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(m), residuals(m)), col="red") #Model-checking plots show that the residuals are well behaved if the red line is horizontal
rownames(both)<- 1:length(both$X)
text(fitted(m),residuals(m),labels=rownames(both)) #rows 1586, 3434 and 3157 are the problematic ones 
text(fitted(m),residuals(m),labels=both$id)

      #check problematic rows to see potential errors?
        both[c(1586,3434,3157),]

qqp(residuals(m))

#Test assumption of constant variance across groups:
#boxplot of residuals by group 
plot(m, id~resid(.), abline=0) #errors should be centered at 0 - constant variance across groups
plot(m, species~resid(.), abline=0)
plot(m, population~resid(.), abline=0)

plot(m, residuals.lme(., type="normalized") ~ fitted(.) | species, id=0.05, adj=-0.3)
plot(m, residuals.lme(., type="normalized") ~ fitted(.) | population, id=0.05, adj=-0.3)

qqnorm(m, ~resid(.)|population) #and the errors are reasonably close to normally distributed in all species
plot(m, corrected.bs.length.std~fitted(.)) #The response variable is a reasonably linear function of the fitted values

#plot residuals versus each x-variable
plot(sresid~both$bs.start.std)
plot(sresid~both$total.nests.peryear)
plot(sresid~both$ms)
plot(sresid~both$sex.available)
plot(sresid~both$year)
plot(sresid~both$pop.sp)

#--------------Model selection------
#selection of fixed terms has to be done using ML-fitted models and anova, test="F"
m<-mm.r
summary(m)
mm5<-update(m, .~.-pop.sp:sex.available, .) 
anova(m,mm5) #use threshold of p<0.001 (Thomas, Vaughan & Lello, page 98)
mm6<-update(mm5, .~. -bs.start.std:pop.sp,.)
anova(mm5,mm6)
summary(mm6)
mm7<-update(mm6, .~. -pop.sp, .)
anova(mm6,mm7)
summary(m)
mm8 <- update(mm6, .~. -sex.available,.)
anova(mm6,mm8)
mm9<- update(mm6, .~. - total.nests.peryear)
anova(mm6,mm9)
mm10<-update(mm6, .~. - bs.start.std,.)
anova(mm6,mm10)
summary(mm5)

#mm11<-update(m, .~. -year,.)
#anova(mm11, m)
#summary(mm11)

#final.model:
summary(mm6)

c<-coef(mm11)
head(c)
ci<-confint(mm11)
ci[[2]]


#-------model validation (based on Thomas, Vaughan and Lello book): and Pinhero and Bates book for mixed models
#another way of testing this: #assumption: distribution of residuals is normal
m<-mm5
par(mfrow=c(2,3), pty="s")
#m<-mm1
sresid<-residuals(m)
plot(fitted(m), sresid, xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(m), residuals(m)), col="red") #Model-checking plots show that the residuals are well behaved if the red line is horizontal
rownames(both)<- 1:length(both$X)
text(fitted(m),residuals(m),labels=rownames(both)) #rows 1586, 3434 and 3157 are the problematic ones 
text(fitted(m),residuals(m),labels=both$id)

#check problematic rows to see potential errors?
both[c(1586,3434,3157),]

qqp(residuals(m))

#Test assumption of constant variance across groups:
#boxplot of residuals by group 

plot(m, id~resid(.), abline=0, cex.lab=0.02,cex.names=0.02,cex.sub=0.2, cex=0.2) #errors should be centered at 0 - constant variance across groups
plot(m, pop.sp~resid(.), abline=0)
plot(m, population~resid(.), abline=0)

plot(m, residuals.lme(., type="normalized") ~ fitted(.) | species, id=0.05, adj=-0.3)
plot(m, residuals.lme(., type="normalized") ~ fitted(.) | population, id=0.05, adj=-0.3)

qqnorm(m, ~resid(.)|pop.sp) #and the errors are reasonably close to normally distributed in all species
plot(m, corrected.bs.length.std~fitted(.)) #The response variable is a reasonably linear function of the fitted values

#plot residuals versus each x-variable
plot(sresid~both$bs.start.std[which(!is.na(both$bs.start.std))])
length(sresid)
length(which(!is.na(both$bs.start.std)))
plot(sresid~both$total.nests.peryear[which(!is.na(both$total.nests.peryear))])
plot(sresid~both$pop.sp[which(!is.na(both$pop.sp))])
plot(sresid~both$sex.available[which(!is.na(both$sex.available))])
length(which(!is.na(both$ms)))

both.sex.av<-both

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Run models with mol sex only and without conflicting sex individuals----------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#USE molecular sex next 05/05/2016 and omit individuals with conflicting sex
both2<-both.mol[both.mol$year > both.mol$year.cr & !both.mol$sex.reliable %in% "conflicting sex",]
length(which(!is.na(both.mol$year))) #4215
length(which(!is.na(both2$year))) #1265

unique(both2$pop.year)
table(both2$pop.year)
#  Years used:
#    Ceuta 2007-2012
#  Madagascar 2012-2015, 2010
#  Maio 2008-2015
#  Tuzla 1997-2000,2004
# Ceuta-2007      Ceuta-2008      Ceuta-2009      Ceuta-2010      Ceuta-2011 
# 71              48              69              64              43 
# Ceuta-2012 Madagascar-2010 Madagascar-2012 Madagascar-2013 Madagascar-2014 
# 28               2               9              47              43 
# Madagascar-2015       Maio-2008       Maio-2009       Maio-2010       Maio-2011 
# 126              14              34              54               9 
# Maio-2012       Maio-2013       Maio-2014       Maio-2015      Tuzla-1997 
# 64              88              89             103              79 
# Tuzla-1998      Tuzla-1999      Tuzla-2000      Tuzla-2004 
# 48             100              21              12 

both<-both2
both$id <- ifelse(is.na(both$ring), both$code, both$ring)
both$id <- as.factor(both$id)
both$year <- as.factor(both$year)
both$species<-as.factor(both$species)
both$ms <- as.factor(both$ms)
both$population<-as.factor(both$population)
both$sex<-as.factor(both$sex)
both$pop.sp<-as.factor(both$pop.sp)

library(lme4)
library(nlme)

#USING SEX.MOL (sex)
mm1<-lme(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex.available +pop.sp + pop.sp:sex.available+pop.sp:bs.start.std, random = ~1|id, method="ML", na.action=na.omit, data=both) #residual plot does not look good, needs change of error structure

mm.reml<-update(mm1, method="REML")


#model the random factors. Using stepwise deletions and REML-fitted models I found the optimal structure of the random component of the model: 
mm2<-update(mm.reml, random=~1|species/id)
anova(mm.reml,mm2)
mm4<-update(mm.reml, random=~1|population/id) #simpler version of random effects is better ~1|species/id
anova(mm.reml,mm4)
anova(mm2,mm4)
mm6<-update(mm.reml, random=~1|population/species/id)
anova(mm.reml,mm6)
anova(mm2,mm6)
mm5<-update(mm.reml, random=~1|pop.sp/id) #blend pop & sp in one factor
anova(mm5,mm.reml)
anova(mm5, mm2)
anova(mm5, mm4)#same as species/id, still random effects  ~1|species/id is better
AIC(mm.reml,mm2,mm4,mm6,mm5)


#Model with random effects defined: (mm.r)
mm.r<-lme(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex +pop.sp + pop.sp:sex+pop.sp:bs.start.std, random = ~1|id, method="ML", na.action=na.omit, data=both)

summary(mm.r)
c<-coef(mm.r)
head(c)

#-------model validation (based on Thomas, Vaughan and Lello book): and Pinhero and Bates book for mixed models
#another way of testing this: #assumption: distribution of residuals is normal
m<-mm.r
m<-mm7
par(mfrow=c(2,3))
sresid<-residuals(m)
plot(fitted(m), sresid, xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(m), residuals(m)), col="red") #Model-checking plots show that the residuals are well behaved if the red line is horizontal
rownames(both)<- 1:length(both$X)
text(fitted(m),residuals(m),labels=rownames(both)) #rows 1586, 3434 and 3157 are the problematic ones 
text(fitted(m),residuals(m),labels=both$id)


qqp(residuals(m))

#Test assumption of constant variance across groups:
#boxplot of residuals by group 
plot(m, id~resid(.), abline=0) #errors should be centered at 0 - constant variance across groups
plot(m, species~resid(.), abline=0)
plot(m, pop.sp~resid(.), abline=0)

plot(m, residuals.lme(., type="normalized") ~ fitted(.) | species, id=0.05, adj=-0.3)
plot(m, residuals.lme(., type="normalized") ~ fitted(.) | population, id=0.05, adj=-0.3)

qqnorm(m, ~resid(.)|population) #and the errors are reasonably close to normally distributed in all species
plot(m, corrected.bs.length.std~fitted(.)) #The response variable is a reasonably linear function of the fitted values

#plot residuals versus each x-variable
plot(sresid~both$bs.start.std[!is.na(both$bs.start.std)])
plot(sresid~both$total.nests.peryear[!is.na(both$total.nests.peryear)])
plot(sresid~both$pop.sp[!is.na(both$pop.sp)])
plot(sresid~both$sex[!is.na(both$sex)])
plot(sresid~both$year)

#--------------Model selection------
#selection of fixed terms has to be done using ML-fitted models and anova, test="F"
m<-mm.r
summary(m)
mm5<-update(m, .~. -sex:pop.sp, .) 
anova(m,mm5) #use threshold of p<0.001 (Thomas, Vaughan & Lello, page 98)
mm6<-update(mm5, .~. -bs.start.std:pop.sp,.)
anova(mm5,mm6)
summary(mm6)
mm7<-update(mm6, .~. -pop.sp, .)
anova(mm6,mm7)
summary(m)
mm8 <- update(mm6, .~. -sex,.)
anova(mm6,mm8)
mm9<- update(mm6, .~. - total.nests.peryear)
anova(mm6,mm9)
mm10<-update(mm6, .~. - bs.start.std,.)
anova(mm6,mm10)
summary(mm5)

#mm11<-update(m, .~. -year,.)
#anova(mm11, m)
#summary(mm11)

#final.model:
summary(mm7)

c<-coef(mm11)
head(c)
ci<-confint(mm11)
ci[[2]]


#-------model validation (based on Thomas, Vaughan and Lello book): and Pinhero and Bates book for mixed models
#another way of testing this: #assumption: distribution of residuals is normal

#Checked plots and look the same 
