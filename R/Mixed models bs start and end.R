#Analysing breeding schedules (bs) data (start and end) from Ceuta, Maio, Tuzla and Mad
#05/05/2016
#by Cristina Carmona-Isunza

#1st log - 05/05/2016 Create code, based on length of breedins schedules
#2nd log - 10/05/2016 Start

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
x<-both$bs.start.std.plus7^(1/2)
qqp(x, "norm") #looks better than untransformed version
boxplot(both$bs.start.std)

qqp(both$bs.end.std[-c(330,249,269,942)], "norm")
x<-both$bs.end.std.plus5[-c(330,249,269,942)]^(2) #this looks almost perfect
qqp(x, "norm")

boxplot(both$bs.end.std)
Boxplot(both$bs.end.std, id.method="identify" )
rownames(both)<- 1:length(both$X)
#Outliers: 249 269 330
both[330,] #end_date is 6 months after end_date....clearly a wrong data point....delete
both[249,] #brood fates in november?? when brood hatched in february
both[269,] #this is an out of season brood...hatched in november
both[942,] #this is an out of season brood....october
both$bs.end.std[c(330,249,269,942)]
both[both$bs.end.std > 3,]
Boxplot(both$bs.end.std[-c(330,249,269,942)], id.method="identify")

#b) Normality----------
shapiro.test(both$bs.start.std)
ks.test(both$bs.start.std, pnorm)

shapiro.test(x)
ks.test(x, pnorm)

#2. Explore relations:------------
pairs(~ bs.start.std + bs.end.std + total.nests.peryear, data=both, panel=panel.smooth)

boxplot(bs.start.std~pop.sp, data=both)

boxplot(bs.start.std~interaction(sex.available,pop.sp), data=both)

boxplot(bs.end.std~interaction(sex.available, pop.sp), data=both)

#--------------------------------------------------
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
both$pop.sp<-as.factor(both$pop.sp)

library(ggplot2)
ggplot(both[both$species %in% "KP",],aes(x=bs.start.std,y=corrected.bs.length.std.plus3))+geom_point(aes(colour=sex.available))+
  geom_line(aes(group=id),alpha=0.3)+scale_y_log10() #from http://stats.stackexchange.com/questions/64555/providing-starting-values-for-a-generalized-linear-mixed-model-with-glmmpql
#explore repetitions per species, all seem to have several repeated breeding events

contrasts(both$population)

#-----------------------
#library(lme4)
library(nlme)

#USING SEX.AVAILABLE
mm1<-lme(bs.start.std ~ total.nests.peryear + sex.available + pop.sp+ pop.sp:sex.available + pop.sp:total.nests.peryear, random = ~1|id, method="ML", na.action=na.omit, data=both) #residual plot does not look good, needs change of error structure
summary(mm1)

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
start.sq<-both$bs.start.std.plus7^(1/2)
mm.r<-lme(start.sq~ total.nests.peryear + sex.available + pop.sp+ pop.sp:sex.available + pop.sp:total.nests.peryear, random = ~1|id, method="ML", na.action=na.omit, data=both)

summary(mm.r)
c<-coef(mm.r)
head(c)
vif.mer(mm.r) #see https://hlplab.wordpress.com/2011/02/24/diagnosing-collinearity-in-lme4/

#-------model validation (based on Thomas, Vaughan and Lello book): and Pinhero and Bates book for mixed models
#another way of testing this: #assumption: distribution of residuals is normal
m<-mm.r
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
dev.off()
plot(m, id~resid(.), abline=0, cex=0.5, cex.lab=0.2) #errors should be centered at 0 - constant variance across groups
plot(m, species~resid(.), abline=0)
plot(m, population~resid(.), abline=0)

plot(m, residuals.lme(., type="normalized") ~ fitted(.) | species, id=0.05, adj=-0.3)
plot(m, residuals.lme(., type="normalized") ~ fitted(.) | population, id=0.05, adj=-0.3)

qqnorm(m, ~resid(.)|population) #and the errors are reasonably close to normally distributed in all species
qqnorm(m, ~resid(.)|species)
plot(m, bs.start.std~fitted(.)) #The response variable is a reasonably linear function of the fitted values


#plot residuals versus each x-variable
plot(sresid~both$bs.start.std)
plot(sresid~both$total.nests.peryear)
plot(sresid~both$ms)
plot(sresid~both$sex.available[!is.na(both$sex.available)])


#--------------Model selection------
#selection of fixed terms has to be done using ML-fitted models and anova, test="F"
m<-mm.r
summary(m)
mm5<-update(m, .~. -pop.sp:sex.available, .) 
anova(m,mm5) #use threshold of p<0.001 (Thomas, Vaughan & Lello, page 98)
mm6<-update(mm5, .~. -total.nests.peryear:pop.sp,.)
anova(mm5,mm6)
summary(mm6)
mm7<-update(mm6, .~. -pop.sp, .)
anova(mm6,mm7)
summary(mm7)
mm8 <- update(mm6, .~. -sex.available,.)
anova(mm6,mm8)
summary(mm8)
mm9<- update(mm6, .~. - total.nests.peryear,.)
anova(mm6,mm9)

#final.model:
summary(mm6)


vif.mer(mfinal)

