#Analysing breeding schedules (bs) data from Ceuta, Maio, Tuzla and Mad
#20/04/2016
#by Cristina Carmona-Isunza

#1st log -20/04/2016 Create script
#2nd log - 21/04/2016 Explore relations
#3rd log - 22/04/2016 Explore problems in models as some coef are not calculated because of singularities
#4th log - 26/04/2016 Establish best model: gaussian (after exploring gamma)
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

indiv$pop.sp.sex<-ifelse(indiv$population %in% "Tuzla", 
                               paste(indiv$population, indiv$species, indiv$field_sex_focal, sep="-"),
                               paste(indiv$population, indiv$species, indiv$mol_sex_focal, sep="-"))
indiv$sex<-ifelse(indiv$population %in% "Tuzla", indiv$field_sex_focal, indiv$mol_sex_focal)
indiv$pop.sp.field_sex<-paste(indiv$population, indiv$species, indiv$field_sex_focal, sep="-")

indiv$pop.sp.year<-paste(indiv$pop.sp, indiv$year, sep="-")

#Add breeding season length to each year
unique(indiv$pop.sp.year)

indiv$season.length <- season$breedingseasonlength[match(indiv$pop.sp.year, season$pop.sp.year)]

#If an individual stayed longer than the breeding season length, then change its length to the breeding season lenght

indiv[indiv$bs.length > indiv$season.length,]
indiv$corrected.bs.length <- ifelse(indiv$bs.length > indiv$season.length, indiv$season.length, indiv$bs.length)

indiv<-ddply(indiv, c("pop.year"), transform, corrected.bs.length.std=scale(corrected.bs.length))

#Monogamous and polygamous pops
indiv$ms <- ifelse(indiv$pop.sp %in% c("Ceuta-SP", "Tuzla-KP", "Madagascar-KiP"), "Pol", "Mon")

males.mol<-indiv[grep(indiv$pop.sp.sex, pattern="M$"),]
females.mol<-indiv[grep(indiv$pop.sp.sex, pattern="F$"),]

both<-rbind(males.mol, females.mol)

both$sex <- as.factor(both$sex)

#restrict to individuals ringed one year before focal year:
names(both)
length(both$year) #4215

both.original<-both

both1<-both[both$year != both$year.cr,]
length(both1$year) #1371

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Breeding Schedule length------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#1. Data structure:--------
both<-both1
summary(both$corrected.bs.length.std)
summary(both$bs.start.std)
both$corrected.bs.length.std.plus3<-both$corrected.bs.length.std + 3.3518
both$bs.start.std.plus7 <- both$bs.start.std + 7.071000
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

pairs(~ bs.start.std + bs.end.std + bs.length+total.nests.peryear, data=both, panel=panel.smooth)


#3. Try Mixed models
#explore random data structure
#create id to solve NAs in ring:
both$id <- ifelse(is.na(both$ring), both$code, both$ring)
both$id <- as.factor(both$id)
both$year <- as.factor(both$year)
both$species<-as.factor(both$species)
both$ms <- as.factor(both$ms)
both$population<-as.factor(both$population)

library(ggplot2)
ggplot(both[both$species %in% "KP",],aes(x=bs.start.std,y=corrected.bs.length.std.plus3))+geom_point(aes(colour=sex))+
  geom_line(aes(group=id),alpha=0.3)+scale_y_log10() #from http://stats.stackexchange.com/questions/64555/providing-starting-values-for-a-generalized-linear-mixed-model-with-glmmpql
   #explore repetitions per species, all seem to have several repeated breeding events

contrasts(both$population)

#-----------------------
library(lme4)
library(nlme)


mm1<-lme(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex+  ms + ms:sex + year, random = ~1|id, method="ML", na.action=na.omit, data=both) #residual plot does not look good, needs change of error structure

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
# mm.reml 23 3167.611
# mm2     24 3168.178
# mm4     24 3167.979
# mm6     25 3169.979
# mm5     24 3168.178

#Model with random effects defined: (mm.r)
mm.r<-lme(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex+  ms + ms:sex + year, random = ~1|id, method="ML", na.action=na.omit, data=both)

summary(mm.r)
c<-coef(mm.r)
head(c)

#with glmer
# mm1<-glmer(corrected.bs.length.std.plus3 ~  sex+  ms + ms:sex + (1|population/species/id), family=Gamma(link = "inverse"),data=both) #doesn't work if family is gaussian
#           #try with: (but data needs conversion to not have negative data nor zeroes)
#               #family=Gamma
#               #family=gaussian(link="log")
# summary(mm1)


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
plot(sresid~both$sex)
plot(sresid~both$year)

#--------------Model selection------
#selection of fixed terms has to be done using ML-fitted models and anova, test="F"
m<-mm.r
summary(m)
mm5<-update(m, .~. -ms:sex, .) 
anova(m,mm5) #use threshold of p<0.001 (Thomas, Vaughan & Lello, page 98)
mm6<-update(mm5, .~. -year,.)
anova(mm5,mm6)
summary(mm6)
mm7<-update(mm6, .~. -ms, .)
anova(mm6,mm7)
summary(m)
mm8 <- update(mm7, .~. -sex,.)
anova(mm7,mm8)
mm9<- update(mm7, .~. - total.nests.peryear)
anova(mm7,mm9)
mm10<-update(mm7, .~. - bs.start.std,.)
anova(mm7,mm10)
summary(mm7)

mm11<-update(m, .~. -year,.)
anova(mm11, m)
summary(mm11)

c<-coef(mm11)
head(c)
ci<-confint(mm11)
ci[[2]]


#-------model validation (based on Thomas, Vaughan and Lello book): and Pinhero and Bates book for mixed models
#another way of testing this: #assumption: distribution of residuals is normal
m<-mm11
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
plot(sresid~both$sex)



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Start of Breeding schedule-----------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
names(both)






