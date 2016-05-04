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

#_-------------------------------------------------------------------------------------
# Breeding Schedule length
#1. Data structure:--------
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

summary(both$corrected.bs.length.std)
summary(both$bs.start.std)
both$corrected.bs.length.std.plus3<-both$corrected.bs.length.std + 3.3518
both$bs.start.std.plus7 <- both$bs.start.std + 7.071000
summary(both$bs.start.std.plus7)

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
indiv$year<- as.factor(indiv$year)
indiv$pop.sp<-as.factor(indiv$pop.sp)
indiv$ms<-as.factor(indiv$ms)

# 
# males.mol<-indiv[grep(indiv$pop.sp.sex, pattern="M$"),]
# females.mol<-indiv[grep(indiv$pop.sp.sex, pattern="F$"),]
# both<-rbind(males.mol, females.mol)
# 
# both$sex <- as.factor(both$sex)


pairs(~ bs.start.std + bs.end.std + bs.length+total.nests.peryear, data=females.mol, panel=panel.smooth)
pairs(~ bs.start.std + bs.end.std + bs.length+total.nests.peryear, data=males.mol, panel=panel.smooth)

lm.1<-lm(corrected.bs.length.std~pop.sp + bs.start.std + total.nests.peryear + sex+  ms + ms:sex+pop.sp:sex, data=both)
summary(lm.1)


lm.m<-lm(corrected.bs.length.std~ms+pop.sp + bs.start.std + total.nests.peryear+year, data=males.mol)
summary(lm.m)
plot(lm.m)


library(HH)
vif(lm.1)

lm.m1<-update(lm.m, .~.-year,.)
anova(lm.m1, lm.m)
lm.m2<-update(lm.m1,.~.-pop.sp,.)
anova(lm.m1,lm.m2)
summary(lm.m1)
lm.m3<-update(lm.m1, .~.-bs.start.std,.)
anova(lm.m1, lm.m3)
lm.m4<-update(lm.m1, .~.-total.nests.peryear,.)
anova(lm.m1,lm.m4)

print(levels(males.mol$year))

#This models run into errors "Coefficients: (2 not defined because of singularities)"


#3. Try Mixed models
#explore random data structure
#create id to solve NAs in ring:
both$id <- ifelse(is.na(both$ring), both$code, both$ring)
both$id <- as.factor(both$id)
both$year <- as.factor(both$year)
both$species<-as.factor(both$species)
both$population<-as.factor(both$population)

library(ggplot2)
ggplot(both[both$species %in% "KiP",],aes(x=bs.start.std,y=corrected.bs.length.std.plus3))+geom_point(aes(colour=sex))+
  geom_line(aes(group=id),alpha=0.3)+scale_y_log10() #from http://stats.stackexchange.com/questions/64555/providing-starting-values-for-a-generalized-linear-mixed-model-with-glmmpql
   #explore repetitions per species, all seem to have several repeated breeding events

contrasts(both$population)

#-----------------------
library(lme4)
library(nlme)


mm1<-lme(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex+  ms + ms:sex + year, random = ~1|population/species/id, method="ML", na.action=na.omit, data=both) #residual plot does not look good, needs change of error structure
mm.noyear<-lme(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex+  ms + ms:sex, random = ~1|population/species/id, method="ML", na.action=na.omit, data=both) #residual plot does not look good, needs change of error structure
mm.year.rdm<-lmer(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex+  ms + ms:sex + (1|population/species/id) + (1|year), REML=F, na.action=na.omit, data=both) #residual plot looks better with this new random error structure, needs change of error structure

#try different error structures
mm2<-lmer(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex+  ms + ms:sex + (1|species/population) +(1|id) + (1|year), REML=F, na.action=na.omit, data=both) #residual plot looks better with this new random error structure, needs change of error structure
mm3<-lmer(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex+  ms + ms:sex + (1|species/population/id) + (1|year), REML=F, na.action=na.omit, data=both) #residual plot looks better with this new random error structure, needs change of error structure
mm4<-lmer(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex+  ms + ms:sex + year + (1|species/population/id) , REML=F, na.action=na.omit, data=both) #residual plot looks better with this new random error structure, needs change of error structure
mm5<-lmer(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex+  ms + ms:sex + year + (1|species/population) + (1|id) , REML=F, na.action=na.omit, data=both) #residual plot looks better with this new random error structure, needs change of error structure


summary(mm1)
c<-coef(mm3)
plot(augPred(mm1), aspect="xy", grid=T) 
write(c, "coef.model.csv")

#with glmer
mm1<-glmer(corrected.bs.length.std.plus3 ~  sex+  ms + ms:sex + (1|population/species/id), family=Gamma(link = "inverse"),data=both) #doesn't work if family is gaussian
          #try with: (but data needs conversion to not have negative data nor zeroes)
              #family=Gamma
              #family=gaussian(link="log")
summary(mm1)

#try gamma : gamma models will not work with all variables nor the correct random error structure
mmg<-glmmPQL(corrected.bs.length.std.plus3 ~  sex + bs.start.std.plus7 + ms + ms:sex  + year, ~1|population/species/id, family=Gamma,data=both, na.action=na.omit) #it won't work if bs.start.std and total.nests.peryear are included
mmg20<-glmer(corrected.bs.length.std.plus3 ~  sex + ms + ms:sex + (1|population/species) + (1|id), family=Gamma, data=both, na.action=na.omit) #it won't work if bs.start.std and total.nests.peryear are included

min(both$bs.start.std.plus7)
#summary(both$corrected.bs.length.std.plus3)
summary(mmg)

 #model doesn't work including ring in random terms, explore repeated rings: 
#including na.action=na.omit solves the problem....but there shouldn't be any NAs...ring has NAs for the ones with only code...change
str(both$ring) #4215
unique(both$ring) #2885
both[is.na(both$ring),] #there are NAs, this is the problem...create ID with ring or code

summary(mmlog)



#-------model validation (based on Thomas, Vaughan and Lello book): and Pinhero and Bates book for mixed models
sresid<-resid(mmg)#, type="normalized")
hist(sresid) #seems normal except for outliers at the end
f<-fitted(mmg)
plot(sresid~f) #it's not random...
plot(mmg) #lme(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex+  ms + ms:sex + year, random=~1|population/species/year/id, method="ML", na.action=na.omit,data=both) does not look good....error needs to be changed

plot(mm1)
plot(mmlog) #mmlog<-glmer(corrected.bs.length.std.plus3 ~ sex+  ms + ms:sex + year+ (1|population/species), family=Gamma,data=both, na.action=na.omit) had to delete varibles so model would run....errors look better but not great

#another way of testing this: #assumption: distribution of residuals is normal
m<-mmg
par(mfrow=c(1,1))
m<-mm1
plot(fitted(m), residuals(m), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(m), residuals(m)), col="red") #according to this line the model: mmlog<-glmer(corrected.bs.length.std.plus3 ~ sex+  ms + ms:sex + year+ (1|population/species/year), family=Gamma,data=both, na.action=na.omit) fits the data alright
rownames(both)<- 1:length(both$X)
text(fitted(m),residuals(m),labels=rownames(both)) #rows 1586, 3484 and 3157 are the problematic ones 
text(fitted(m),residuals(m),labels=both$id)

      #check problematic rows to see potential errors?
        both[1586,]

qqp(residuals(m))

#Test assumption of constant variance across groups:
#boxplot of residuals by group 
plot(m, id~resid(.), abline=0) #errors should be centered at 0 - constant variance across groups
plot(m, species~resid(.), abline=0)
plot(m, population~resid(.), abline=0)

plot(m, residuals.lme(., type="normalized") ~ fitted(.) | species, id=0.05, adj=-0.3)
plot(m, residuals.lme(., type="normalized") ~ fitted(.) | population, id=0.05, adj=-0.3)


#plot residuals versus each x-variable
plot(sresid~both$bs.start.std)
plot(sresid~both$total.nests.peryear)
plot(sresid~both$ms)
plot(sresid~both$sex)
plot(sresid~both$year)

#--------------Model selection------
m<-mg
mm2<-update(m, .~. -ms:sex, .)
anova(m,mm2, test="F")
mm3<-update(mm2, .~. -sex,.)
anova(mm2,mm3, test="F")
mm4<-update(mm2, .~. -ms, .)
anova(mm2,mm4, test="F")
summary(m)
