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

#8th log - 06/05/2016 Add ASR estimates Luke sent
#9th log - 09/05/2016 Created plots to explore differences
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
both<-both1[which(!is.na(both1$year)),]
summary(both$corrected.bs.length.std)
ind<-which(is.na(both$corrected.bs.length.std))
both[ind,] #no NAs ,/
summary(both$bs.start.std)
both$corrected.bs.length.std.plus3<-both$corrected.bs.length.std + 3
both$bs.start.std.plus7 <- both$bs.start.std + 4
summary(both$bs.start.std.plus7) 

 #a) Histograms:---------------
hist(both$corrected.bs.length.std)
table(both$bs.length.std)

require(MASS)
library(car)
qqnorm(both$corrected.bs.length.std)
qqline(both$corrected.bs.length.std,lty=2)

#log norm dist?
qqp(both$corrected.bs.length.std.plus3, "lnorm") #needs newer version of R
#see: http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

#norm dist?
qqp(both$corrected.bs.length.std, "norm")
qqp(both$corrected.bs.length.std.plus3)
both[is.na(both$corrected.bs.length.std.plus3),]

#outliers or mistaken data?
plot(both$corrected.bs.length.std, type="n")
rownames(both)<- 1:length(both$X)
text(both$corrected.bs.length.std,labels=rownames(both)) #236, 304 882

both[c(236,304,882),] #one has conflicting sex...the other two had no brood fate data...could delete them
both.no.outliers<-both[-c(236,304,882),]
plot(both.no.outliers$corrected.bs.length.std)
qqp(both.no.outliers$corrected.bs.length.std, "norm")

leverage<-function(x){1/length(x)+(x-mean(x))^2/sum((x-mean(x))^2)}
a<-leverage(both$corrected.bs.length.std)
summary(a)

plot(leverage(both.no.outliers$corrected.bs.length.std),type="h")
abline(0.0299,0,lty=2)
points(leverage(both.no.outliers$corrected.bs.length.std))
text(leverage(both.no.outliers$corrected.bs.length.std[a>0.008]),labels=rownames(both[a>0.008,]),
     cex = 0.5)

#gamma? gamma seems to do the best fit
gamma <- fitdistr(both$corrected.bs.length.std.plus3, "gamma")
qqp(both$corrected.bs.length.std.plus3, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

#poisson?
poisson <- fitdistr(both$corrected.bs.length.std.plus3, "Poisson")
qqp(both$corrected.bs.length.std.plus3, "pois", poisson$estimate)

  #b) Normality----------
shapiro.test(both$corrected.bs.length.std)
ks.test(both$corrected.bs.length.std, pnorm)


#2. Explore relations:------------
attach(indiv)
names(indiv)

pairs(~ bs.start.std + bs.end.std + bs.length+total.nests.peryear, data=both, panel=panel.smooth)

boxplot(bs.length.std~interaction(sex.available, ms),data=both,
        ylab="Std length of breeding schedule")

plot(bs.length.std~asr, data=both)
loess_fit<-loess(bs.length.std~asr, data=both)
lines(both$asr[which(!is.na(both$asr))], predict(loess_fit), col="red")

boxplot(bs.length.std~interaction(sex.available,asr), data=both)

interaction.plot(both$asr, both$sex.available, both$bs.length.std, type="b")
interaction.plot(both$asr, both$ms, both$bs.length.std, type="b")
interaction.plot(both$asr, paste(both$ms, both$sex.available), both$bs.length.std, type="b")

#plot Confidence intervals for means per population:
both$asr.sex.ms<-paste(both$asr, both$sex.available,both$ms, sep="-")
mean.bs.length<-aggregate(both$corrected.bs.length.std, by=list(both$asr.sex.ms), mean)
n<-aggregate(both$corrected.bs.length.std, by=list(both$asr.sex.ms), length)
sd<-aggregate(both$corrected.bs.length.std, by=list(both$asr.sex.ms), sd)

left.ci<-mean.bs.length$x-((qnorm(0.975)*sd$x)/(sqrt(n$x)))
right.ci<-mean.bs.length$x+((qnorm(0.975)*sd$x)/(sqrt(n$x)))

means<-cbind(mean.bs.length, n, sd, left.ci,right.ci)
means<-means[,c(1,2,4,6,7,8)]
colnames(means)<-c("group","mean","n","sd","lower.ci.length","upper.ci.length")
means$asr<-c(0.386,0.386,0.421,0.421,0.429,0.429,0.469,0.469,0.585,0.585,0.608,0.608)
means$asr.lcl<-c(0.288,0.288,0.3384,0.3384,0.396,0.396,0.169,0.169,0.509, 0.509, 0.501,0.501)
means$asr.ucl<-c(0.497,0.497,0.521,0.521,0.543,0.543,0.680,0.680,0.652,0.652,0.711,0.711)

xy.error.bars<-function (x,y,xbar.u, xbar.l, ybar.u, ybar.l){
  plot(x, y, pch=16, ylim=c(min(ybar.l),max(ybar.u)),
       xlim=c(min(xbar.l),max(xbar.u)))
  arrows(x, ybar.l, x, ybar.u, code=3, angle=90, length=0.1)
  arrows(xbar.l, y, xbar.u, y, code=3, angle=90, length=0.1) }

xy.error.bars(means$asr, means$mean, means$asr.ucl, means$asr.lcl, means$upper.ci.length,means$lower.ci.length)

plot(means$asr, means$mean, pch=16, 
     ylim=c(min(means$lower.ci.length),max(means$upper.ci.length))#,
     #xlim=c(min(xbar.l),max(xbar.u)))
)
arrows(means$asr, means$lower.ci.length, means$asr, means$upper.ci.length, code=3, angle=90, length=0.1)

#try kruskal.test for all groups...
both$asr.sex.ms<-as.factor(both$asr.sex.ms)
leveneTest(both$corrected.bs.length.std~ both$asr.sex.ms)
kruskal.test(both$corrected.bs.length.std~ both$asr.sex.ms)


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
#library(lme4)
library(nlme)

#USING SEX.AVAILABLE
mm1<-lme(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex.available+  asr:bs.start.std+ asr + asr:sex.available +asr:total.nests.peryear + year, random = ~1|id, method="ML", na.action=na.omit, data=both) #residual plot does not look good, needs change of error structure
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
mm.r<-lme(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex.available + ms:sex.available +z.(asr)+ms + z.(asr):sex.available + z.(asr):ms + year, random = ~1|id, method="ML", na.action=na.omit, data=both)

summary(mm.r)
c<-coef(mm.r)
head(c)
vif.mer(mm.r) #see https://hlplab.wordpress.com/2011/02/24/diagnosing-collinearity-in-lme4/

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


qqp(residuals(m))

#Test assumption of constant variance across groups:
#boxplot of residuals by group 
plot(m, id~resid(.), abline=0) #errors should be centered at 0 - constant variance across groups
plot(m, species~resid(.), abline=0)
plot(m, population~resid(.), abline=0)

plot(m, residuals.lme(., type="normalized") ~ fitted(.) | species, id=0.05, adj=-0.3)
plot(m, residuals.lme(., type="normalized") ~ fitted(.) | population, id=0.05, adj=-0.3)

qqnorm(m, ~resid(.)|population) #and the errors are reasonably close to normally distributed in all species
qqnorm(m, ~resid(.)|species)
plot(m, corrected.bs.length.std~fitted(.)) #The response variable is a reasonably linear function of the fitted values

#plot residuals versus each x-variable
plot(sresid~both$bs.start.std)
plot(sresid~both$total.nests.peryear)
plot(sresid~both$ms)
plot(sresid~both$sex.available)
plot(sresid~both$year)

#--------------Model selection------
#selection of fixed terms has to be done using ML-fitted models and anova, test="F"
m<-mm.r
summary(m)
mm5<-update(m, .~. -ms:sex.available, .) 
anova(m,mm5) #use threshold of p<0.001 (Thomas, Vaughan & Lello, page 98)
mm6<-update(mm5, .~. -ms:asr,.)
anova(mm5,mm6)
summary(mm6)
mm7<-update(mm6, .~. -sex.available:asr, .)
anova(mm6,mm7)
summary(mm7)
mm8 <- update(mm7, .~. -year,.)
anova(mm7,mm8)
summary(mm8)
mm9<- update(mm8, .~. - asr)
anova(mm8,mm9)
mm10<-update(mm9, .~. - sex.available,.)
anova(mm9,mm10)
summary(mm9)
mm11<-update(mm9,.~. -ms,.)
anova(mm9,mm11)
mm12<-update(mm9,.~.-total.nests.peryear,.)
anova(mm9,mm12)
mm14<-update(mm9,.~.-bs.start.std,.)
anova(mm9,mm14)
#mm11<-update(m, .~. -year,.)
#anova(mm11, m)
#summary(mm11)

#final.model:
summary(mm9)
mfinal<-update(mm9, .~.+ms:asr+asr,.)
summary(mfinal)

vif.mer(mfinal) #from: https://hlplab.wordpress.com/2011/02/24/diagnosing-collinearity-in-lme4/
# bs.start.std total.nests.peryear      sex.availableM 
# 1.094663            1.072572            1.009095 
# msPol                 asr           msPol:asr 
# 350.661784           41.191430          538.136008 
#High collinearity between ms and asr

c<-coef(mm11)
head(c)
ci<-confint(mm11)
ci[[2]]


#-------model validation (based on Thomas, Vaughan and Lello book): and Pinhero and Bates book for mixed models
#another way of testing this: #assumption: distribution of residuals is normal
m<-mm6
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
plot(sresid~both$bs.start.std[which(!is.na(both$bs.start.std))])
length(sresid)
length(which(!is.na(both$bs.start.std)))
plot(sresid~both$total.nests.peryear[which(!is.na(both$total.nests.peryear))])
plot(sresid~both$ms[which(!is.na(both$ms))])
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

library(lme4)
library(nlme)

#USING SEX.MOL (sex)
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


#Model with random effects defined: (mm.r)
mm.r<-lme(corrected.bs.length.std ~ bs.start.std + total.nests.peryear + sex +ms + ms:sex+ year, random = ~1|id, method="ML", na.action=na.omit, data=both)

summary(mm.r)
c<-coef(mm.r)
head(c)

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
plot(m, id~resid(.), abline=0) #errors should be centered at 0 - constant variance across groups
plot(m, species~resid(.), abline=0)
plot(m, population~resid(.), abline=0)

plot(m, residuals.lme(., type="normalized") ~ fitted(.) | species, id=0.05, adj=-0.3)
plot(m, residuals.lme(., type="normalized") ~ fitted(.) | population, id=0.05, adj=-0.3)

qqnorm(m, ~resid(.)|population) #and the errors are reasonably close to normally distributed in all species
plot(m, corrected.bs.length.std~fitted(.)) #The response variable is a reasonably linear function of the fitted values

#plot residuals versus each x-variable
plot(sresid~both$bs.start.std[!is.na(both$bs.start.std)])
plot(sresid~both$total.nests.peryear[!is.na(both$total.nests.peryear)])
plot(sresid~both$ms[!is.na(both$ms)])
plot(sresid~both$sex.available[!is.na(both$sex.available)])
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
mm8 <- update(mm6, .~. -sex,.)
anova(mm6,mm8)
mm9<- update(mm6, .~. - total.nests.peryear)
anova(mm6,mm9)
mm10<-update(mm6, .~. - bs.start.std,.)
anova(mm6,mm10)
summary(mm7)

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
