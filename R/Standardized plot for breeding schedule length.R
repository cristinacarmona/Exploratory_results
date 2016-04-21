#Preliminary plot with standardized values for length of breeding schedules
#21/04/2016

#------------import files--------------------
setwd("F:/Plovers/3rd Chapter/Exploratory_results/input")

csvfiles <- list.files(path = ".", pattern='*\\.csv$', all.files=TRUE)
csvfiles

import.list <- lapply(csvfiles, read.csv, header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))

working.list <- import.list[3]

names(working.list) <- c("both")
attach(working.list)
#detach(working.list)
#------------------------------------------------------------


indiv$pop.sp.sex<-ifelse(indiv$population %in% "Tuzla", 
                         paste(indiv$population, indiv$species, indiv$field_sex_focal, sep="-"),
                         paste(indiv$population, indiv$species, indiv$mol_sex_focal, sep="-"))

males.mol<-indiv[grep(indiv$pop.sp.sex, pattern="M$"),]
females.mol<-indiv[grep(indiv$pop.sp.sex, pattern="F$"),]
both<-rbind(males.mol, females.mol)

ind<-which(both$year.cr < both$year)
ringed.prev<-both[ind,]
#reorder factor levels:
ringed.prev$pop.sp.sex <- as.factor(ringed.prev$pop.sp.sex)
print(levels(ringed.prev$pop.sp.sex))
ringed.prev$pop.sp.sex <- factor(ringed.prev$pop.sp.sex, levels(ringed.prev$pop.sp.sex)[c(1,2,11,12,5,6,9,10,7,8,3,4)])
table(ringed.prev$pop.sp.sex)
#plot:
boxplot(bs.length.std~pop.sp.sex, data=ringed.prev,                                                                            
        #horizontal = T,
        names = c("", "", "", "", "","","","","","","",""),
        ylab="",                  
        at =c(1,2, 4,5, 7,8, 10,11, 13,14, 16,17), #specify position of each boxplot on x
        par(mar = c(7, 4, 4, 2)+ 0.1),    #enlarge plot's window default is c(5, 4, 4, 2) + 0.1.
        par(cex.lab=0.75), 
        par(cex.axis=0.7), 
        yaxt='n', 
        ylim=c(-3,+3),                                                                
        boxwex=0.3, outwex=0.3,
        outcex=0.5)                                                                                                                                                                                                      
y<-seq(-3,+3, 0.5)
axis(2, at=y,las=1, cex.axis=0.7) 

mtext("Standardized length of breeding schedules", side=2, line=2.5, cex=0.75)
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

mtext(c("142","181","138","122","17","17","241","220","54","49","44","55"), side= 3, line = 0.5, 
      at=c(1,2,4,5,7,8,10,11,13,14,16,17), cex=0.6)
