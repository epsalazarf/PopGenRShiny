# Admixture Plotting
# Author: Pavel Salazar-Fernandez
# Developed at: LANGEBIO - Mexico
# Original Script: Andres Moreno-Estrada (Nov 2012)

# Pipeline:
# 1. Reads .Q File of selected K.
# 2. Reads popinfo file.
# 3. Generates plot.

# Features:
# - Changing the 'Ks' parameter changes the plot showed.

#<START>

#<INPUT>
#Set working directory
setwd("PATH")

#Set Base File Name (all characters before ".[K-number].Q"):
basefile <- "File"

#Declare popinfo File:
pifile <-"popinfo.txt"

#Set K number to read:
Ks <- 7

#</INPUT>

#Data Extraction
actfile <- paste(basefile,".",Ks,".Q",sep="")
kData <- read.table(actfile,header=FALSE)
popinfo <- read.csv(pifile,header=TRUE, sep= " ")
barnames <- as.vector(popinfo[,8])
popnames <- unique(barnames)

#Data Sorting
for(p in popnames[2:length(popnames)]){
  pop <- as.numeric(rownames(popinfo[popinfo$POP==p,]))
  vsort <- names(sort(apply(kData[c(pop),],2,mean), decreasing = TRUE))[1]
  popsort <- order(kData[c(pop),][vsort[1]])

  kData[min(pop):max(pop),] <- kData[pop[popsort],]
}

#Data Formatting
KData <- t(kData)
spaces <- c(0,diff(popinfo[,8]))
spaces <- replace(spaces, spaces != 0, 2)
for (i in 2:length(barnames)){if(spaces[i] == 0){barnames[i] <- ""}}
barcolors <- c("red2","blue3","green4","blueviolet","darkorange","cyan2","gold"
               ,"deeppink1","chartreuse2","dodgerblue","saddlebrown")

#<OUTPUT>

#Plot ancestry proportions
barplot(KData, col=barcolors, border=NA, space=spaces, las=2, 
        xlab= "Population",ylab= paste("K=",Ks), names.arg = barnames,
        cex.names = 0.7, cex.axis= 0.7, cex=0.5)

#</OUTPUT>

#<END>


#<SANDBOX>
sel.col = c(1,2,3,30:300)
barplot(KData[,sel.col], col=barcolors, border=NA, space= spaces[sel.col], las=2, 
        xlab= "Population", ylab= paste("K=",Ks), names.arg= barnames[sel.col],
        cex.names = 0.7, cex.axis= 0.7, cex=0.5)


