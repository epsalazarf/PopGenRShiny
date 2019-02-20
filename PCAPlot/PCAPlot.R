#Test script, most features tried here are included in app.R

setwd("PATH)")

sample=c("XYZ")

sample.pdf=paste(sample,"TestPCA.pdf",sep=".")

evec.in="file.evec"
eval.in="file.eval"
  
  
data <- read.table(evec.in)
eval <- read.table(eval.in)
#data <- data[nrow(data):1,]
## data must match the dimensions of data matrix (eg. (1,10) if the first 10 PCs are given) 
pca <- data[,seq(1,10)+1]

# -> CALCULATE PCA CONTRIBUTIONS
pct.varPC1 = paste("PC1 (",round(100*eval[1,]/sum(eval),2),"%)",sep="")
pct.varPC2 = paste("PC2 (",round(100*eval[2,]/sum(eval),2),"%)",sep="")
pct.varPC1
pct.varPC2

####
individual.name <- gsub(".*:","",data[,1])
sample.idx <- match(sample,gsub(".*:","",data[,1]))

#### population information:
popinfo <- read.csv("popinfoBot1517.txt",sep="",header=T)

## popinfo file can be longer than the actual number of samples ploted as long as the ID is matched by the index function
## but if there is an excess of samples with corresponding regions and colors, these will be ploted in the legend too
## therefore, ideal to have a popinfo file for each plotting session

regions <- as.character(unique(popinfo[,13]))
populations <- as.character(unique(popinfo[,9]))


#regions.color <- c("black","#00008090", "#1E90FF90", "#0000FF90", "#87CEEB90", "#00F5F590")
regions.color <- c("black","navyblue", "dodgerblue", "blue", "skyblue", "turquoise1", "darkgreen", "lightgreen")


names(regions.color) <- regions

print(cbind(regions,regions.color))

#### map to the pop infor.
nsamples <- length(individual.name)

idx <- match(individual.name, popinfo[,2])
pop.pc1.pc2 <- cbind(popinfo[idx,13],data[,2:3])
colors <- regions.color[as.character(popinfo[idx,13])]

##########################################################################################
## WITH SOLID CIRCLES
##########################################################################################

pdf(sample.pdf)
plot(pca[,1],pca[,2],col = colors, pch = 19, xlab =pct.varPC1,ylab=pct.varPC2)
abline(v=0,h=0,lty=2,col="grey")
legend("topright", regions, ncol=1,col= regions.color,pch=19, cex=0.7,bty = "n")
points(pca[sample.idx,c(1,2)], cex = 1, pch = 19, col = "black")
text(pca[sample.idx,c(1,2)],sample,pos=4,offset=0.25,cex=0.6)

##########################################################################################
## WITH POP ID NAMES
##########################################################################################

#pdf(sample.ids.pdf)
plot(pca[,1],pca[,2],type="n",col = colors, pch = 19, xlab =pct.varPC1,ylab=pct.varPC2)
abline(v=0,h=0,lty=2,col="grey")
text(pca[,1],pca[,2], labels=popinfo[idx,1],col=colors,cex=0.5)
points(pca[sample.idx,c(1,2)], cex = 1, pch = 19, col = "black")
text(pca[sample.idx,c(1,2)],sample,pos=4,offset=0.25,cex=0.6)
dev.off()

plot(pca[,1],pca[,2],type="n",col = colors, pch = 19, xlab =pct.varPC1,ylab=pct.varPC2, xlim=c(-0.03,-0.02),ylim =c(-0.025,-0.015))
abline(v=0,h=0,lty=2,col="grey")
text(pca[,1],pca[,2], labels=popinfo[idx,1],col=colors,cex=0.5)
points(pca[sample.idx,c(1,2)], cex = 1, pch = 19, col = "black")
text(pca[sample.idx,c(1,2)],sample,pos=4,offset=0.25,cex=0.6)
dev.off()

NeartBot17 <- popinfo[popinfo$ID=="TAH-487",]
append(NeartBot17,popinfo[popinfo$ID=="1023",])


#SUBSELECT POPULATION
sbP = "XYZ"
sbP.idn <- as.vector(popinfo[popinfo$POP==sbP,2])
sbP.idx <- match(sbP.idn, data[,1])
plot(pca[,1],pca[,2],type="n",col = colors, pch = 19, xlab =pct.varPC1,ylab=pct.varPC2)
text(pca[sbP.idx,c(1,2)],sbP.idn,pos=4,offset=0.25,cex=0.5, col= "cyan")
text(pca[sample.idx,c(1,2)],sample,pos=4,offset=0.25,cex=0.6)

roof(max(pca[sbP.idx,c(1)])*1.10,3)

plot(pca[,1],pca[,2],type="n",col = colors, pch = 19, xlab =pct.varPC1,ylab=pct.varPC2, xlim=c(-0.0417,0.0063),ylim =c(-0.0263,-0.0052))

#xlim=zLimits[0:2],ylim = zLimits[3:4]

limits <- c()
limits <- c(limits,min(pca[subpop,c(1)])-abs(min(pca[subpop,c(1)])*0.1))
limits <- c(limits,max(pca[subpop,c(1)])+abs(max(pca[subpop,c(1)])*0.1))
limits <- c(limits,min(pca[subpop,c(2)])-abs(min(pca[subpop,c(2)])*0.1))
limits <- c(limits,max(pca[subpop,c(2)])+abs(max(pca[subpop,c(2)])*0.1))

