pi <- read.csv("1KGP3.popinfo.txt", sep="\t")

pops <- as.character(unique(pi$POP))

nsam <- 5

popsel <- c()

for (i in pops){
  popsel <- c(popsel,sort(sample(which(pi$POP==i),nsam)))
}

popx <- pi[popsel,]

pids <- popx[,c("FAMID","ID")]

write.table(popx,file="1KG.sample.popinfo.txt",quote=F,sep="\t")
write.table(pids,file="1KG.sample.ids.txt",quote=F,sep="\t",row.names = F,col.names = F)
