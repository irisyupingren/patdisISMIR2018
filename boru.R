library(Boruta)
set.seed(123)

anaboruta <- function(groupfile){
colnames(groupfile)[1] <- "class"
# names(DF) <- gsub(".", "_", names(DF), fixed = TRUE)
groupfile$class <- as.factor(groupfile$class)
levels(groupfile$class) <- make.names(levels(factor(groupfile$class)))
boruta.groupfile <- Boruta(class~., data = groupfile, doTrace = 2)

boruta.groupfile <- borutagroup3
plot(boruta.groupfile, colCode =c("black","grey","white","black"), xlab = "", xaxt = "n", sort = FALSE)
plot(boruta.groupfile)
lz<-lapply(1:ncol(boruta.groupfile$ImpHistory),function(i)
  boruta.groupfile$ImpHistory[is.finite(boruta.groupfile$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.groupfile$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.groupfile$ImpHistory), cex.axis = 0.7)
# title(naming)
return(boruta.groupfile)
}

borutagroup2 <- boruta.group2
borutagroup3 <- anaboruta(group3)
borutagroupalg335 <- anaboruta(groupalg355)
borutagroup335 <- anaboruta(group355)

borutapcagroup2 <- anaboruta(groupcoor2pca)
borutapcagroup3 <- anaboruta(groupcoor3pca)

borutadedupgroup3 <- anaboruta(dedupgroup3)
borutanonsonggroup3 <- anaboruta(group3nonsong1657)
borutanonsongroup3dedup <- anaboruta(group3nonsongunbalan)
borutanonsongbalancegroup3 <- anaboruta(group3nonsong1287)


save(borutagroup2, borutagroup3, borutagroupalg335, borutagroup335, borutapcagroup2, borutapcagroup3, file = "borutagroupresults.RData")

library(Boruta)

# boruta.Imp.sort<-boruta.groupfile$ImpHistory[,order(colMedians(boruta.groupfile$ImpHistory),decreasing=TRUE)]
boruta.Imp.sort<-boruta.groupfile$ImpHistory[,order(apply(boruta.groupfile$ImpHistory, 2, median),decreasing=TRUE)]
decision.order <- order(apply(boruta.groupfile$ImpHistory, 2, median),decreasing=TRUE)[1:63]
boruta.decision.sort<-boruta.groupfile$finalDecision[decision.order]

boruta.groupfile.copy <- boruta.groupfile
boruta.groupfile.copy$ImpHistory <- boruta.Imp.sort
boruta.groupfile.copy$finalDecision <- boruta.decision.sort


dev.off()
# png(filename="boru.pdf", width=900, bg="white")
par(mar=c(23,4,2,1)+.1)
# plot(boruta.groupfile.copy)
plot(boruta.groupfile.copy,  xlab = "", xaxt = "n", yaxt = "n", ylab="", sort = FALSE)
title(ylab="Importance", line=0, cex.lab=1.2, family="Calibri Light")
# plot(boruta.groupfile,  xlab = "", xaxt = "n",sort = FALSE)
lz<-lapply(1:ncol(boruta.groupfile.copy$ImpHistory),function(i)
  boruta.groupfile.copy$ImpHistory[is.finite(boruta.groupfile.copy$ImpHistory[,i]),i])

names(lz) <- colnames(boruta.groupfile.copy$ImpHistory)
names(lz) <- gsub("_", " ", colnames(boruta.groupfile.copy$ImpHistory))
names(lz)[61] <- "randomMax"
names(lz)[64] <- "randomMean"
names(lz)[65] <- "randomMin"

Labels <- sort(sapply(lz,median), decreasing = TRUE)
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.groupfile.copy$ImpHistory), cex.axis = 0.85)
dev.off()
