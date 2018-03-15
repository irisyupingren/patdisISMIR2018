library(Boruta)
set.seed(123)

anaboruta <- function(groupfile, naming){
colnames(groupfile)[1] <- "class"
groupfile$class <- as.factor(groupfile$class)
levels(groupfile$class) <- make.names(levels(factor(groupfile$class)))
boruta.groupfile <- Boruta(class~., data = groupfile, doTrace = 2)

plot(boruta.groupfile, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.groupfile$ImpHistory),function(i)
  boruta.groupfile$ImpHistory[is.finite(boruta.groupfile$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.groupfile$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.groupfile$ImpHistory), cex.axis = 0.7)
title(naming)
return(boruta.groupfile)
}

borutagroup2 <- boruta.group2
borutagroup3 <- anaboruta(group3)
borutagroupalg335 <- anaboruta(groupalg355)
borutagroup335 <- anaboruta(group355)

borutapcagroup2 <- anaboruta(groupcoor2pca)
borutapcagroup3 <- anaboruta(groupcoor3pca)

save(borutagroup2, borutagroup3, borutagroupalg335, borutagroup335, borutapcagroup2, borutapcagroup3, file = "borutagroupresults.RData")