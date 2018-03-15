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

anaboruta(group3)
