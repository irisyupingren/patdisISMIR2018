library(Boruta)
set.seed(123)
source("utilfuncs.R")

selemerged <- selemergedjsym(merged)
boruta.train <- Boruta(des~., data = selemerged, doTrace = 2)

selenonmerged <- selemergedjsym(nonmerged)
nonboruta.train <- Boruta(des~., data = selenonmerged, doTrace = 2)

seleranmerged <- selemergedjsym(ran3merged)
ranboruta.train <- Boruta(des~., data = seleranmerged, doTrace = 2)



dev.off()
attach(mtcars)
par(mfrow=c(3,1))
par(bty = 'n') 

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

plot(nonboruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(nonboruta.train$ImpHistory),function(i)
  nonboruta.train$ImpHistory[is.finite(nonboruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(nonboruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(nonboruta.train$ImpHistory), cex.axis = 0.7)

plot(ranboruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(ranboruta.train$ImpHistory),function(i)
  ranboruta.train$ImpHistory[is.finite(ranboruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(ranboruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(ranboruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)

library(mlr)
dev.off()
attach(mtcars)
par(mfrow=c(3,1))
par(bty = 'n') 
fs <- makeClassifTask(data = selemerged, target = "des")
fv2 = generateFilterValuesData(fs, method = c("information.gain", "chi.squared"))
plotFilterValues(fv2)
fs <- makeClassifTask(data = selenonmerged, target = "des")
fv2 = generateFilterValuesData(fs, method = c("information.gain", "chi.squared"))
plotFilterValues(fv2)
fs <- makeClassifTask(data = seleranmerged, target = "des")
fv2 = generateFilterValuesData(fs, method = c("information.gain", "chi.squared"))
plotFilterValues(fv2)


selemerged <- selemergedjsym(merged)
tfboruta.train <- Boruta(tf~., data = selemerged, doTrace = 2)

selenonmerged <- selemergedjsym(nonmerged)
tfnonboruta.train <- Boruta(tf~., data = selenonmerged, doTrace = 2)

seleranmerged <- selemergedjsym(ran3merged)
tfranboruta.train <- Boruta(tf~., data = seleranmerged, doTrace = 2)

selemerged <- selemergedjsym(merged)
annoboruta.train <- Boruta(anno~., data = selemerged, doTrace = 2)

selenonmerged <- selemergedjsym(nonmerged)
annononboruta.train <- Boruta(anno~., data = selenonmerged, doTrace = 2)

seleranmerged <- selemergedjsym(ran3merged)
annoranboruta.train <- Boruta(anno~., data = seleranmerged, doTrace = 2)

save(boruta.train, tfboruta.train, annoboruta.train, nonboruta.train, tfnonboruta.train, annononboruta.train, ranboruta.train, tfranboruta.train, annoranboruta.train, file = "boruta.RData")

dev.off()
attach(mtcars)
par(mfrow=c(3,1))
par(bty = 'n') 

plot(tfboruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(tfboruta.train$ImpHistory),function(i)
  tfboruta.train$ImpHistory[is.finite(tfboruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(tfboruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(tfboruta.train$ImpHistory), cex.axis = 0.7)

plot(tfnonboruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(tfnonboruta.train$ImpHistory),function(i)
  tfnonboruta.train$ImpHistory[is.finite(tfnonboruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(tfnonboruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(tfnonboruta.train$ImpHistory), cex.axis = 0.7)

plot(tfranboruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(tfranboruta.train$ImpHistory),function(i)
  tfranboruta.train$ImpHistory[is.finite(tfranboruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(tfranboruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(tfranboruta.train$ImpHistory), cex.axis = 0.7)



dev.off()
attach(mtcars)
par(mfrow=c(3,1))
par(bty = 'n') 
plot(annoboruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(tfboruta.train$ImpHistory),function(i)
  tfboruta.train$ImpHistory[is.finite(tfboruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(tfboruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(tfboruta.train$ImpHistory), cex.axis = 0.7)

plot(annononboruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(tfnonboruta.train$ImpHistory),function(i)
  tfnonboruta.train$ImpHistory[is.finite(tfnonboruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(tfnonboruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(tfnonboruta.train$ImpHistory), cex.axis = 0.7)

plot(annoranboruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(tfranboruta.train$ImpHistory),function(i)
  tfranboruta.train$ImpHistory[is.finite(tfranboruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(tfranboruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(tfranboruta.train$ImpHistory), cex.axis = 0.7)


dev.off()
attach(mtcars)
par(mfrow=c(3,1))
par(bty = 'n') 
fs <- makeClassifTask(data = selemerged, target = "tf")
fv2 = generateFilterValuesData(fs, method = c("information.gain", "chi.squared"))
plotFilterValues(fv2)
fs <- makeClassifTask(data = selenonmerged, target = "tf")
fv2 = generateFilterValuesData(fs, method = c("information.gain", "chi.squared"))
plotFilterValues(fv2)
fs <- makeClassifTask(data = seleranmerged, target = "tf")
fv2 = generateFilterValuesData(fs, method = c("information.gain", "chi.squared"))
plotFilterValues(fv2)

dev.off()
attach(mtcars)
par(mfrow=c(3,1))
par(bty = 'n') 
fs <- makeClassifTask(data = selemerged, target = "anno")
fv2 = generateFilterValuesData(fs, method = c("information.gain", "chi.squared"))
plotFilterValues(fv2)
fs <- makeClassifTask(data = selenonmerged, target = "anno")
fv2 = generateFilterValuesData(fs, method = c("information.gain", "chi.squared"))
plotFilterValues(fv2)
fs <- makeClassifTask(data = seleranmerged, target = "anno")
fv2 = generateFilterValuesData(fs, method = c("information.gain", "chi.squared"))
plotFilterValues(fv2)

