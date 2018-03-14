#CLEAR WORKSPACE
rm(list = ls(all = TRUE)) 
gc(reset=TRUE)

#Setup parallel cluster
#If running on the command line of linux, use method='fork'
library(doParallel)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)

#Fit model
library(caret)
set.seed(19556)
model <- train(
  Species~., 
  data=iris, 
  method='knn',
  tuneGrid=expand.grid(.k=1:30),
  metric='Accuracy',
  trControl=trainControl(
    method='repeatedcv', 
    number=10, 
    repeats=15,
    classProbs=TRUE,
    summaryFunction=multiClassSummary))

#Stop parallel cluster
stopCluster(cl)

#Save pdf of plots
dev.off()
pdf('plots.pdf')
for(stat in c('Accuracy', 'Kappa', 'AccuracyLower', 'AccuracyUpper', 'AccuracyPValue', 
              'Sensitivity', 'Specificity', 'Pos_Pred_Value', 
              'Neg_Pred_Value', 'Detection_Rate', 'ROC', 'logLoss')) {
  
  print(plot(model, metric=stat))
}
dev.off()