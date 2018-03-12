library(mlbench)
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)

classifygroup <- function(groupfile, control){
  groupfile$motifid <- NULL
  groupfile$X1 <- NULL
  groupfile$num <- NULL
  
  set.seed(7)
  print("starting lda")
  modellda <- train(tunefamily~., data=groupfile, method="lda", trControl=control)
  print("starting lvq")
  modelLvq <- train(tunefamily~., data=groupfile, method="lvq", trControl=control)
  print("starting gbm")
  modelGbm <- train(tunefamily~., data=groupfile, method="gbm", trControl=control, verbose=FALSE)
  print("starting svm")
  modelSvm <- train(tunefamily~., data=groupfile, method="svmRadial", trControl=control)
  print("starting rf")
  modelrf <- train(tunefamily~., data=groupfile, method="rf", trControl=control)
  caresults <- resamples(list(LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm,
                              LDA=modellda, RF=modelrf))
  
  return(caresults)
}