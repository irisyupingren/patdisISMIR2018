library(mlbench)
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)

classifygroup <- function(groupfile, control){
  colnames(groupfile)[1] <- "class"
  groupfile$class <- as.factor(groupfile$class)
  levels(groupfile$class) <- make.names(levels(factor(groupfile$class)))
  
  
  set.seed(7)
  print("starting lda")
  modellda <- train(class~., data=groupfile, method="lda", trControl=control)
  print("starting lvq")
  modelLvq <- train(class~., data=groupfile, method="lvq", trControl=control)
  print("starting gbm")
  modelGbm <- train(class~., data=groupfile, method="gbm", trControl=control, verbose=FALSE)
  print("starting svm")
  modelSvm <- train(class~., data=groupfile, method="svmRadial", trControl=control)
  print("starting rf")
  modelrf <- train(class~., data=groupfile, method="rf", trControl=control)
  caresults <- resamples(list(LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm,
                              LDA=modellda, RF=modelrf))
  
  return(caresults)
}

caranresults <- classifygroup(groupran,control)
saveRDS(caranresults,"caranresults.rds")

group300 <- readRDS(file="group300.rds")
ca300results <- classifygroup(group300,control)

group4 <- readRDS(file="group4.rds")
ca1657results <- classifygroup(group4,control)
# summary(results)
bwplot(caresults)# # dot plots of results

cagroup10results <- classifygroup(groupran,control)
