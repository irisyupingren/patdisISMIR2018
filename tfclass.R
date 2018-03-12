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

classifygroup <- function(groupfile, control, expnum){
  groupfile$motifid <- NULL
  groupfile$X1 <- NULL
  groupfile$num <- NULL
  
  set.seed(7)
  print("starting lda")
  modellda <- train(class~., data=groupfile, method="lda", trControl=control)
  saveRDS(modellda,file = glue("modellda{expnum}.rds"))
  
  print("starting lvq")
  modelLvq <- train(class~., data=groupfile, method="lvq", trControl=control)
  saveRDS(modelLvq,file = glue("modellvq{expnum}.rds"))
  
  print("starting gbm")
  modelGbm <- train(class~., data=groupfile, method="gbm", trControl=control, verbose=FALSE)
  saveRDS(modelGbm,file = glue("modelgbm{expnum}.rds"))
  
  print("starting svm")
  modelSvm <- train(class~., data=groupfile, method="svmRadial", trControl=control)
  saveRDS(modelSvm,file = glue("modelsvm{expnum}.rds"))
  
  print("starting rf")
  modelrf <- train(class~., data=groupfile, method="rf", trControl=control)
  saveRDS(modelrf,file = glue("modelrf{expnum}.rds"))
  
  caresults <- resamples(list(LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm,
                              LDA=modellda, RF=modelrf))
  # caresults <- resamples(list(LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm,
  #                             RF=modelrf))
  return(caresults)
}

classify4group <- function(groupfile, control, expnum){
  colnames(groupfile)[64] <- "class"
  groupfile$class <- as.factor(groupfile$class)
  levels(groupfile$class) <- make.names(levels(factor(groupfile$class)))
  
  # set.seed(7)
  
  print("starting lvq")
  modelLvq <- train(class~., data=groupfile, method="lvq", trControl=control)
  saveRDS(modelLvq,file = glue("modellvq{expnum}.rds"))
  
  print("starting gbm")
  modelGbm <- train(class~., data=groupfile, method="gbm", trControl=control, verbose=FALSE)
  saveRDS(modelGbm,file = glue("modelgbm{expnum}.rds"))
  
  print("starting svm")
  modelSvm <- train(class~., data=groupfile, method="svmRadial", trControl=control)
  saveRDS(modelSvm,file = glue("modelsvm{expnum}.rds"))
  
  print("starting rf")
  modelrf <- train(class~., data=groupfile, method="rf", trControl=control)
  saveRDS(modelrf,file = glue("modelrf{expnum}.rds"))
  
  caresults <- resamples(list(LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm,
                              RF=modelrf))
  return(caresults)
}

selemerge <- selemergedjsym(merged)
seleranmerge <- selemergedjsym(ran3merged)
selenonmerge <- selemergedjsym(nonmerged)

tfranresults <- classify4group(seleranmerge, control, "tf")
saveRDS(tfranresults, file = "tfresults.rds")
tfranresults <- classify4group(seleranmerge, control, "tfran")
saveRDS(tfranresults, file = "tfranresults.rds")
tfnonresults <- classify4group(selenonmerge, control,"tfnon")
saveRDS(tfnonnresults, file = "tfnonresults.rds")

desresults <- classify4group(selemerge, control, "des")
saveRDS(desresults, file = "desresults.rds")
desranresults <- classify4group(seleranmerge, control, "desran")
saveRDS(desranresults, file = "desranresults.rds")
desnonresults <- classify4group(selenonmerge, control,"desnon")
saveRDS(desnonresults, file = "desnonresults.rds")

annoresults <- classify4group(selemerge, control, "anno")
saveRDS(annoranresults, file = "annoranresults.rds")
annoranresults <- classify4group(seleranmerge, control, "annoran")
saveRDS(annoranresults, file = "annoranresults.rds")
annononresults <- classify4group(selenonmerge, control,"annonon")
saveRDS(annononresults, file = "annononresults.rds")
