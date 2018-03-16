library(mlbench)
library(caret)
library(glue)

control <- trainControl(method="repeatedcv", verboseIter = TRUE, summaryFunction = multiClassSummary, classProbs = T, savePredictions = T, number=10, repeats=3)
control <- trainControl(method="repeatedcv", preProcOptions = list(thresh=0.8), verboseIter = TRUE, summaryFunction = multiClassSummary, classProbs = T, savePredictions = T, number=10, repeats=3)

classifygroupprepropval <- function(groupfile, control, expnum){
  
  colnames(groupfile)[1] <- "class"
  groupfile$class <- as.factor(groupfile$class)
  levels(groupfile$class) <- make.names(levels(factor(groupfile$class)))
  
  validation_index <- createDataPartition(groupfile$class, p=0.80, list=FALSE)
  validation <- groupfile[-validation_index,]
  saveRDS(validation), file = glue("val{expnum}.rds"))
  validationdata <- validation
  validationdata$class <- NULL
  training <- groupfile[validation_index,]
  saveRDS(training), file = glue("train{expnum}.rds"))
  
  
  set.seed(7)
  print("starting lda")
  modellda <- train(class~., data=training, method="lda", trControl=control, preProc=c("center", "scale"))
  final_predictionslda <- predict(modellda$finalModel, validationdata)
  cmlda <- confusionMatrix(final_predictionslda$class, validation$class)
  saveRDS(modellda, file = glue("modellda{expnum}.rds"))
  saveRDS(final_predictionslda, file = glue("predlda{expnum}.rds"))
  saveRDS(cmlda, file = glue("cmlda{expnum}.rds"))
  
  print("starting nb")
  modelnb <- train(class~., data=training, method = 'nb', trControl=control, preProc=c("center", "scale"))
  saveRDS(modelnb,file = glue("modelnb{expnum}.rds"))
  final_predictionsnb <- predict(modelnb$finalModel, validationdata)
  saveRDS(final_predictionsnb, file = glue("prednb{expnum}.rds"))
  cmnb <- confusionMatrix(final_predictionsnb, validation$class)
  saveRDS(cmnb, file = glue("cmnb{expnum}.rds"))
  
  print("starting lvq")
  modelLvq <- train(class~., data=training, method="lvq", trControl=control, preProc=c("center", "scale"))
  saveRDS(modelLvq,file = glue("modellvq{expnum}.rds"))
  final_predictionslvq <- predict(modelLvq$finalModel, validationdata)
  saveRDS(final_predictionslvq, file = glue("predlvq{expnum}.rds"))
  cmlvq <- confusionMatrix(final_predictionslvq, validation$class)
  saveRDS(cmlvq, file = glue("cmlvq{expnum}.rds"))
  
  print("starting gbm")
  modelGbm <- train(class~., data=training, method="gbm", trControl=control, preProc=c("center", "scale"))
  saveRDS(modelGbm,file = glue("modelgbm{expnum}.rds"))
  final_predictionsgbm <- predict(modelgbm$finalModel, validationdata)
  saveRDS(final_predictionsgbm, file = glue("predgbm{expnum}.rds"))
  cmgbm <- confusionMatrix(final_predictionsgbm, validation$class)
  saveRDS(cmgbm, file = glue("cmgbm{expnum}.rds"))

  print("starting svm")
  modelSvm <- train(class~., data=training, method="svmRadial", trControl=control, preProc=c("center", "scale"))
  saveRDS(modelSvm,file = glue("modelsvm{expnum}.rds"))
  final_predictionssvm <- predict(modelsvm$finalModel, validationdata)
  saveRDS(final_predictionssvm, file = glue("predsvm{expnum}.rds"))
  cmsvm <- confusionMatrix(final_predictionssvm, validation$class)
  saveRDS(cmsvm, file = glue("cmsvm{expnum}.rds"))
  # 
  print("starting rf")
  modelrf <- train(class~., data=training, method="rf", trControl=control, preProc=c("center", "scale"))
  saveRDS(modelrf,file = glue("modelrf{expnum}.rds"))
  final_predictionsrf <- predict(modelrf$finalModel, validationdata)
  saveRDS(final_predictionsrf, file = glue("predrf{expnum}.rds"))
  cmrf <- confusionMatrix(final_predictionsrf, validation$class)
  saveRDS(cmrf, file = glue("cmrf{expnum}.rds"))

  caresults <- resamples(list(NB=modelnb, LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm,
                              LDA=modellda, RF=modelrf))

  return(caresults)
}

cagroup2valresults <- classifygroupprepropval(group2, control, "group2")
