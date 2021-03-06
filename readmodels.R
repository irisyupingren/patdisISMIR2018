library(caret)
library(glue)

readmodels <- function(expnum){
  print("reading lda")
  modellda <- readRDS(glue("modellda{expnum}.rds")
  final_predictionsproblda <- readRDS(glue("predldaprob{expnum}.rds")
  cmlda <- readRDS(glue("cmlda{expnum}.rds")
  # 
  print("starting nb")
  modelnb <- train(class~., data=training, method = 'nb', trControl=control, preProc=c("center", "scale"))
  saveRDS(modelnb,file = glue("modelnb{expnum}.rds"))
  final_predictionsnb <- predict(modelnb, validationdata)
  final_predictionsprobnb <- predict(modelnb, newdata = validationdata, type = "prob")
  # saveRDS(final_predictionsnb, file = glue("prednb{expnum}.rds"))
  saveRDS(final_predictionsprobnb, file = glue("prednbprob{expnum}.rds"))
  cmnb <- confusionMatrix(final_predictionsnb, validation$class)
  saveRDS(cmnb, file = glue("cmnb{expnum}.rds"))
  
  print("starting lvq")
  modelLvq <- train(class~., data=training, method="lvq", trControl=control, preProc=c("center", "scale"))
  saveRDS(modelLvq,file = glue("modellvq{expnum}.rds"))
  library(caret)
  final_predictionslvq <- predict(modelLvq, validationdata)
  # final_predictionsproblvq <- predict(modelLvq, newdata = validationdata, type = "prob")
  # saveRDS(final_predictionslvq, file = glue("predlvq{expnum}.rds"))
  # saveRDS(final_predictionsproblvq, file = glue("predlvqprob{expnum}.rds"))
  cmlvq <- confusionMatrix(final_predictionslvq, validation$class)
  saveRDS(cmlvq, file = glue("cmlvq{expnum}.rds"))
  
  print("starting gbm")
  modelGbm <- train(class~., data=training, method="gbm", trControl=control, preProc=c("center", "scale"))
  saveRDS(modelGbm,file = glue("modelgbm{expnum}.rds"))
  final_predictionsGbm <- predict(modelGbm, validationdata)
  final_predictionsprobGbm <- predict(modelGbm, newdata = validationdata, type = "prob")
  # saveRDS(final_predictionsGbm, file = glue("predGbm{expnum}.rds"))
  saveRDS(final_predictionsprobGbm, file = glue("predgbmprob{expnum}.rds"))
  cmGbm <- confusionMatrix(final_predictionsGbm, validation$class)
  saveRDS(cmGbm, file = glue("cmGbm{expnum}.rds"))
  
  print("starting svm")
  modelSvm <- train(class~., data=training, method="svmRadial", trControl=control, preProc=c("center", "scale"))
  saveRDS(modelSvm,file = glue("modelsvm{expnum}.rds"))
  final_predictionsSvm <- predict(modelSvm, newdata = validationdata)
  final_predictionsprobSvm <- predict(modelSvm, newdata = validationdata, type = "prob")
  # saveRDS(final_predictionsSvm, file = glue("predSvm{expnum}.rds"))
  saveRDS(final_predictionsprobSvm, file = glue("predsvmprob{expnum}.rds"))
  cmSvm <- confusionMatrix(final_predictionsSvm, validation$class)
  saveRDS(cmSvm, file = glue("cmSvm{expnum}.rds"))
  
  print("starting rf")
  modelrf <- train(class~., data=training, method="rf", trControl=control, preProc=c("center", "scale"))
  saveRDS(modelrf,file = glue("modelrf{expnum}.rds"))
  final_predictionsrf <- predict(modelrf$finalModel, newdata = scale(validationdata))
  final_predictionsprobrf <- predict(modelrf$finalModel, newdata = scale(validationdata), type = "prob")
  # saveRDS(final_predictionsrf, file = glue("predrf{expnum}.rds"))
  saveRDS(final_predictionsprobrf, file = glue("predrfprob{expnum}.rds"))
  cmrf <- confusionMatrix(final_predictionsrf, validation$class)
  saveRDS(cmrf, file = glue("cmrf{expnum}.rds"))
  
  models <- list(NB=modelnb, LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm,LDA=modellda, RF=modelrf)
  predTargets <- extractPrediction(models, testX = validationdata, testY = validation$class)
  caresults <- resamples(models)
  output <-list(caresults, predTargets)
  return(output)
}