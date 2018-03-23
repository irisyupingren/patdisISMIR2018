library(mlbench)
library(caret)
library(glue)

control <- trainControl(method="repeatedcv", verboseIter = TRUE, summaryFunction = multiClassSummary, classProbs = T, savePredictions = T, number=10, repeats=3)
# control <- trainControl(method="repeatedcv", preProcOptions = list(thresh=0.8), verboseIter = TRUE, summaryFunction = multiClassSummary, classProbs = T, savePredictions = T, number=10, repeats=3)

classifygroupprepropvalpred <- function(groupfile, control, expnum){
  
  colnames(groupfile)[1] <- "class"
  groupfile$class <- as.factor(groupfile$class)
  levels(groupfile$class) <- make.names(levels(factor(groupfile$class)))
  
  validation_index <- createDataPartition(groupfile$class, p=0.90, list=FALSE)
  validation <- groupfile[-validation_index,]
  saveRDS(validation, file = glue("val{expnum}.rds"))
  validationdata <- validation
  validationdata$class <- NULL
  
  training <- groupfile[validation_index,]
  saveRDS(training, file = glue("train{expnum}.rds"))
  
  set.seed(7)
  print("starting lda")
  modellda <- caret::train(class~., data=training, method="lda", trControl=control, preProc=c("center", "scale"))
  final_predictionslda <- predict(modellda, validationdata)
  final_predictionsproblda <- predict(modellda, newdata = validationdata, type = "prob")
  saveRDS(final_predictionsproblda, file = glue("predldaprob{expnum}.rds"))
  cmlda <- confusionMatrix(final_predictionslda, validation$class)
  saveRDS(modellda, file = glue("modellda{expnum}.rds"))
  # saveRDS(final_predictionslda, file = glue("predlda{expnum}.rds"))
  saveRDS(cmlda, file = glue("cmlda{expnum}.rds"))
  # 
  print("starting nb")
  modelnb <- caret::train(class~., data=training, method = 'nb', trControl=control, preProc=c("center", "scale"))
  saveRDS(modelnb,file = glue("modelnb{expnum}.rds"))
  final_predictionsnb <- predict(modelnb, validationdata)
  final_predictionsprobnb <- predict(modelnb, newdata = validationdata, type = "prob")
  # saveRDS(final_predictionsnb, file = glue("prednb{expnum}.rds"))
  saveRDS(final_predictionsprobnb, file = glue("prednbprob{expnum}.rds"))
  cmnb <- confusionMatrix(final_predictionsnb, validation$class)
  saveRDS(cmnb, file = glue("cmnb{expnum}.rds"))
  
  print("starting lvq")
  modelLvq <- caret::train(class~., data=training, method="lvq", trControl=control, preProc=c("center", "scale"))
  saveRDS(modelLvq,file = glue("modellvq{expnum}.rds"))
  library(caret)
  final_predictionslvq <- predict(modelLvq, validationdata)
  # final_predictionsproblvq <- predict(modelLvq, newdata = validationdata, type = "prob")
  # saveRDS(final_predictionslvq, file = glue("predlvq{expnum}.rds"))
  # saveRDS(final_predictionsproblvq, file = glue("predlvqprob{expnum}.rds"))
  cmlvq <- confusionMatrix(final_predictionslvq, validation$class)
  saveRDS(cmlvq, file = glue("cmlvq{expnum}.rds"))
  
  print("starting gbm")
  modelGbm <- caret::train(class~., data=training, method="gbm", trControl=control, preProc=c("center", "scale"))
  saveRDS(modelGbm,file = glue("modelgbm{expnum}.rds"))
  final_predictionsGbm <- predict(modelGbm, validationdata)
  final_predictionsprobGbm <- predict(modelGbm, newdata = validationdata, type = "prob")
  # saveRDS(final_predictionsGbm, file = glue("predGbm{expnum}.rds"))
  saveRDS(final_predictionsprobGbm, file = glue("predgbmprob{expnum}.rds"))
  cmGbm <- confusionMatrix(final_predictionsGbm, validation$class)
  saveRDS(cmGbm, file = glue("cmGbm{expnum}.rds"))
  
  print("starting svm")
  modelSvm <- caret::train(class~., data=training, method="svmRadial", trControl=control, preProc=c("center", "scale"))
  saveRDS(modelSvm,file = glue("modelsvm{expnum}.rds"))
  final_predictionsSvm <- predict(modelSvm, newdata = validationdata)
  final_predictionsprobSvm <- predict(modelSvm, newdata = validationdata, type = "prob")
  # saveRDS(final_predictionsSvm, file = glue("predSvm{expnum}.rds"))
  saveRDS(final_predictionsprobSvm, file = glue("predsvmprob{expnum}.rds"))
  cmSvm <- confusionMatrix(final_predictionsSvm, validation$class)
  saveRDS(cmSvm, file = glue("cmSvm{expnum}.rds"))
  
  print("starting rf")
  modelrf <- caret::train(class~., data=training, method="rf", trControl=control, preProc=c("center", "scale"))
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
cacoorgroup2algpatoutput <- classifygroupprepropvalpred(groupcooralgpat2pca, control, "coorgroup2algpatnewdata")
saveRDS(cacoorgroup2algpatoutput, file = "cacoorgroupalgpat2output")

cagroup2output <- classifygroupprepropvalpred(group2, control, "group2newdata")
saveRDS(cagroup2output, file = "cagroup2output.rds")

cagroup3output <- classifygroupprepropvalpred(group3, control, "group3newdata")
saveRDS(cagroup3output, file = "cagroup3output.rds")

cagroup355output <- classifygroupprepropvalpred(group355, control, "group355newdata")
saveRDS(cagroup355output, file = "cagroup355output.rds")

cagroupalg355output <- classifygroupprepropvalpred(groupalg355, control, "groupalg355newdata")
saveRDS(cagroupalg355output, file = "cagroupalg355output.rds")
