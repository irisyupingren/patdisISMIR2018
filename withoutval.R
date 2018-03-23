library(mlbench)
library(caret)
library(glue)

control <- trainControl(method="repeatedcv", verboseIter = TRUE, summaryFunction = multiClassSummary, classProbs = T, savePredictions = T, number=10, repeats=3)
# control <- trainControl(method="repeatedcv", preProcOptions = list(thresh=0.8), verboseIter = TRUE, summaryFunction = multiClassSummary, classProbs = T, savePredictions = T, number=10, repeats=3)

classifygroupprepropvalpred <- function(groupfile, control, expnum){
  
  colnames(groupfile)[1] <- "class"
  groupfile$class <- as.factor(groupfile$class)
  levels(groupfile$class) <- make.names(levels(factor(groupfile$class)))

  training <- groupfile
  validation <- groupfile
  validationdata <- groupfile[2:64]
    
  set.seed(7)
  print("starting lda")
  modellda <- caret::train(class~., data=training, method="lda", trControl=control, preProc=c("center", "scale"))
  final_predictionslda <- predict(modellda, validationdata)
  # saveRDS(final_predictionsproblda, file = glue("predlda{expnum}.rds"))
  final_predictionsproblda <- predict(modellda, newdata = validationdata, type = "prob")
  saveRDS(final_predictionsproblda, file = glue("predldaprob{expnum}.rds"))
  cmlda <- confusionMatrix(final_predictionslda, validation$class)
  saveRDS(modellda, file = glue("modellda{expnum}.rds"))
  saveRDS(final_predictionslda, file = glue("predlda{expnum}.rds"))
  saveRDS(cmlda, file = glue("cmlda{expnum}.rds"))
  # 
  print("starting nb")
  modelnb <- train(class~., data=training, method = 'nb', trControl=control, preProc=c("center", "scale"))
  saveRDS(modelnb,file = glue("modelnb{expnum}.rds"))
  final_predictionsnb <- predict(modelnb, validationdata)
  final_predictionsprobnb <- predict(modelnb, newdata = validationdata, type = "prob")
  saveRDS(final_predictionsnb, file = glue("prednb{expnum}.rds"))
  saveRDS(final_predictionsprobnb, file = glue("prednbprob{expnum}.rds"))
  cmnb <- confusionMatrix(final_predictionsnb, validation$class)
  saveRDS(cmnb, file = glue("cmnb{expnum}.rds"))
  
  print("starting lvq")
  modelLvq <- train(class~., data=training, method="lvq", trControl=control, preProc=c("center", "scale"))
  saveRDS(modelLvq,file = glue("modellvq{expnum}.rds"))
  library(caret)
  final_predictionslvq <- predict(modelLvq, validationdata)
  # final_predictionsproblvq <- predict(modelLvq, newdata = validationdata, type = "prob")
  saveRDS(final_predictionslvq, file = glue("predlvq{expnum}.rds"))
  # saveRDS(final_predictionsproblvq, file = glue("predlvqprob{expnum}.rds"))
  cmlvq <- confusionMatrix(final_predictionslvq, validation$class)
  saveRDS(cmlvq, file = glue("cmlvq{expnum}.rds"))
  
  print("starting gbm")
  modelGbm <- train(class~., data=training, method="gbm", trControl=control, preProc=c("center", "scale"))
  saveRDS(modelGbm,file = glue("modelgbm{expnum}.rds"))
  final_predictionsGbm <- predict(modelGbm, validationdata)
  final_predictionsprobGbm <- predict(modelGbm, newdata = validationdata, type = "prob")
  saveRDS(final_predictionsGbm, file = glue("predGbm{expnum}.rds"))
  saveRDS(final_predictionsprobGbm, file = glue("predgbmprob{expnum}.rds"))
  cmGbm <- confusionMatrix(final_predictionsGbm, validation$class)
  saveRDS(cmGbm, file = glue("cmGbm{expnum}.rds"))
  
  print("starting svm")
  modelSvm <- train(class~., data=training, method="svmRadial", trControl=control, preProc=c("center", "scale"))
  saveRDS(modelSvm,file = glue("modelsvm{expnum}.rds"))
  final_predictionsSvm <- predict(modelSvm, newdata = validationdata)
  final_predictionsprobSvm <- predict(modelSvm, newdata = validationdata, type = "prob")
  saveRDS(final_predictionsSvm, file = glue("predSvm{expnum}.rds"))
  saveRDS(final_predictionsprobSvm, file = glue("predsvmprob{expnum}.rds"))
  cmSvm <- confusionMatrix(final_predictionsSvm, validation$class)
  saveRDS(cmSvm, file = glue("cmSvm{expnum}.rds"))
  
  print("starting rf")
  modelrf <- train(class~., data=training, method="rf", trControl=control, preProc=c("center", "scale"))
  saveRDS(modelrf,file = glue("modelrf{expnum}.rds"))
  final_predictionsrf <- predict(modelrf$finalModel, newdata = scale(validationdata))
  final_predictionsprobrf <- predict(modelrf$finalModel, newdata = scale(validationdata), type = "prob")
  saveRDS(final_predictionsrf, file = glue("predrf{expnum}.rds"))
  saveRDS(final_predictionsprobrf, file = glue("predrfprob{expnum}.rds"))
  cmrf <- confusionMatrix(final_predictionsrf, validation$class)
  saveRDS(cmrf, file = glue("cmrf{expnum}.rds"))
  
  models <- list(NB=modelnb, LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm,LDA=modellda, RF=modelrf)
  predTargets <- extractPrediction(models, testX = validationdata, testY = validation$class)
  modelsprob <- list(NB=modelnb, GBM=modelGbm, SVM=modelSvm,LDA=modellda, RF=modelrf)
  probTargets <- extractProb(modelsprob, testX = validationdata, testY = validation$class)
  caresults <- resamples(models)
  output <-list(caresults, predTargets, probTargets)
  return(output)
}

cagroup2output <- classifygroupprepropvalpred(group2, control, "group2newdata")
saveRDS(cagroup2output, file = "cagroup2output")

cagroup2algpat <- classifygroupprepropvalpred(group2algpat, control, "group2algpatwoval")
saveRDS(cagroup2algpat, file = "cagroup2algpatwovaloutput.rds")
cagroup2nonalg <- classifygroupprepropvalpred(group2nonalg, control, "group3nonalgwoval")
saveRDS(cagroup2nonalg, file = "cagroup3nonlagwovaloutput.rds")
cagroup2nonpat <- classifygroupprepropvalpred(group2nonpat, control, "group3nonpatwoval")
saveRDS(cagroup2nonpat, file = "cagroup3nonpatwovaloutput.rds")

cagroup3 <- classifygroupprepropvalpred(group3, control, "group3woval")
saveRDS(cagroup3, file = "cagroup3.rds")

cagroup3coor <- classifygroupprepropvalpred(groupcoor3pca, control, "group3coorwoval")
saveRDS(cagroup3coor, file = "cagroup3coor.rds")

cagroup355output <- classifygroupprepropvalpred(group355, control, "group355newdata")
saveRDS(cagroup355output, file = "cagroup355output")

cagroupalg355output <- classifygroupprepropvalpred(groupalg355, control, "groupalg355newdata")
saveRDS(cagroupalg355output, file = "cagroupalg355output")
