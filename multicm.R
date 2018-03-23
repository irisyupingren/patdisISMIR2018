library(mlbench)
library(caret)
library(glue)

control <- trainControl(method="repeatedcv", verboseIter = TRUE, summaryFunction = multiClassSummary, classProbs = T, savePredictions = T, number=10, repeats=3)
# control <- trainControl(method="repeatedcv", preProcOptions = list(thresh=0.8), verboseIter = TRUE, summaryFunction = multiClassSummary, classProbs = T, savePredictions = T, number=10, repeats=3)

classifymulti <- function(groupfile, control, descri, expnum){
  outputarray <- c()
  cmarray <- c()
  
  colnames(groupfile)[1] <- "class"
  groupfile$class <- as.factor(groupfile$class)
  levels(groupfile$class) <- make.names(levels(factor(groupfile$class)))
  
  training <- groupfile
  validation <- groupfile
  validationdata <- groupfile[2:64]
  
  nfold = expnum
  for ( i in 1:nfold){
    set.seed(i)
    print("starting lda")
    modellda <- caret::train(class~., data=training, method="lda", trControl=control, preProc=c("center", "scale"))
    final_predictionslda <- predict(modellda, validationdata)
    # saveRDS(final_predictionsproblda, file = glue("predlda{descri}-{descri}-{i}.rds"))
    final_predictionsproblda <- predict(modellda, newdata = validationdata, type = "prob")
    saveRDS(final_predictionsproblda, file = glue("predldaprob{descri}-{i}.rds"))
    cmlda <- confusionMatrix(final_predictionslda, validation$class)
    saveRDS(modellda, file = glue("modellda{descri}-{i}.rds"))
    saveRDS(final_predictionslda, file = glue("predlda{descri}-{i}.rds"))
    saveRDS(cmlda, file = glue("cmlda{descri}-{i}.rds"))
    # 
    print("starting nb")
    modelnb <- train(class~., data=training, method = 'nb', trControl=control, preProc=c("center", "scale"))
    saveRDS(modelnb,file = glue("modelnb{descri}-{i}.rds"))
    final_predictionsnb <- predict(modelnb, validationdata)
    final_predictionsprobnb <- predict(modelnb, newdata = validationdata, type = "prob")
    saveRDS(final_predictionsnb, file = glue("prednb{descri}-{i}.rds"))
    saveRDS(final_predictionsprobnb, file = glue("prednbprob{descri}-{i}.rds"))
    cmnb <- confusionMatrix(final_predictionsnb, validation$class)
    saveRDS(cmnb, file = glue("cmnb{descri}-{i}.rds"))
    
    print("starting lvq")
    modelLvq <- train(class~., data=training, method="lvq", trControl=control, preProc=c("center", "scale"))
    saveRDS(modelLvq,file = glue("modellvq{descri}-{i}.rds"))
    library(caret)
    final_predictionslvq <- predict(modelLvq, validationdata)
    # final_predictionsproblvq <- predict(modelLvq, newdata = validationdata, type = "prob")
    saveRDS(final_predictionslvq, file = glue("predlvq{descri}-{i}.rds"))
    # saveRDS(final_predictionsproblvq, file = glue("predlvqprob{descri}-{i}.rds"))
    cmlvq <- confusionMatrix(final_predictionslvq, validation$class)
    saveRDS(cmlvq, file = glue("cmlvq{descri}-{i}.rds"))
    
    print("starting gbm")
    modelGbm <- train(class~., data=training, method="gbm", trControl=control, preProc=c("center", "scale"))
    saveRDS(modelGbm,file = glue("modelgbm{descri}-{i}.rds"))
    final_predictionsGbm <- predict(modelGbm, validationdata)
    final_predictionsprobGbm <- predict(modelGbm, newdata = validationdata, type = "prob")
    saveRDS(final_predictionsGbm, file = glue("predGbm{descri}-{i}.rds"))
    saveRDS(final_predictionsprobGbm, file = glue("predgbmprob{descri}-{i}.rds"))
    cmGbm <- confusionMatrix(final_predictionsGbm, validation$class)
    saveRDS(cmGbm, file = glue("cmGbm{descri}-{i}.rds"))
    
    print("starting svm")
    modelSvm <- train(class~., data=training, method="svmRadial", trControl=control, preProc=c("center", "scale"))
    saveRDS(modelSvm,file = glue("modelsvm{descri}-{i}.rds"))
    final_predictionsSvm <- predict(modelSvm, newdata = validationdata)
    final_predictionsprobSvm <- predict(modelSvm, newdata = validationdata, type = "prob")
    saveRDS(final_predictionsSvm, file = glue("predSvm{descri}-{i}.rds"))
    saveRDS(final_predictionsprobSvm, file = glue("predsvmprob{descri}-{i}.rds"))
    cmSvm <- confusionMatrix(final_predictionsSvm, validation$class)
    saveRDS(cmSvm, file = glue("cmSvm{descri}-{i}.rds"))
    
    print("starting rf")
    modelrf <- train(class~., data=training, method="rf", trControl=control, preProc=c("center", "scale"))
    saveRDS(modelrf,file = glue("modelrf{descri}-{i}.rds"))
    final_predictionsrf <- predict(modelrf$finalModel, newdata = scale(validationdata))
    final_predictionsprobrf <- predict(modelrf$finalModel, newdata = scale(validationdata), type = "prob")
    saveRDS(final_predictionsrf, file = glue("predrf{descri}-{i}.rds"))
    saveRDS(final_predictionsprobrf, file = glue("predrfprob{descri}-{i}.rds"))
    cmrf <- confusionMatrix(final_predictionsrf, validation$class)
    saveRDS(cmrf, file = glue("cmrf{descri}-{i}.rds"))
    
    models <- list(NB=modelnb, LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm,LDA=modellda, RF=modelrf)
    modelsprob <- list(NB=modelnb, GBM=modelGbm, SVM=modelSvm,LDA=modellda, RF=modelrf)
    predTargets <- extractPrediction(models, testX = validationdata, testY = validation$class)
    probTargets <- extractProb(modelsprob, testX = validationdata, testY = validation$class)
    caresults <- resamples(models)
    cmlist <-list(cmnb, cmlvq, cmGbm, cmSvm, cmlda, cmrf)
    output <- list(models, predTargets, probTargets, caresults, cmlist)
    saveRDS(output, file = glue("output{descri}-{i}.rds"))
  }
  
  outputarray[[i]] <- output
  cmarray[[i]] <- cmlist
  finaloutput <- list(outputarray, cmarray)
  return(finaloutput)
}

# readmodels("group3woval")
cagroup3allnon5dedup <- classifymulti(group3allnon5dedup, control, "non5dedup", 10)
saveRDS(cagroup3allnon5dedup, file="cagroup3allnon5dedup.rds")
cagroup3allnon4dedup <- classifymulti(group3allnon4dedup, control, "non4dedup", 10)
saveRDS(cagroup3allnon4dedup, file="cagroup3allnon4dedup.rds")
cagroup3allnon3dedup <- classifymulti(group3allnon3dedup, control, "non3dedup", 10)
saveRDS(cagroup3allnon3dedup, file="cagroup3allnon3dedup.rds")
cagroup3allnon2dedup <- classifymulti(group3allnon2dedup, control, "non2dedup", 10)
saveRDS(cagroup3allnon2dedup, file="cagroup3allnon2dedup.rds")
cagroup3allnon1dedup <- classifymulti(group3allnon1dedup, control, "non1dedup", 10)
saveRDS(cagroup3allnon1dedup, file="cagroup3allnon1dedup.rds")

library(parallel)
install.packages("beepr")
library(beepr)
beep()
library(mail)
sendmail("yuping.ren.iris@gmail.com", subject="Finished", message = "non sample groups finished", password = "rmail")

cagroup3allnon5 <- classifymulti(group3allnon5, control, "non5", 10)
saveRDS(cagroup3allnon5, file="cagroup3allnon5.rds")
cagroup3allnon4 <- classifymulti(group3allnon4, control, "non4", 10)
saveRDS(cagroup3allnon4, file="cagroup3allnon4.rds")
cagroup3allnon3 <- classifymulti(group3allnon3, control, "non3", 10)
saveRDS(cagroup3allnon3, file="cagroup3allnon3.rds")
cagroup3allnon2 <- classifymulti(group3allnon2, control, "non2", 10)
saveRDS(cagroup3allnon2, file="cagroup3allnon2.rds")
cagroup3allnon1 <- classifymulti(group3allnon1, control, "non1", 10)
saveRDS(cagroup3allnon1, file="cagroup3allnon1.rds")

cagroup3 <- classifymulti(group3, control, "normal1657", 10)
saveRDS(cagroup3, file = "caroup3multi.rds")

cagroup3dedup <- classifymulti(dedupgroup3, control, "dedup", 10)
saveRDS(cagroup3dedup, file = "cagroup3multidedup.rds")

cagroup3dedupbalan <- classifymulti(dedupngroup3balan, control, "dedupbalan", 10)
saveRDS(cagroup3dedupbalan, file = "cagroup3multidedupbalan.rds")

cagroup3coor <- classifygroupprepropvalpred(groupcoor3pca, control, "group3coorwoval")
saveRDS(cagroup3coor, file = "cagroup3coor.rds")

