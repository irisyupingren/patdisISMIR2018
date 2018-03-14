library(mlbench)
library(caret)
library(glue)
control <- trainControl(method="repeatedcv", number=10, repeats=3)

classifygroup <- function(groupfile, control, expnum){
  colnames(groupfile)[1] <- "class"
  groupfile$class <- as.factor(groupfile$class)
  levels(groupfile$class) <- make.names(levels(factor(groupfile$class)))
  
  set.seed(7)
  print("starting lda")
  modellda <- train(class~., data=groupfile, method="lda", trControl=control)
  saveRDS(modellda,file = glue("modellda{expnum}.rds"))
  
  print("starting nb")
  modellda <- train(class~., data=groupfile, method = 'nb', trControl=control)
  saveRDS(modellda,file = glue("modelnb{expnum}.rds"))
  
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
  colnames(groupfile)[1] <- "class"
  groupfile$class <- as.factor(groupfile$class)
  levels(groupfile$class) <- make.names(levels(factor(groupfile$class)))
  
  set.seed(7)
  
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

classify3group <- function(groupfile, control, expnum){
  colnames(groupfile)[1] <- "class"
  groupfile$class <- as.factor(groupfile$class)
  levels(groupfile$class) <- make.names(levels(factor(groupfile$class)))
  
  set.seed(7)
  
  
  print("starting gbm")
  modelGbm <- train(class~., data=groupfile, method="gbm", trControl=control, verbose=FALSE)
  saveRDS(modelGbm,file = glue("modelgbm{expnum}.rds"))
  
  print("starting svm")
  modelSvm <- train(class~., data=groupfile, method="svmRadial", trControl=control)
  saveRDS(modelSvm,file = glue("modelsvm{expnum}.rds"))
  
  print("starting rf")
  modelrf <- train(class~., data=groupfile, method="rf", trControl=control)
  saveRDS(modelrf,file = glue("modelrf{expnum}.rds"))
  
  caresults <- resamples(list(GBM=modelGbm, SVM=modelSvm,
                              RF=modelrf))
  return(caresults)
}


# caranresults <- classifygroup(groupran,control)
# saveRDS(caranresults,"caranresults.rds")

caalgresults <- classifygroup(groupalgsample, control, "alg")
saveRDS(caalgresults, file="caalgresults.rds")

# ca2results <- classifygroup(group2, control,"group2")
# saveRDS(ca2results, file="ca2results.rds")

ca3results <- classifygroup(group3,control, "group3")
saveRDS(ca3results, file="ca3results2.rds")

# ca355algresults <- classifygroup(group355alg, control, "alg")
# saveRDS(ca355algresults, file="ca355algresults.rds")

ca355results <- classifygroup(group355,control, "355")
saveRDS(ca355results, file="ca355results2.rds")



# capca2results <- classifygroup(pca2l, control,"group2pca")
# saveRDS(capca2results, file="capca2results.rds")
# 
# capca3results <- classifygroup(pca3l,control,"group3pca")
# saveRDS(capca3results, file="capca3results.rds")

# capca355algresults <- classifygroup(pca355algl, control, "group355alg")
# capca355algresults <- classify4group(pca355algl, control, "group355alg")
# saveRDS(capca355algresults, file="capca355algresults.rds")

# capca355results <- classifygroup(pca355l,control,"group355")
# saveRDS(capca355results, file="capca355results.rds")

# capca1657results <- classifygroup(pca1657l,control, "group4pca")
# saveRDS(capca1657results, file="capca1657results.rds")
# 
# 
# cacoorpca3results <- readRDS("cacoorpca3results.rds")
# bwplot(cacoorpca3results)
# capca3results <- readRDS("capca3results.rds")
# bwplot(cap)

cacoorpca2results <- classify4group(groupcoor2pca, control,"groupcoor2pca")
saveRDS(cacoorpca2results, file="cacoorpca2results2.rds")
# 
cacoorpca3results <- classify4group(groupcoor3pca, control,"groupcoor3pca")
saveRDS(cacoorpca3results, file="cacoorpca3results2.rds")
# 
# cacoorpca4results <- classify4group(group4pca, control,"groupcoor4pca")
# saveRDS(cacoorpca4results, file="cacoorpca4results.rds")
# 
# cacoorpcaalg355results <- classifygroup(coorgroup355alg, control,"groupcooralg355pca")
# saveRDS(cacoorpcaalgresults, file="cacoorpcaalg355results.rds")

cacoorpca355results <- classifygroup(coorgroup355, control,"groupcooralgpca")
saveRDS(cacoorpca355results, file="cacoorpca355results2.rds")
# 
# cacoorpca1657results <- classifygroup(group1657pca, control,"groupcoor1657pca")
# saveRDS(cacoorpca1657results, file="cacoorpca1657results.rds")