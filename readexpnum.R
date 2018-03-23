library(caret)
library(glue)

readmodels <- function(expnum){
  print("reading lda")
  modellda <- readRDS(glue("modellda{expnum}.rds"))
  final_predictionsproblda <- readRDS(glue("predldaprob{expnum}.rds"))
  cmlda <- readRDS(glue("cmlda{expnum}.rds"))
  
  print("reading nb")
  modelnb <- readRDS(glue("modelnb{expnum}.rds"))
  final_predictionsprobnb <- readRDS(glue("prednbprob{expnum}.rds"))
  cmnb <- readRDS(glue("cmnb{expnum}.rds"))
  
  print("reading gbm")
  modelgbm <- readRDS(glue("modelgbm{expnum}.rds"))
  final_predictionsprobgbm <- readRDS(glue("predgbmprob{expnum}.rds"))
  cmgbm <- readRDS(glue("cmgbm{expnum}.rds"))
  
  print("reading svm")
  modelsvm <- readRDS(glue("modelsvm{expnum}.rds"))
  final_predictionsprobsvm <- readRDS(glue("predsvmprob{expnum}.rds"))
  cmsvm <- readRDS(glue("cmsvm{expnum}.rds"))
  
  print("reading lvq")
  print("model")
  modellvq <- readRDS(glue("modellvq{expnum}.rds"))
  # print("probpred")
  # final_predictionsproblvq <- readRDS(glue("predlvqprob{expnum}.rds"))
  print("cm")
  cmlvq <- readRDS(glue("cmlvq{expnum}.rds"))
  
  print("reading rf")
  modelrf <- readRDS(glue("modelrf{expnum}.rds"))
  final_predictionsprobrf <- readRDS(glue("predrfprob{expnum}.rds"))
  cmrf <- readRDS(glue("cmrf{expnum}.rds"))
  
  save(modelnb, final_predictionsprobnb, cmnb,
       modellvq, cmlvq,
       modelgbm, final_predictionsprobgbm, cmgbm,
       modelsvm, final_predictionsprobsvm, cmsvm,
       modellda, final_predictionsproblda, cmlda,
       modelrf, final_predictionsprobrf, cmrf,
       file=glue("{expnum}.RData"))
  
  # cmoutput <-list(cmnb, cmlvq, cmgbm, cmsvm, cmlda, cmrf)
  # return(output)
}

readmodels("coorgroup2algpatnewdata")
readmodels("group2algpatnewdata")
readmodels("group355newdatacoor")
readmodels("groupalg355newdatacoor")
readmodels("group355newdata")

readmodels("group3newdata")
readmodels("group3newdatacoor")

readmodels("group2newdata")
readmodels("group2newdatacoor")

readmodels("groupalg355newdata")
readmodels("groupalg355newdatacoor")
