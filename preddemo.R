library(mlbench)
library(caret)
data(Satellite)
numSamples <- dim(Satellite)[1]
set.seed(716)

varIndex <- 1:numSamples

trainSamples <- sample(varIndex, 150)

varIndex <- (1:numSamples)[-trainSamples]
testSamples <- sample(varIndex, 100)

varIndex <- (1:numSamples)[-c(testSamples, trainSamples)]
unkSamples <- sample(varIndex, 50)

trainX <- Satellite[trainSamples, -37]
trainY <- Satellite[trainSamples, 37]

testX <- Satellite[testSamples, -37]
testY <- Satellite[testSamples, 37]

unkX <- Satellite[unkSamples, -37]

knnFit <- train(trainX, trainY, "knn")
rpartFit <- train(trainX, trainY, "rpart")

predict(knnFit)
predict(knnFit, newdata = testX)
predict(knnFit, newdata = testX, type = "prob")

bothModels <- list(
  knn = knnFit,
  tree = rpartFit)

predict(bothModels)

predTargets <- extractPrediction(
  bothModels,
  testX = testX,
  testY = testY,
  unkX = unkX)

predProb <- extractProb(
  bothModels,
  testX = testX,
  testY = testY,
  unkX = unkX)

