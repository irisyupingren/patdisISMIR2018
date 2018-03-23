colnames(group2)[1] <- "class"
group2$class <- as.factor(group2$class)
levels(group2$class) <- make.names(levels(factor(group2$class)))
class_dat <- group2
class_dat[,32] <- NULL

lda_data <- learing_curve_dat(dat = class_dat, 
                              outcome = "class",
                              test_prop = 0.2, 
                              ## `train` arguments:
                              method = "rf", 
                              trControl = trainControl(classProbs = TRUE, 
                                                       summaryFunction = twoClassSummary))


ggplot(lda_data, aes(x = Training_Size, y = ROC, color = Data)) + 
  geom_smooth(method = loess, span = .8) + 
  theme_bw()


control <- trainControl(method="repeatedcv", verboseIter = TRUE, summaryFunction = multiClassSummary, classProbs = T, savePredictions = T, number=10, repeats=3)
lc <- learing_curve_dat(dat = class_dat, 
                              outcome = "class",
                              test_prop = 0.2, 
                              ## `train` arguments:
                              method = "rf", 
                              trControl = control)

ggplot(lc, aes(x = Training_Size, y = F1, color = Data)) + 
  geom_smooth(method = loess, span = .8) + 
  theme_bw()
