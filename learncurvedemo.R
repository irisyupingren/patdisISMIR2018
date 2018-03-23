set.seed(1412)
class_dat <- twoClassSim(1000)

set.seed(29510)
lda_data <- learing_curve_dat(dat = class_dat, 
                              outcome = "Class",
                              test_prop = 1/4, 
                              ## `train` arguments:
                              method = "lda", 
                              metric = "ROC",
                              trControl = trainControl(classProbs = TRUE, 
                                                       summaryFunction = twoClassSummary))


ggplot(lda_data, aes(x = Training_Size, y = ROC, color = Data)) + 
  geom_smooth(method = loess, span = .8) + 
  theme_bw()
