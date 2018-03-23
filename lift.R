trellis.par.set(caretTheme())

lift_testing <- valgroup2newdata
## Generate the test set results
lift_results <- data.frame(Class = lift_testing$class)
svm_lift <- modelsvmgroup3newdata
rf_lift <- modelrfgroup3newdata
lda_lift <- modelldagroup3newdata

lift_results$SVM <- predict(svm_lift, lift_testing, type = "prob")[,"X1"]
lift_results$LDA <- predict(lda_lift, lift_testing, type = "prob")[,"X1"]
lift_results$RF <- predict(rf_lift, lift_testing, type = "prob")[,"X1"]
head(lift_results)


lift_obj <- lift(Class ~ SVM + LDA + RF, data = lift_results)
plot(lift_obj, values = 60, auto.key = list(columns = 3,
                                            lines = TRUE,
                                            points = FALSE))

ggplot(lift_obj, values = 90)

cal_obj <- calibration(Class ~ SVM + LDA + RF,
                       data = lift_results,
                       cuts = 13)

plot(cal_obj, type = "l", auto.key = list(columns = 3,
                                          lines = TRUE,
                                          points = FALSE))


ggplot(cal_obj)

library(ggplot2)
library(plotROC)
library(pROC)
selectedIndices <- rf_lift$pred$mtry == 2
plot.roc(rf_lift$pred$obs[selectedIndices],
         rf_lift$pred$M[selectedIndices])
