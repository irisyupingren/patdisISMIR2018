library(caret)
library(C50)
set.seed(2)
lift_training <- twoClassSim(1000)
lift_testing  <- twoClassSim(1000)

ctrl <- trainControl(method = "cv", classProbs = TRUE,
                     summaryFunction = twoClassSummary)

set.seed(1045)
fda_lift <- train(Class ~ ., data = lift_training,
                  method = "fda", metric = "ROC",
                  tuneLength = 20,
                  trControl = ctrl)
set.seed(1045)
lda_lift <- train(Class ~ ., data = lift_training,
                  method = "lda", metric = "ROC",
                  trControl = ctrl)

set.seed(1045)
c5_lift <- train(Class ~ ., data = lift_training,
                 method = "C5.0", metric = "ROC",
                 tuneLength = 10,
                 trControl = ctrl,
                 control = C5.0Control(earlyStopping = FALSE))

## Generate the test set results
lift_results <- data.frame(Class = lift_testing$Class)
lift_results$FDA <- predict(fda_lift, lift_testing, type = "prob")[,"Class1"]
lift_results$LDA <- predict(lda_lift, lift_testing, type = "prob")[,"Class1"]
lift_results$C5.0 <- predict(c5_lift, lift_testing, type = "prob")[,"Class1"]
head(lift_results)

trellis.par.set(caretTheme())
lift_obj <- lift(Class ~ FDA + LDA + C5.0, data = lift_results)
plot(lift_obj, values = 60, auto.key = list(columns = 3,
                                            lines = TRUE,
                                            points = FALSE))

ggplot(lift_obj, values = 60)

trellis.par.set(caretTheme())
cal_obj <- calibration(Class ~ FDA + LDA + C5.0,
                       data = lift_results,
                       cuts = 13)
plot(cal_obj, type = "l", auto.key = list(columns = 3,
                                          lines = TRUE,
                                          points = FALSE))

ggplot(cal_obj)

# test <- predict(fda_lift, lift_testing, type = "prob")
