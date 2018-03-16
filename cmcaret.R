validation_index <- createDataPartition(group2$id, p=0.80, list=FALSE)
validation <- group2[-validation_index,]
training <- group2[validation_index,]


predictions <- predict(modelsvmgroupcoor355prepctrl)
group355$id <- as.factor(group355$id)
levels(group355$id) <- make.names(levels(factor(group355$id)))

cm <- confusionMatrix(predictions, group355$id)
