draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  

library(caret)
modelrf <- train(id~., data=group355, method="rf", trControl=control)
modelrf<- readRDS("modelrf355.rds")

cm <- confusionMatrix(modelrf$pred[order(modelrf$pred$rowIndex),2], )

draw_confusion_matrix(cm)

dev.off()
cm <- as.matrix(read_csv("Data/cm.csv"))
cm <- as.matrix(read_csv("Data/cmworan.csv"))
colnames(cm) <- c("SIAF1", "SIAP", "SIAR", "VM2", "VM", "MP", "SCFP", "Non", "Ran", "Pat")
rownames(cm) <- c("SIAF1", "SIAP", "SIAR", "VM2", "VM", "MP", "SCFP", "Non", "Ran", "Pat")
colnames(cm) <- c("SIAF1", "SIAP", "SIAR", "VM2", "VM", "MP", "SCFP", "Non", "Pat")
rownames(cm) <- c("SIAF1", "SIAP", "SIAR", "VM2", "VM", "MP", "SCFP", "Non", "Pat")
library(pheatmap)
shadesOfGrey <- colorRampPalette(c("grey0", "grey100"))
fiftyGreys <- rev(shadesOfGrey(50))
pheatmap(cm, cluster_rows = F, cluster_cols=F, fontsize_col=20, fontsize_row=20,color = fiftyGreys)


heatmap(cm)

cm <-as.data.frame(cm)
library(ggplot2)
library(plyr)
nba.m <- ddply(cm, .(variable), transform,rescale = rescale(value))
p <- ggplot(cm, aes(variable, Name)) + 
    geom_tile(aes(fill = rescale),colour = "white") + 
  scale_fill_gradient(low = "white",high = "steelblue")

base_size <- 9
p + theme_grey(base_size = base_size) + labs(x = "",y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  opts(legend.position = "none",axis.ticks = theme_blank(), axis.text.x = theme_text(size = base_size *
                                                                                                               +         0.8, angle = 330, hjust = 0, colour = "grey50"))