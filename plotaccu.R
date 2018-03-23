cagroup3copy <- cagroup3
cagroup3coorcopy <- cagroup3coor
# cagroup3copy[[1]]$values[!colnames(copyval[c(2,13,24,35,46,57)])]
library(dplyr)
# cagroup3copy[[1]]$values[,!colnames(copyval[c(2,13,24,35,46,57)])] <- NULL
cagroup3copy[[1]]$values %>% select("NB~Accuracy", "LVQ~Accuracy", "GBM~Accuracy", "SVM~Accuracy", "LDA~Accuracy","RF~Accuracy" )
values <- cagroup3copy[[1]]$values %>% select("Resample","NB~Accuracy", "LVQ~Accuracy", "GBM~Accuracy", "SVM~Accuracy", "LDA~Accuracy","RF~Accuracy" )
cagroup3copy[[1]]$values <- values

values <- cagroup3coorcopy[[1]]$values %>% select("Resample","NB~Accuracy", "LVQ~Accuracy", "GBM~Accuracy", "SVM~Accuracy", "LDA~Accuracy","RF~Accuracy" )
cagroup3coorcopy[[1]]$values <- values

accu <- bind_rows(cagroup3copy[[1]]$values, cagroup3coorcopy[[1]]$values, .id="id")
library(plyr)
accu$id <- mapvalues(accu$id, from = c("1", "2"), to = c("Raw", "PCA"))
colnames(accu) <- c("Feat","Resample","NB", "LVQ", "GBM", "SVM", "LDA","RF" )
library(reshape2)
accumelt <- melt(accu)
colnames(accumelt) <- c("Raw.PCA", "Resample","Classifier","Accuracy")
write.csv(accumelt, file="accumelt.csv")

library(ggplot2)
library(ggthemes)
p<-ggplot(accumelt, aes(x=Classifier, y=Accuracy, fill=Raw.PCA)) +
  geom_boxplot(position=position_dodge(width = 0),lwd=0.1,alpha = 0.3)
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.1,position=position_dodge(width = 0), binwidth=0.0001) + 
  scale_fill_manual(values = c("red","blue"),labels = c("PCA","Raw"),name = "PCA/RAW:")+
  theme_bw()+
  theme(axis.text=element_text(size=25),axis.title=element_text(size=20,face="bold"), panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.title = element_text(size=25))+
  theme(legend.text = element_text(size =25))+
  # theme(legend.title = element_text(size=25, face="bold"))+
  # theme(legend.text = element_text(size =25, face = "bold"))+
  theme(legend.position="top")
  # scale_fill_brewer(palette="Blues") + 
  # scale_fill_manual(values=c("blue","green","red","cyan","darkblue","darkgreen","darkred","darkcyan","darkblue","darkgreen")) +
  # theme_classic()
  # theme_solarized()
  # theme_tufte()
  # theme_fivethirtyeight()
  # theme_few()
  # theme_economist()
  # theme_wsj()
  # theme_calc()
  # theme_base()
  # theme_pander()

library(caret)
par(col.lab = "black")

bwplot(cagroup3copy[[1]],scales=list(x=list(cex=2), y=list(cex=2)), ylab=list("Classifiers", cex=1.5))
bwplot(cagroup3copy[[1]],scales=list(x=list(cex=1.5), y=list(cex=1.5)), ylab=list("Classifiers", cex=1.5))

cagroup3copy[[1]]$models
