library(xtable)
library(ggplot2)
library(reshape2)

res1 <- colMeans(ca2resultsprepctrl$values[2:67])
# dupcol <- duplicated(res1)
# dedup <- res1[!dupcol]
evalmatrix <- matrix(res1, nrow=6)

colnames(evalmatrix)<-c(names(res1[1:11]))
rownames(evalmatrix)<-c("NB", "LVQ", "GBM","SVM","LDA","RF")
evalmatrix <- as.data.frame(evalmatrix)

print(xtable(as.matrix(colMeans(ca2resultsprepctrl$values[2:67])), type = "latex"), file = "filename2.tex")
print(xtable(as.matrix(evalmatrix), type = "latex", file = "filename2.tex"))

print(xtable(as.matrix(cmrfgroup2newdata$table), type = "latex", file = "filename2.tex"))
print(xtable(as.matrix(cmrfgroup355newdata$table), type = "latex"))
      
print(xtable(as.matrix(cmrfgroup3newdata$table), type = "latex"))

cm <- cmrfgroup3newdata$table
colnames(cm)<-c("Algorithms", "Nonpatterns", "AnnoPatterns")
rownames(cm)<-c("Algorithms", "Nonpatterns", "AnnoPatterns")
print(xtable(as.matrix(cm, type = "latex")))
      
cm <- cmrfgroup355newdata$table
colnames(cm)<-c("SIAF1", "SIAP", "SIAR", "VM2", "VM", "SC", "SIACFP", "Non", "Anno")
rownames(cm)<-c("SIAF1", "SIAP", "SIAR", "VM2", "VM", "SC", "SIACFP", "Non", "Anno")
print(xtable(as.matrix(cm, type = "latex")))
