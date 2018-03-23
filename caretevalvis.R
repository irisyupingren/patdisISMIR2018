res1 <- colMeans(ca2resultsprepctrl$values[2:67])
# dupcol <- duplicated(res1)
# dedup <- res1[!dupcol]
evalmatrix <- matrix(res1, nrow=6)

colnames(evalmatrix)<-c(names(res1[1:11]))
rownames(evalmatrix)<-c("NB", "LVQ", "GBM","SVM","LDA","RF")
evalmatrix <- as.data.frame(evalmatrix)


p <- ggplot(data=evalmatrix, aes(x = species_id, y = weight)) + 
  geom_boxplot()
print(p)

library(colorspace) # get nice colors
m <- rep((1:6), each=1)
mcol <- rev(rainbow_hcl(6))[as.numeric(m)]
MASS::parcoord(evalmatrix, col=mcol, var.label = TRUE, lwd = 2)

dev.off()
nn <- ncol(evalmatrix)
matplot(t(evalmatrix), type='b', lty = 'solid',axes=F)
legend("top", colnames(evalmatrix), col=seq_len(nn), cex=0.8, fill=seq_len(nn), legend=c("NB", "LVQ", "GBM","SVM","LDA","RF"))
axis(1, at = 1:11, labels =names(res1[1:11]), cex.axis = 0.7)
axis(2)