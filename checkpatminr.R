patminr <- selepatjsym(COSIA[[5]])
patminr <- selepatjsym(patminr)
pcapatminr = prcomp(patminr, scale. = T)

library(factoextra)
fviz_contrib(pcapatminr, choice = "var", axes = 1, top = 10)
fviz_contrib(pcapatminr, choice = "var", axes = 2, top = 10)
fviz_pca_biplot(pcapatminr, label="var")

common <- intersect(colnames(patminr), colnames(selepatjsym(X1657val)))

pcapat <- prcomp(selepatjsym(X1657val)[,common], scale.=T)

coor <- as.data.frame(scale(patminr[,common], pcapat$center, pcapat$scale) %*% pcapat$rotation)

scores <- as.data.frame(pcapat$x)

library(ggplot2)
ggplot(data = scores, aes(x = PC1, y = PC2, label=rownames(scores))) +
  geom_point(data = coor,  alpha = 0.3, size = 2, col="blue", aes(x = PC1, y = PC2, label=rownames(coor))) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(colour = "black", alpha = 0.3, size = 3) +
  ggtitle("PCA")

fviz_contrib(pcapat, choice = "var", axes = 1, top = 10)
fviz_contrib(pcapat, choice = "var", axes = 2, top = 10)
fviz_pca_biplot(pcapat, label="var")
