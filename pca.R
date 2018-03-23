pca355alg = prcomp(group355alg[2:66], scale. = T)
pca1657 = prcomp(group4[2:66], scale. = T)
pca355 = prcomp(group355[2:66], scale. = T)
pca2 = prcomp(group2[2:64], scale. =T)
pca3 = prcomp(group3[2:64], scale. =T)

pca355algl = as.data.frame(cbind(group355alg$id, pca355alg$x) )
pca1657l = as.data.frame(cbind(selepat1657$`newpat1657$id`, pca1657$x) )
pca355l = as.data.frame(cbind(group355$id, pca355$x) )
pca2l = as.data.frame(cbind(group2$`group2$id`, pca2$x) )
pca3l = as.data.frame(cbind(group3$`group3$id`, pca3$x) )

pcanon = prcomp(non1657, scale. = T)
# pcaran = prcomp(groupran[2:67], scale. = T)
pcaall = prcomp(groupall[2:66], scale. = T)
pca4 = prcomp(group4[2:67], scale. = T)
# pca300 = prcomp(group300[2:66], scale. = T)
# pca10 = prcomp(group10[2:66], scale. = T)
pca3 = prcomp(group3[2:64], scale. = T)
 
library(factoextra)
# fviz_contrib(pcajnon, choice = "var", axes = 1, top = 10)
# fviz_contrib(pcajnon, choice = "var", axes = 2, top = 10)
# fviz_pca_biplot(pcajnon, label="var", habillage = groupnon[,1], addEllipses = T, ellipse.level=0.95)
# 
# pcajran = prcomp(groupran[2:67], scale. = T)
# fviz_contrib(pcajran, choice = "var", axes = 1, top = 10)
# fviz_contrib(pcajran, choice = "var", axes = 2, top = 10)
# fviz_pca_biplot(pcajran, label="var", habillage = groupran[,1], addEllipses = T, ellipse.level=0.95)
# 
# fviz_contrib(pca300, choice = "var", axes = 1, top = 10)
# fviz_contrib(pca300, choice = "var", axes = 2, top = 10)
# fviz_pca_biplot(pca300, label="var", habillage = group300[,1], addEllipses = T, ellipse.level=0.95)
# fviz_pca_biplot(pca300, habillage = group300[,1])
# fviz_pca_ind(pca300, habillage = group300[,1])
# 
# fviz_pca_ind(pca4, label="var", habillage = group4[,1])
# fviz_pca_ind(pca10, label="var", habillage = group10[,1])

fviz_pca_biplot(pca3, label="var", habillage = pca3l[,1], addEllipses = T, ellipse.level=0.95)
fviz_pca_ind(pca3, label="var", habillage = group3$id)


X1657val4pca = selepatjsym(X1657val)
vm24pca = selepatjsym(vm2)
setdiff(colnames(X1657val4pca), colnames(vm24pca))
setdiff(colnames(vm24pca), colnames(X1657val4pca))
common <- intersect(colnames(vm24pca), colnames(X1657val4pca))  
vm24pca <- vm24pca[, common]
X1657val4pca <-X1657val4pca[,common] 
# ran4pca <- ran[, common]
non4pca <- non1657[, common]
mp4pca  <- mp[,common] 
scfp4pca <- scfp[,common] 
siaf14pca <- siaf1[,common] 
siar4pca <- siar[,common] 
siap4pca <- siap[,common] 
vm4pca  <- vm[,common] 

pcapat <- prcomp(X1657val4pca, scale. = T)
pcavm <- prcomp(vm24pca, scale. = T)

coorvm2 <- as.data.frame(scale(vm24pca, pcapat$center, pcapat$scale) %*% pcapat$rotation)
# coorran <- as.data.frame(scale(ran4pca, pcapat$center, pcapat$scale) %*% pcapat$rotation)
coornon <- as.data.frame(scale(non4pca, pcapat$center, pcapat$scale) %*% pcapat$rotation)
coormp <- as.data.frame(scale(mp4pca, pcapat$center, pcapat$scale) %*% pcapat$rotation)
coorsc <- as.data.frame(scale(scfp4pca, pcapat$center, pcapat$scale) %*% pcapat$rotation)
coorsir <- as.data.frame(scale(siar4pca, pcapat$center, pcapat$scale) %*% pcapat$rotation)
coorsip <- as.data.frame(scale(siap4pca, pcapat$center, pcapat$scale) %*% pcapat$rotation)
coorsif1 <- as.data.frame(scale(siaf14pca, pcapat$center, pcapat$scale) %*% pcapat$rotation)
coorvm <- as.data.frame(scale(vm4pca, pcapat$center, pcapat$scale) %*% pcapat$rotation)
scores <- as.data.frame(pcapat$x)

# library(ggplot2)
# ggplot(data = scores, alpha=0.1, col="black", aes(x = PC1, y = PC2, label=rownames(scores))) +
#   # geom_point(data = coorvm2,  alpha = 0.1, size = 1, col="blue", aes(x = PC1, y = PC2, label=rownames(coorvm2))) +
#   # geom_text(data = coorvm,  alpha = 0.1, size = 1, col="green", aes(x = PC1, y = PC2, label=rownames(coorvm))) +
#   # geom_text(data = coorvm2,  alpha = 0.1, size = 1, col="purple", aes(x = PC1, y = PC2, label=rownames(coorvm2))) +
#   # geom_text(data = coorsir,  alpha = 0.1, size = 1, col="yellow", aes(x = PC1, y = PC2, label=rownames(coorsir))) +
#   # geom_text(data = coorsip,  alpha = 0.1, size = 1, col="pink", aes(x = PC1, y = PC2, label=rownames(coorsip))) +
#   # geom_text(data = coorsif1,  alpha = 0.1, size = 1, col="navy", aes(x = PC1, y = PC2, label=rownames(coorsif1))) +
#   geom_text(data = coormp,  alpha = 0.5, size = 1, col="blue", aes(x = PC1, y = PC2, label=rownames(coormp))) +
#   # geom_text(data = coorsc,  alpha = 0.1, size = 1, col="orange", aes(x = PC1, y = PC2, label=rownames(coorsc))) +
#   geom_point(data = coorran,  alpha = 0.1, size = 1, col="red", aes(x = PC1, y = PC2, label=rownames(coorran))) +
#   geom_point(data = coornon,  alpha = 0.1, size = 1, col="green", aes(x = PC1, y = PC2, label=rownames(coornon))) +
#   geom_hline(yintercept = 0, colour = "gray65") +
#   geom_vline(xintercept = 0, colour = "gray65") +
#   geom_point(colour = "black", alpha = 0.1, size = 1) +
#   ggtitle("PCA")
