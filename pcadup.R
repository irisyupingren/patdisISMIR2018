
pcapat <- prcomp(pat1657, scale. = T)
pcapat1287 <- prcomp(pat1287, scale. = T)

coornon <- as.data.frame(scale(non1657, pcapat$center, pcapat$scale) %*% pcapat$rotation)
coor

scores <- as.data.frame(pcapat$x)
scores1287 <- as.data.frame(pcapat1287$x)

groupcoor2pca <- bind_rows(coornon, scores, .id="id")