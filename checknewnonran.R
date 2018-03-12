source("./utilfuncs.R")
library(dplyr)

groupnon <- bind_rows(non,non2,non3,non4,non5,non6, .id="id")
groupnon <- selectjsym(groupnon)

groupran <- bind_rows(ran,ran2,ran3,ran4,ran5,ran6, .id="id")
groupran <- selectjsym(groupran)

pcanon = prcomp(groupnon[2:65], scale. = T)
pcaran = prcomp(groupran[2:69], scale. = T)

library(factoextra)
fviz_contrib(pcanon, choice = "var", axes = 1, top = 10)
fviz_contrib(pcanon, choice = "var", axes = 2, top = 10)
fviz_pca_biplot(pcanon, label="var", habillage = groupnon[,1], addEllipses = T, ellipse.level=0.95)

