library(ggplot2)
cols <- c("Alg"="blue", "Anno"="black", "Ran"="red", "Non=green")
p1 <- ggplot(data = scores, alpha=0.1, col="black", aes(x = PC1, y = PC2, label=rownames(scores))) +
  # geom_point(data = coorvm2,  alpha = 0.1, size = 1, col="blue", aes(x = PC1, y = PC2, label=rownames(coorvm2))) +
  # geom_text(data = coorvm,  alpha = 0.1, size = 1, col="green", aes(x = PC1, y = PC2, label=rownames(coorvm))) +
  # geom_text(data = coorvm2,  alpha = 0.1, size = 1, col="purple", aes(x = PC1, y = PC2, label=rownames(coorvm2))) +
  # geom_text(data = coorsir,  alpha = 0.1, size = 1, col="yellow", aes(x = PC1, y = PC2, label=rownames(coorsir))) +
  # geom_text(data = coorsip,  alpha = 0.1, size = 1, col="pink", aes(x = PC1, y = PC2, label=rownames(coorsip))) +
  # geom_text(data = coorsif1,  alpha = 0.1, size = 1, col="navy", aes(x = PC1, y = PC2, label=rownames(coorsif1))) +
  geom_point(data = coormp,  alpha = 0.5, size = 1, col="blue", aes(x = PC1, y = PC2, label=rownames(coormp))) +
  # geom_text(data = coorsc,  alpha = 0.1, size = 1, col="orange", aes(x = PC1, y = PC2, label=rownames(coorsc))) +
  geom_point(data = coorran,  alpha = 0.1, size = 1, col="red", aes(x = PC1, y = PC2, label=rownames(coorran))) +
  geom_point(data = coornon,  alpha = 0.1, size = 1, col="green", aes(x = PC1, y = PC2, label=rownames(coornon))) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_point(colour = "black", alpha = 0.1, size = 1) +
  theme(legend.position="bottom") +
  ggtitle("PCA of Ran.Non project onto the pca feature space of the annotations")

# dev.off()
p2<- ggplot(data = scores, alpha=0.1, col="black", aes(x = PC1, y = PC2, label=rownames(scores), colour="Alg")) +
  geom_point(data = coorvm2,  alpha = 0.1, size = 1, col="blue", aes(x = PC1, y = PC2, label=rownames(coorvm2), colour="Alg")) +
  geom_point(data = coorvm,  alpha = 0.1, size = 1, col="green", aes(x = PC1, y = PC2, label=rownames(coorvm))) +
  # geom_text(data = coorvm2,  alpha = 0.1, size = 1, col="purple", aes(x = PC1, y = PC2, label=rownames(coorvm2))) +
  geom_point(data = coorsir,  alpha = 0.1, size = 1, col="yellow", aes(x = PC1, y = PC2, label=rownames(coorsir))) +
  # geom_text(data = coorsip,  alpha = 0.1, size = 1, col="pink", aes(x = PC1, y = PC2, label=rownames(coorsip))) +
  geom_point(data = coorsif1,  alpha = 0.1, size = 1, col="navy", aes(x = PC1, y = PC2, label=rownames(coorsif1))) +
  # geom_text(data = coormp,  alpha = 0.5, size = 1, col="blue", aes(x = PC1, y = PC2, label=rownames(coormp))) +
  # geom_text(data = coorsc,  alpha = 0.1, size = 1, col="orange", aes(x = PC1, y = PC2, label=rownames(coorsc))) +
  # geom_point(data = coorran,  alpha = 0.1, size = 1, col="red", aes(x = PC1, y = PC2, label=rownames(coorran))) +
  # geom_point(data = coornon,  alpha = 0.1, size = 1, col="green", aes(x = PC1, y = PC2, label=rownames(coornon))) +
  # geom_hline(yintercept = 0, colour = "gray65") +
  # geom_vline(xintercept = 0, colour = "gray65") +
  scale_colour_manual(name="legend", values=cols) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10)) + 
  theme(axis.title.y = element_text(size = 10)) + 
  geom_point(colour = "black", alpha = 0.1, size = 1) +
  ggtitle("PCA of 3 algorithms project onto the pca space of annotation")
# print(p2)

library(dplyr)
library(plyr)
df <- bind_rows(scores, coornon, coormp, coorvm, coorvm2, coorsif1, coorsip, coorsir, coorsc, .id="id")
maps <- revalue(df$id, 
        c("1"="Anno", 
          "2"="Non",
          "3"="MP",
          "4"="VM",
          "5"="VM2",
          "6"="SIAF1",
          "7"="SIAP",
          "8"="SIAR",
          "9"="SIACFP"))
df <- cbind(maps, df)
df$id <- as.numeric(df$id)

df1 <- df[df$id<3,]
p1 <- ggplot(df1, aes(x=PC1, y=PC2, shape=maps, colour=maps)) + 
  geom_point(position=position_jitter(w=0.04,h=0.02), size=1.5,  alpha = 0.5)  +
  theme_bw()+
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(panel.grid = element_blank(), panel.border = element_blank()) +
  theme(legend.position="top", legend.direction="horizontal") +
  scale_fill_discrete("") +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=20))
  
df$maps <- factor(df$maps)
df2 <- df
p2 <- ggplot(df2, aes(x=PC1, y=PC2, shape=maps, colour=maps)) + 
  scale_shape_manual(values=1:nlevels(df2$maps)) +
  geom_point(position=position_jitter(w=0.04,h=0.02), size=1.5,  alpha = 0.5) +
  theme_bw()+
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(panel.grid = element_blank(), panel.border = element_blank()) +
  theme(legend.position="top", legend.direction="horizontal") +
  scale_fill_discrete("") +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=13))

df3 <- df[df$maps== "MP" | df$maps== "Anno",]
p3 <- ggplot(df3, aes(x=PC1, y=PC2, shape=maps, colour=maps)) + 
  geom_point(position=position_jitter(w=0.04,h=0.02), size=1.5,  alpha = 0.5) +
  theme_bw()+
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(panel.grid = element_blank(), panel.border = element_blank()) +
  theme(legend.position="top", legend.direction="horizontal") +
  scale_fill_discrete("") +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=20))

df4 <- df[df$maps== "SIAR" | df$maps== "Anno",]
p4 <- ggplot(df4, aes(x=PC1, y=PC2, shape=maps, colour=maps)) + 
  geom_point(position=position_jitter(w=0.04,h=0.02), size=1.5,  alpha = 0.5) +
  theme_bw()+
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(panel.grid = element_blank(), panel.border = element_blank()) +
  theme(legend.position="top", legend.direction="horizontal") +
  scale_fill_discrete("") +
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=20))
#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


mylegend<-g_legend(p2)

p5 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                               p2 + theme(legend.position="none"),
                               p3 + theme(legend.position="none"),
                               p4 + theme(legend.position="none"),
                               nrow=2, ncol=3),
                   mylegend)

multiplot(p1,p3,p4,p2,cols=2)
