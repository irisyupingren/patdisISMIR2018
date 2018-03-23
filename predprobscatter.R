library(plotly)

groupcoor3pca$id[which(groupcoor3pca$id == 1)] <- 'Non'
groupcoor3pca$id[which(groupcoor3pca$id == 2)] <- 'Alg'
groupcoor3pca$id[which(groupcoor3pca$id == 3)] <- 'Anno'
groupcoor3pca$id <- as.factor(groupcoor3pca$id)

p <- plot_ly(groupcoor3pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~id, colors = c('#BF382A', '#0C4B8E','#0C2222')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
Sys.setenv("plotly_username"="musicpatISMIR2018")
Sys.setenv("plotly_api_key"="aYOD8HwFqVwd0bZHthhR")


chart_link =api_create(p, filename="pca")
chart_link
