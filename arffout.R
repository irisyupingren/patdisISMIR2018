library(dplyr)
library(lubridate)
library(foreign)  

groupnon %>% glimpse()

makeclass <- function(groupfile){
  colnames(groupfile)[1] <- "class"
  groupfile$class <- as.factor(groupfile$class)
  levels(groupfile$class) <- make.names(levels(factor(groupfile$class)))
  return(groupfile)
}

groupnon <- makeclass(groupnon)
write.arff(groupnon, file='groupnon.arff')

groupran <- makeclass(groupran)
write.arff(groupran, file='groupran.arff')

groupall <- makeclass(groupall)
write.arff(groupall, file='groupall.arff')

group10 <- makeclass(group10)
write.arff(group10, file="group10.arff")

groupsamp <- makeclass(selepat100)
write.arff(groupsamp, file="groupsamp.arff")

group300 <- makeclass(selepat300)
write.arff(groupsamp, file="group300.arff")

group4 <- makeclass(selepat1657)
write.arff(group4, file="group4.arff")
