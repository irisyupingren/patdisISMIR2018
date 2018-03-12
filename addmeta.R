library(dplyr)
splitted <- t(sapply(strsplit(X1657val$X1,split="NLB"), unlist))
motifidfea <- substring(splitted[,2], 1, 12)
X1657val$motifid <- motifidfea

motifidraw <- substring(rawdata$motifid, 4)
# raw6 <- rawdata %>% select("tunefamily", "songid", "description", "annotator", "motifclass")
# raw6 <- cbind(raw4, motifidraw)

raw4 <- rawdata %>% select("tunefamily", "description", "annotator")
raw4 <- cbind(raw4, motifidraw)

merged <- merge(raw4, X1657val, by.x="motifidraw", by.y="motifid")

splitted <- t(sapply(strsplit(non2$X1,split="patmidi2/"), unlist))
num <- as.numeric(substring(splitted[,2],1,4))
non2$num <- num
nonorder <- non2[order(non2$num),]
nonnames2 <- read.csv("Data/nonnames2.txt", header=FALSE)
nonnames2$V3 <- substring(nonnames2$V3,2,13)
nonorder$motifid <- as.character(nonnames2$V3[2:1657])
raw4$motifid <- as.character(raw4$motifidraw)
raw4$motifidraw <- NULL
nonmerged <- merge(raw4, nonorder, by.x="motifid", by.y="motifid")

library(glue)
rannonmeta <- function(non2, raw4){
  splitted <- t(sapply(strsplit(non2$X1,split="patmidi3/"), unlist))
  num <- as.numeric(substring(splitted[,2],1,4))
  non2$num <- num
  nonorder <- non2[order(non2$num),]
  nonnames2 <- read.csv(glue("Data/rannames3.txt"), header=FALSE)
  nonnames2$V3 <- substring(nonnames2$V3,2,13)
  nonorder$motifid <- as.character(nonnames2$V3[2:1657])
  # raw4$motifid <- as.character(raw4$motifidraw)
  # raw4$motifidraw <- NULL
  nonmerged <- merge(raw4, nonorder, by.x="motifid", by.y="motifid")
  return(nonmerged)
}
ran3merged <- rannonmeta(ran3,raw4)
