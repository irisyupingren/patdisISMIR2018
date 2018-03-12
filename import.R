library(readr)
non <- read_csv("Data/nonval.csv")
non3 <- read_csv("Data/nonval3.csv")
non4 <- read_csv("Data/nonval4.csv")
non5 <- read_csv("Data/nonval5.csv")
non2 <- read_csv("Data/nonval2.csv")

ran <- read_csv("Data/ranval.csv")
ran3 <- read_csv("Data/ran2val.csv")
ran4 <- read_csv("Data/ran4val.csv")
ran5 <- read_csv("Data/ran5val.csv")
ran2 <- read_csv("Data/ran3val.csv")

scfp <- read_csv("Data/scfpvalue.csv")
siaf1 <- read_csv("Data/siaf1value.csv")
siap <- read_csv("Data/siapvalue.csv")
siar <- read_csv("Data/siarvalue.csv")
vm2 <- read_csv("Data/vm2values.csv")
vm <- read_csv("Data/vmval.csv")
mp <- read_csv("Data/mpvalue.csv")

X1657val <- read_csv("Data/1657val.csv")

temp = list.files(path = "./Data/COSIAPeter", pattern="*.csv",full.names = T)
COSIA = lapply(temp, read.csv)

patminr <- read_csv("Data/PatMinr4val.csv")
