cacoorpca2results2prepctrl <- readRDS("~/Dropbox/patdis4ISMIR2018/patdisISMIR2018/caprepctrl/cacoorpca2results2prepctrl.rds")
cacoorpca2results3prepctrl <- readRDS("~/Dropbox/patdis4ISMIR2018/patdisISMIR2018/caprepctrl/cacoorpca2results3prepctrl.rds")
cacoorpca3resultsprepctrl <- readRDS("~/Dropbox/patdis4ISMIR2018/patdisISMIR2018/caprepctrl/cacoorpca3resultsprepctrl.rds")
cacoorpcaalg355resultsprepctrl <- readRDS("~/Dropbox/patdis4ISMIR2018/patdisISMIR2018/caprepctrl/cacoorpcaalg355resultsprepctrl.rds")
cacoor355prepctrl <- readRDS("~/Dropbox/patdis4ISMIR2018/patdisISMIR2018/caprepctrl/cacoor355prepctrl.rds")
groupcoor355prepctrl <- readRDS("~/Dropbox/patdis4ISMIR2018/patdisISMIR2018/caprepctrl/groupcoor355prepctrl.rds")


ca355algresultsprepctrl <- readRDS("~/Dropbox/patdis4ISMIR2018/patdisISMIR2018/caprepctrl/ca355algresultsprepctrl.rds")
ca355results2prepctrl <- readRDS("~/Dropbox/patdis4ISMIR2018/patdisISMIR2018/caprepctrl/ca355results2prepctrl.rds")
ca3resultsprepctrl <- readRDS("~/Dropbox/patdis4ISMIR2018/patdisISMIR2018/caprepctrl/ca3resultsprepctrl.rds")
ca2resultsprepctrlpca <- readRDS("~/Dropbox/patdis4ISMIR2018/patdisISMIR2018/caprepctrl/ca2resultsprepctrlpca.rds")
`ca2resultsprepctrl (ics066605's conflicted copy 2018-03-16)` <- readRDS("~/Dropbox/patdis4ISMIR2018/patdisISMIR2018/caprepctrl/ca2resultsprepctrl (ics066605's conflicted copy 2018-03-16).rds")

library(resample)
r <- ca355algresultsprepctrl
r <- ca355results2prepctrl
r <- ca355resultsprepctrl
r <- cacoorpca3resultsprepctrl
r <- cacoor355prepctrl
r <- cacoorpcaalg355resultsprepctrl
res1 <- colMeans(r$values[2:67])
res2 <- colVars(r$values[2:67])
resaccumean <- matrix(res1, nrow=6)[,1]
resaccuvar <- matrix(res2, nrow=6)[,1]

