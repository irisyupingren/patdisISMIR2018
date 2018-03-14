library(dplyr)
scfpcoor355 <- sample_n(coorsc, 355)
siafcoor355 <- sample_n(coorsif1, 355)
siapcoor355 <- sample_n(coorsip, 355)
siarcoor355 <- sample_n(coorsir, 355)
vm2coor355 <- sample_n(coorvm2, 355)
vmcoor355 <- sample_n(coorvm, 355)
mpcoor355 <- sample_n(coormp, 355)
noncoor355 <- sample_n(coornon, 355)
rancoor355 <- sample_n(coorran, 355)
patcoor355 <- sample_n(scores, 355)

algcoor <- bind_rows(coorsc, coormp, coorsif1, coorsip, coorsir, coorvm2, coorvm, .id="id")
algcoor1657 <- sample_n(algcoor, 1657)
algcoor1657$id <- NULL

group2pca <- bind_rows(coornon, scores, .id="id")
group3pca <- bind_rows(coornon, coorran, scores, .id="id")
group4pca <- bind_rows(algcoor1657, coornon, coorran, scores, .id="id")
groupalgpca <- bind_rows(algcoor1657, coornon, coorran, scores, .id="id")
group1657pca <-bind_rows(scores, coornon, coorran, coormp, coorvm, coorvm2, coorsif1, coorsip, coorsir, coorsc, .id="id")

coorgroup355alg <- bind_rows(siafcoor355, siapcoor355, siarcoor355, vm2coor355, vmcoor355, mpcoor355, scfpcoor355,.id="id")
coorgroup355 <- bind_rows(siafcoor355, siapcoor355, siarcoor355, vm2coor355, vmcoor355, mpcoor355, scfpcoor355, noncoor355, rancoor355, patcoor355,.id="id")
