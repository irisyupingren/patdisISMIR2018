library(dplyr)
source('utilfuncs.R')
scfp355 <- sample_n(scfp, 355)
siaf355 <- sample_n(siaf1, 355)
siap355 <- sample_n(siap, 355)
siar355 <- sample_n(siar, 355)
vm2355 <- sample_n(vm2, 355)
vm355 <- sample_n(vm, 355)
mp355 <- sample_n(mp, 355)
non355 <- sample_n(non, 355)
ran355 <- sample_n(ran, 355)
pat355 <- sample_n(X1657val, 355)
newpat355 <- bind_rows(siaf355, siap355, siar355, vm2355, vm355, mp355, scfp355, non355, ran355, pat355 , .id="id")
selepat355 <- selectjsym(newpat355)
saveRDS(selepat355, file="group355.rds")

alg1657 <- sample_n(groupalg, 1657)
alg1657[1] <- NULL
non1657 <- sample_n(groupnon, 1657)
non1657[1] <- NULL
ran1657 <- sample_n(groupran, 1657)
ran1657[1] <- NULL
X1657val[1] <- NULL
newpat1657 <- bind_rows(alg1657, non1657, ran1657, X1657val , .id="id")

selepat1657 <- selepatjsym(newpat1657)
selepat1657 <- cbind(newpat1657$id, selepat1657)
saveRDS(selepat1657, file="group4.rds")

newpat355 <- bind_rows(siaf355, siap355, siar355, vm2355, vm355, mp355, scfp355, .id="id")
selepat355 <- selectjsym(newpat355)
saveRDS(selepat355, file="group355.rds")


