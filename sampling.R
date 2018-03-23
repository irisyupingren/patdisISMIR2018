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
# ran355 <- sample_n(ran, 355)
pat355 <- sample_n(X1657val, 355)
# newpat355 <- bind_rows(siaf355, siap355, siar355, vm2355, vm355, mp355, scfp355, non355, ran355, pat355 , .id="id")
newpat355 <- bind_rows(siaf355, siap355, siar355, vm2355, vm355, mp355, scfp355, non355, pat355 , .id="id")
selepat355 <- selectjsym(newpat355)
group355 <- selepat355
saveRDS(selepat355, file="group355.rds")

alg1657.1 <- sample_n(groupalg, 1657)
alg1657.1[1] <- NULL
alg1657.2 <- sample_n(groupalg, 1657)
alg1657.2[1] <- NULL
alg1657.3 <- sample_n(groupalg, 1657)
alg1657.3[1] <- NULL
alg1657.4 <- sample_n(groupalg, 1657)
alg1657.4[1] <- NULL
alg1657.5 <- sample_n(groupalg, 1657)
alg1657.5[1] <- NULL
groupalgsample <- bind_rows(alg1657.1,alg1657.2, alg1657.3, alg1657.4, alg1657.5, .id="id")

alg1657 <- sample_n(groupalgsample, 1657)
alg1657[1] <- NULL
non1657 <- sample_n(groupnon, 1657)
non1657[1] <- NULL
# ran1657 <- sample_n(groupran, 1657)
# ran1657[1] <- NULL
pat1657 <- selepatjsym(X1657val) 
setdiff(colnames(alg1657), colnames(pat1657))
common <- intersect(colnames(alg1657), colnames(pat1657))
alg1657 <- alg1657[,common]
non1657 <- non1657[,common]

# newpat1657 <- bind_rows(alg1657, non1657, ran1657, X1657val , .id="id")
newpat1657 <- bind_rows(alg1657, non1657, pat1657, .id="id")
group2nonpat <- bind_rows(non1657, pat1657, .id="id")
group2algpat <- bind_rows(alg1657, pat1657, .id="id")
group2nonalg <- bind_rows(non1657, alg1657, .id="id")
group3 <- newpat1657

group3allnon5 <- bind_rows(alg, non, pat1657, .id="id")
group3allnon4 <- bind_rows(alg, non4, pat1657, .id="id")
group3allnon3 <- bind_rows(alg, non3, pat1657, .id="id")
group3allnon2 <- bind_rows(alg, non2, pat1657, .id="id")
group3allnon1 <- bind_rows(alg, non1657, pat1657, .id="id")

group3allnon5dedup <- bind_rows(unique(alg), unique(non), pat1657, .id="id")
group3allnon4dedup <- bind_rows(unique(alg), unique(non4), pat1657, .id="id")
group3allnon3dedup <- bind_rows(unique(alg), unique(non3), pat1657, .id="id")
group3allnon2dedup <- bind_rows(unique(alg), unique(non2), pat1657, .id="id")
group3allnon1dedup <- bind_rows(unique(alg), unique(non1657), pat1657, .id="id")

# selepat1657 <- selepatjsym(newpat1657)
# selepat1657 <- cbind(newpat1657$id, selepat1657)
# saveRDS(selepat1657, file="group4.rds")

newpat355 <- bind_rows(siaf355, siap355, siar355, vm2355, vm355, mp355, scfp355, .id="id")
selepat355 <- selectjsym(newpat355)
groupalg355 <- selepat355

saveRDS(selepat355, file="group355.rds")



