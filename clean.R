source("./utilfuncs.R")
library(dplyr)
non1s <- non1song[,common]
non2s <- non2song[,common]
non3s <- non3song[,common]
non4s <- non4song[,common]
non5s <- non5song[,common]
groupnonsdedup <- bind_rows(unique(non1s),unique(non2s),unique(non3s),unique(non4s),unique(non5s))
groupnons <- bind_rows(non1s,non2s,non3s, non4s, non5s)
# groupnonsdedup1657 <- sample_n(groupnonsdedup, 1657)
groupnonsdedup1287 <- sample_n(groupnonsdedup,1287)
groupnon1287 <- sample_n(non1s, 1287)

alg1287 <- sample_n(unique(alg1657),1287)
pat1287 <- sample_n(unique(pat1657),1287)
non1287 <- sample_n(unique(non1657),1287)
alg1657 <- alg1657[,common]

group3nonsong1657 <- bind_rows(alg1657, non1s, pat1657,.id="id")
group3nonsong1287 <- bind_rows(alg1287, non1287, pat1287,.id="id")
group3nonsongunbalan <- bind_rows(unique(alg1657), unique(non1s), unique(pat1657),.id="id")

groupnons <- bind_rows(non1s,non2s,non3s, .id="id")
ugroupnons <- unique(groupnons)
upat1657 <- unique(pat1657)
nonanno <- bind_rows(ugroupnons, upat1657)

groupnon <- bind_rows(non,non2,non3,non4,non5, .id="id")
groupnon <- bind_rows(non1song,non2song,non3song,non4song,non5song, .id="id")

groupnon <- selectjsym(groupnon)

non <- groupnon[,common]
non2 <- sample_n(non, 1657*2)
non3 <- sample_n(non, 1657*3)
non4 <- sample_n(non, 1657*4)


# groupran <- bind_rows(ran,ran2,ran3,ran4,ran5, .id="id")
# groupran <- selectjsym(groupran)

# group10 <- bind_rows(scfp, mp, siaf1, siap, siar, vm2, vm, non,ran, X1657val , .id="id")
# group10 <- selectjsym(group10)

groupalg <- bind_rows(scfp, mp, siaf1, siap, siar, vm2, vm, .id="id")
groupalg <- selectjsym(groupalg)

alg <- groupalg[,common]
alg1657 <- sample_n(alg,1657)
# groupall <- bind_rows(scfp, mp, siaf1, siap, siar, vm2, vm, non,non2,non3,non4,non5,ran,ran2,ran3,ran4,ran5, X1657val , .id="id")
# groupall <- selectjsym(groupall)

pat <- selepatjsym(X1657val)
