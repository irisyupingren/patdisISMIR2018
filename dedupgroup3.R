dedupedpat <- unique(pat1657)
dedupedalg <- unique(alg1657)
dedupednon <- unique(non1657)

library(dplyr)

alg1287 <- sample_n(alg1657, 1287)
non1287 <- sample_n(non1657, 1287)
common <- intersect(colnames(alg1657), colnames(pat1657))
alg1287 <- alg1287[,common]
non1287 <- non1287[,common]
dedupedalg <- dedupedalg[,common]

library(plyr)
dedupngroup3balan <- bind_rows(dedupedpat, alg1287, non1287, .id="id")
dedupgroup3 <- bind_rows(dedupedpat, dedupedalg, dedupednon, .id="id")
