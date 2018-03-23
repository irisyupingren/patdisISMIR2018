library(resample)
r <- cagroup2algpatoutput[[1]]$values
r <- cagroup2outputcoor
r <- cagroup3output

r <- r[[1]]$values
res1 <- colMeans(r[2:67])
res2 <- colVars(r[2:67])
resaccu <- matrix(res1,nrow=6)[,1]
resvar <- matrix(res2, nrow=6)[,2]
