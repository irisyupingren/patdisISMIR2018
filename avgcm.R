library(abind)
library(readr)
library(glue)
acomb <- function(...) abind(..., along=3)

readcm <- function(model, des, nfold){
  print(glue("cm{model}{des}.rds"))
  # cm <- c()
  # cmall <- array(dim=c(3,3))
  cmall <- array(dim=c(3,3,10))
  # cm <- array()
  for ( i in 1:nfold){
    print(i)
    cmrf <- readRDS(glue("cm{model}{des}-{i}.rds"))
    # cmrf <- readRDS(glue("cmrfdedupbalan-1.rds"))
    # cmall <- as.data.frame(cmrf$table)$Freq
    cmall[,,i] <- as.matrix(cmrf$table)
    # cm[[i]] <- as.matrix(cmrf$table)
  }
  return(cmall)
}

cmall <- readcm("rf", "dedup", "10")
cmall <- readcm("rf", "group3", "10")
avg <- apply(cmall, c(1,2), mean)
v <- apply(cmall, c(1,2), var)
# temp_array <- abind(cmrfgroup2algnonnewdata$table, cmrfgroup2algnonnewdata$table, along=3)
# res <- apply(temp_array, 1:2, mean)
