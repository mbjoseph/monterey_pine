library(dplyr)
library(rstan)
load("fatedata.rdata")

# remove trees/rows with no live tree observations
rm_row <- apply(fatemx, 1, function(row) all(row <=0)) %>%
  which() 
fatemx <- fatemx[-rm_row,]

# keep growth data, rename
g_fatemx <- fatemx

# change growth to surv 
fatemx[fatemx > 0] <- 1
head(fatemx)

# vector containing 1st observed size of each tree
init_sz <- NULL
for(i in 1:nrow(fatemx)){
  init_sz[i] <- round(fatemx[i, which(fatemx[i, ] > 0)[1]], 2)
}
init_sz

