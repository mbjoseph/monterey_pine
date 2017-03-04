library(tidyverse)
library(rstan)
load("./rdata/fatedata_OP.rdata")
load("./rdata/fatedata_SSC.RDATA")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)

# remove trees/rows with no live tree observations
rm_row <- apply(fatemx, 1, function(row) all(row <=0)) %>%
  which() 
fatemx <- fatemx[-rm_row,]

# vector containing 1st observed size of each tree, and year of 1st observation
init_sz <- NULL
init_yr <- NULL

for(i in 1:nrow(fatemx)){
  init_sz[i] <- round(fatemx[i, which(fatemx[i, ] > 0)[1]], 2)
  init_yr[i] <- which(fatemx[i, ] > 0)[1]
  }

# precip and fog data

w <- c(201.422,	242.57,	314.706,	436.626,	427.99,	140.208,	288.544,	
           305.562,	458.978,	586.232,	279.146,	74.168,	207.01, 173.228) %>%
  scale() %>%
  as.vector()

cloud <- read.csv("cloud_160305.csv") %>%
  filter(Metric == 5, Year < 2016) %>%
  select(Mean) %>%
  scale() %>% 
  as.vector()

#subset of data to test
# fmx <- fatemx[1:150,]
# sinit <- init_sz[1:150]
# yinit <- init_yr[1:150]

# Stan call
stan_d <- list(ntree = nrow(fatemx),
               time = ncol(fatemx),
               fatemx = fatemx,
               init_sz = init_sz,
               init_yr = init_yr,
               wx = w)

out <- stan('g.stan', data = stan_d, chains = 3)
#print(out, digits = 1)
traceplot(out)
pairs(out, pars = c('a', 'b', 'c', 'sigma_ind', 'sigma_t', 'sigma_obs'))

# extract posteriors from model obj
post <- rstan::extract(out)
plot(density(post$b))
    