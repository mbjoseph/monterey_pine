library(dplyr)
library(rstan)
load("fatedata.rdata")


# allow Stan to execute multiple Markov chains in parallel
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

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

# precip data
w <- c(201.422,	242.57,	314.706,	436.626,	427.99,	140.208,	288.544,	
           305.562,	458.978,	586.232,	279.146,	74.168,	207.01)

#subset of data to test
fmx <- fatemx[1:200,]
sinit <- init_sz[1:200]
yinit <- init_yr[1:200]

# Stan call
stan_d <- list(ntree = nrow(fmx),
               time = ncol(fmx),
               fatemx = fmx,
               init_sz = sinit,
               init_yr = yinit,
               precip = w)

out <- stan('g.stan', data = stan_d)
#print(out, digits = 1)
traceplot(out)
pairs(out, pars = c('a', 'b', 'c', 'sigma_proc', 'sigma_obs'))

# extract posteriors from model obj
post <- rstan::extract(out)
plot(density(post$d))
