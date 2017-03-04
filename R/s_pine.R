library(tidyverse)
library(rstan)
load("./rdata/fatedata_OP.RDATA")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)

# remove trees/rows with no live tree observations
rm_row <- apply(fatemx, 1, function(row) all(row <=0)) %>%
  which() 
fatemx <- fatemx[-rm_row,]
survmx <- survmx[-rm_row,]

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
fatemx <- fatemx[1:150,]
survmx <- survmx[1:150,]
init_sz <- init_sz[1:150]
init_yr <- init_yr[1:150]

# bundle data for stan
stan_d <- list(ntree = nrow(fatemx),
               time = ncol(fatemx),
               fatemx = fatemx,
               survmx = survmx,
               init_sz = init_sz,
               init_yr = init_yr,
               wx = w)

# call stan
out <- stan('s.stan', data = stan_d, chains = 3,
            control = list(adapt_delta = 0.99))

# diagnostics
traceplot(out)
pairs(out, pars = c('a', 'b', 'c', 'alpha', 'beta', 'delta',
                    'sigma_ind', 'sigma_t', 'sigma_obs'))
post <- rstan::extract(out)
