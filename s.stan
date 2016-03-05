data {
  int ntree;
  int<lower=0> time;
  matrix[ntree,time] fatemx;
  int init_yr[ntree];
  //vector[time] precip;
}

parameters {
  real a; // intercept
  real b; // size
  //real c; // precip
  //real d; // interaction
  real<lower=0> sigma_proc; // SD of state process
  real<lower=0> sigma_obs;  // SD of observation process
  matrix<lower=0>[ntree,time] surv_var;
}

transformed parameters {
  matrix[ntree,time] surv;
  
  // State process
  for (n in 1:ntree){
    sz[n, init_yr[n]] <- 1; 
    // add one to init_yr to allow time loop index
    for (t in (init_yr[n]+1):time){
      surv[n,t] <- bernoulli_logit(a + b*sz[n,t-1] + surv_var[n,t-1]);
    }
  }
}

model {
  // Priors
  sz_est1 ~ normal(init_sz, 2); // not sure if this will work
  sigma_proc ~ normal(0,3);
  sigma_obs ~ normal(0,3);

  for (i in 1:ntree){
    for (j in 1:time){
        surv_var[i,j] ~ normal(0, sigma_proc);
    }
  }

  // Observation process

  for (n in 1:ntree){
    for (t in 2:time){
      if (fatemx[n,t] > 0){
        survmx[n,t] ~ normal(sz[n,t], sigma_obs);
      }
    }
  }
  
}
