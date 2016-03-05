data {
  int ntree;
  int<lower=0> time;
  matrix[ntree,time] fatemx;
  vector[ntree] init_sz;
  int init_yr[ntree];
  vector[time] precip;
}

parameters {
  real a; // intercept
  real b; // size
  real c; // precip
  real d; // interaction
  real<lower=0> sigma_proc; // SD of state process
  real<lower=0> sigma_obs;  // SD of observation process
  matrix<lower=0>[ntree,time] growvar;
  vector<lower=0>[ntree] sz_est1;
}

transformed parameters {
  matrix[ntree,time] sz;
  
  // State process
  for (n in 1:ntree){
    sz[n, init_yr[n]] <- sz_est1[n]; 
    survprob[n,init_yr[n]]=1
    // add one to init_yr to allow time loop index
    for (t in (init_yr[n]+1):time){
      sz[n,t] <- a + b*sz[n,t-1] + c*precip[t] + d*sz[n,t-1]*precip[t] + growvar[n,t-1];
      survprob[n,t] <- survprob[n,t-1]*logit(Sa + Sb*sz[n,t-1] + Sc*precip[t] + Sd*sz[n,t-1]*precip[t] + growvar[n,t-1];
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
        growvar[i,j] ~ normal(0, sigma_proc);
    }
  }

  // Observation process

  for (n in 1:ntree){
     for (t in (init_yr[n]+1):time){
       if (fatemx[n,t] > 0){
        fatemx[n,t] ~ normal(sz[n,t], sigma_obs);
      if (survmx[n,t] >= 0){
        survmx[n,t] ~ bernoulli(surv[n,t]);
      }
    }
  }
  
}
