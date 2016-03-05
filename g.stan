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
  //real d; // interaction
  real<lower=0> sigma_obs;  // SD of observation process
  real<lower=0> sigma_ind;
  real<lower=0> sigma_t;
  vector<lower=0>[ntree] ind_var;
  vector<lower=0>[time] t_var;
  vector<lower=0>[ntree] sz_est1;
}

transformed parameters {
  matrix[ntree,time] sz;
  
  // State process
  for (n in 1:ntree){
    sz[n, init_yr[n]] <- sz_est1[n]; 
    // add one to init_yr to allow time loop index
    for (t in (init_yr[n]+1):time){
      sz[n,t] <- a + b*sz[n,t-1] + c*precip[t] + ind_var[n] + t_var[t];
      //interaction: d*sz[n,t-1]*precip[t]
    }
  }
}

model {
  // Priors
  sz_est1 ~ normal(init_sz, 2); // not sure if this will work
  sigma_ind ~ normal(0,3);
  sigma_t ~ normal(0,3);
  sigma_obs ~ normal(0,3);
  

  ind_var ~ normal(0, sigma_ind);
  t_var ~ normal(0, sigma_t);


  // Observation process

  for (n in 1:ntree){
    for (t in (init_yr[n]+1):time){
      if (fatemx[n,t] > 0){
        fatemx[n,t] ~ normal(sz[n,t], sigma_obs);
      }
    }
  }
  
}
