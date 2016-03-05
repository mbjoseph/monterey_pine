data {
  int ntree;
  int<lower=0> time;
  matrix[ntree,time] fatemx;
  int survmx[ntree,time];
  vector[ntree] init_sz;
  int init_yr[ntree];
  vector[time] wx;
}

parameters {
  real a; // intercept
  real b; // size
  real c; // precip
  real d; // interaction
  real alpha;
  real beta;
  real delta;
  real gamma;
  real<lower=0> sigma_ind;
  real<lower=0> sigma_t;
  real<lower=0> sigma_obs;  // SD of observation process
  vector<lower=0>[ntree] ind_var;
  vector<lower=0>[time] t_var;
  vector<lower=0>[ntree] sz_est1;
}

transformed parameters {
  matrix[ntree,time] sz;
  matrix[ntree,time] survprob;
  
  // State process
  for (n in 1:ntree){
    
    sz[n, init_yr[n]] <- sz_est1[n]; 
    survprob[n,init_yr[n]] <- 1;
    
    for (t in (init_yr[n]+1):time){
      
      sz[n,t] <- a + b*sz[n,t-1] + c*wx[t] + d*sz[n,t-1]*wx[t] + 
        ind_var[n] + t_var[t];
      
      survprob[n,t] <- survprob[n,t-1] * logit(alpha + beta*sz[n,t-1] + 
        delta*wx[t] + gamma*sz[n,t-1]*wx[t]);
      
    }
  }
}

model {
  // Priors
  sz_est1 ~ normal(init_sz, 2);
  sigma_ind ~ normal(0,3);
  sigma_t ~ normal(0,3);
  sigma_obs ~ normal(0,3);
  a ~ normal(0,5);
  b ~ normal(0,5);
  c ~ normal(0,5);
  d ~ normal(0,5);
  alpha ~ normal(0,5);
  beta ~ normal(0,5);
  delta ~ normal(0,5);
  gamma ~ normal(0,5);
  
  ind_var ~ normal(0, sigma_ind);
  t_var ~ normal(0, sigma_t);


  // Observation process

  for (n in 1:ntree){
     for (t in (init_yr[n]+1):time){
       if (fatemx[n,t] > 0){
        fatemx[n,t] ~ normal(sz[n,t], sigma_obs);
      if (survmx[n,t] >= 0){
        survmx[n,t] ~ bernoulli(survprob[n,t]);
        }
      }
    }
  }
}
