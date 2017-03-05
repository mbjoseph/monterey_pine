data{
  int<lower = 1> n; // number of individual trees
  int<lower = 1> k; // number of observations
  int<lower = 1> n_t; // time steps
  vector[n] z1;
  vector[n] z_hat1;
  vector[k] y;
  int r[k];
  int c[k];
}

parameters{
  real alpha;
  real<lower =0> beta;
  matrix[n, n_t] epsilon;
  real<lower = 0> sigma_epsilon;
}

transformed parameters{
  matrix[n, n_t] z;
  matrix[n, n_t] log_z_hat;
  z[,1] = z1;
  log_z_hat[,1] = z_hat1;
  
  for(tree in 1:n){
    for (t in 2:n_t) {
      log_z_hat[tree, t] =  alpha + beta * log(z[tree, t-1]);
      z[tree, t] = z[tree, t-1] + exp(log_z_hat[tree, t]) + epsilon[tree, t];
    } 
  }
  
}

model{
  alpha ~ lognormal(0, 0.5);
  beta ~ normal(0, 1);
  sigma_epsilon ~ normal(0, 0.5);
  to_vector(epsilon) ~ normal(0, sigma_epsilon);

  for(i in 1:k)
    y[i] ~ normal(z[r[i], c[i]], 0.1);
}
