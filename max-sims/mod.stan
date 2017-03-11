data{
  int<lower = 1> n; // number of individual trees
  int<lower = 1> n_t; // time steps
  int<lower = 1, upper = n * n_t> k_obs; // number of observations
  vector<lower = 0>[k_obs] y_obs;
  int r_obs[k_obs];
  int c_obs[k_obs];
  
  // missing indices for y matrix
  int<lower = 1, upper = n*n_t> k_miss;
  int r_miss[k_miss];
  int c_miss[k_miss];
}

parameters{
  real<lower = 0> alpha;
  real<lower = 0> beta;
  real<lower = 0> sigma_y;
  vector[n] mu1;
  real mu_mu1;
  real<lower = 0> sigma_mu1;
  vector<lower = 0>[k_miss] y_miss;
}

transformed parameters {
  matrix[n, n_t] mu;
  matrix[n, n_t] y_full;
  
  for (i in 1:k_obs) 
    y_full[r_obs[i], c_obs[i]] = y_obs[i];
    
  for (i in 1:k_miss) 
    y_full[r_miss[i], c_miss[i]] = y_miss[i];
  
  // location parameter
  mu[, 1] = mu1;
  for (i in 1:n) {
    for (t in 2:n_t) {
      mu[i, t] = y_full[i, t - 1] + alpha * pow(y_full[i, t - 1], beta);
    }
  }
}

model {
  // priors for initial size
  mu_mu1 ~ student_t(5, 0, 1);
  sigma_mu1 ~ lognormal(0, 1);
  mu1 ~ normal(mu_mu1, sigma_mu1);
  
  // growth parameters
  alpha ~ normal(0, 3);
  beta ~ normal(0, 3);
  
  // error term (includes process and measurement error)
  sigma_y ~ student_t(5, 0, 1);

  to_vector(y_full) ~ normal(to_vector(mu), sigma_y);
}
