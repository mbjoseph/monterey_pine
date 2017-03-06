data{
  int<lower = 1> n; // number of individual trees
  int<lower = 1> k; // number of observations
  int<lower = 1> n_t; // time steps
  vector[k] delta_y; // observed growth
  matrix[n, n_t] z; // observed sizes
  vector[n] dy1; // init state process
  int r[k];
  int c[k];
}

parameters{
  real<lower = 0> alpha;
  real<lower = 0> beta;
  real<lower = 0> sigma_epsilon_n;
  vector[n] epsilon;
}

transformed parameters{

matrix[n, n_t] dy;
dy[,1] = dy1;

  for(tree in 1:n){
    for (t in 2:n_t) {
      dy[tree, t] = alpha * pow(z[tree, t-1], beta) * exp(epsilon[n]);
    } 
  }
}

model{
  alpha ~ normal(0, 0.5);
  beta ~ normal(0, 0.5);
  sigma_epsilon_n ~ normal(0, 0.3);
  epsilon ~ normal(0, sigma_epsilon_n);
  
  for(i in 1:k)
    delta_y[i] ~ normal(dy[r[i], c[i]], 0.01);
}
