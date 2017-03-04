data {
  int<lower = 1> n; // number of individual trees
  int<lower = 1> k; // number of observations
  int<lower = 1> n_t; // time steps
  vector[n] z1;
  vector[k] y;
  int r[k];
  int c[k];
}

parameters {
  real<lower = 0> beta;
  real epsilon[n, n_t];
  real<lower = 0> sigma_epsilon;
}

transformed parameters {
  matrix[n, n_t] z;
 
  z[,1] = z1;

  for(tree in 1:n){
    for (t in 2:n_t) {
      z[tree, t] = z[tree, t-1] + beta * z[tree, t-1] + epsilon[tree, t];
    } 
  }
}

model {
  beta ~ normal(0, 0.5);
  sigma_epsilon ~ normal(0, 0.5);
  
  for(q in 1:n){
    for (p in 2:n_t) {
      epsilon[q,p] ~ normal(0.0, sigma_epsilon);
    }
  }
  
  for(i in 1:k)
    y[i] ~ normal(z[r[i], c[i]], 0.1);

}
