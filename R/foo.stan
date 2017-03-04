data{
  int<lower = 0> n; // number of total observations
  int indv[n]; // assign indv tree to each observation
  vector[n] sz; // last observed size
  vector[n] y; // current size
  vector[n] delta; // time since last observation
}

parameters{
  vector[n] a;
  vector[n] b;
  real c;
  real mu_a;
  real mu_b;
  real mu_c;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  real<lower=0> sigma_c;
  real<lower=0> sigma;
}

transformed parameters{
  vector[n] y_hat;
  
  for (i in 1:n)
      y_hat[i] = a[indv[i]] + b[indv[i]] * sz[i] + c * delta[i];
}

model{
  mu_a ~ normal(0, 3);
  mu_b ~ normal(0, 3);
  mu_c ~ normal(0, 3);
  sigma_a ~ normal(0, 2);
  sigma_b ~ normal(0, 2);
  sigma_c ~ normal(0, 2);
  sigma ~ normal(0, 2);

  a ~ normal(mu_a, sigma_a);
  b ~ normal(mu_b, sigma_b);
  c ~ normal(mu_c, sigma_c);

  y ~ normal(y_hat, sigma);
}
