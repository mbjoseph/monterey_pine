data{
  int<lower = 0> n; // number of total observations
  //int indv[n]; // assign indv tree to each observation
  //int t[n]; // time of each obsevation
  vector[n] sz; // last observed size
  vector[n] y; // current size
  //vector[n] wx; //weather data
  vector[n] delta; // number of missed years since last observation
}

parameters{
  real a;
  real <lower=0> b;
  real mu_a;
  real <lower=0> mu_b;
  //real mu_b0;
  //real mu_b1;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  real<lower=0> sigma;
}

transformed parameters{
  vector[n] y_hat;
  //vector[n] b_hat;
  real y1;
  real y2;
  real y3;

  for (i in 1:n) {
      //b_hat[i] = mu_b0 + mu_b1 * wx[t[i]];

        if (delta[i] == 0)
          y_hat[i] = a + b * sz[i];
        else if (delta[i] == 1){
          y1 = a + b * sz[i];
          y_hat[i] = a + b * y1;
        }
        else if (delta[i] == 2){
          y1 = a + b * sz[i];
          y2 = a + b * y1;
          y_hat[i] = a + b * y2;
        }
        else if (delta[i] == 3){
          y1 = a + b * sz[i];
          y2 = a + b * y1;
          y3 = a + b * y2;
          y_hat[i] = a + b * y3;
        }
  }
}

model{
  mu_a ~ normal(0, 5);
  mu_b ~ normal(0, 3);
  sigma_a ~ normal(0, 1);
  sigma_b ~ normal(0, 1);
  sigma ~ normal(0, 3);

  a ~ normal(mu_a, sigma_a);
  b ~ normal(mu_b, sigma_b);

  y ~ normal(y_hat, sigma);
}
