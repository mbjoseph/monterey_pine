# Simulating growth and trying to recover parameters ----------------------

library(tidyverse)
library(reshape2)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = 2)

# number of individuals and timesteps
n_ind <- 36
n_t <- 15

# growth parameters
alpha <- 3
beta <- .25

# initial timestep hyperparameters
mu_mu_1 <- 80
sigma_1 <- 15

# error in y (includes measurement and process error)
sigma_y <- 5



# simulating observations
mu <- matrix(nrow = n_ind, ncol = n_t)
y <- matrix(nrow = n_ind, ncol = n_t)

mu[, 1] <- rnorm(n_ind, mu_mu_1, sigma_1) # lognormal
y[, 1] <- rnorm(n_ind, mean = mu[, 1], sd = sigma_y)

for (t in 2:n_t) {
  mu[, t] <- y[, t - 1] + alpha * y[, t - 1] ^ beta
  y[, t] <- rnorm(n_ind, mean = mu[, t], sd = sigma_y)
}


# visualize size time sizes
y %>%
  reshape2::melt(varnames = c("individual", "timestep"), 
                 value.name = "measured_size") %>%
  tbl_df %>%
  ggplot(aes(timestep, measured_size)) + 
  geom_line(aes(group = individual))

mu %>%
  reshape2::melt(varnames = c("individual", "timestep"), 
                 value.name = "measured_size") %>%
  tbl_df %>%
  ggplot(aes(timestep, measured_size)) + 
  geom_line(aes(group = individual))



# simulate missing observations
y_true <- y

pct_missing <- .5
missing_obs <- sample(length(y), size = floor(pct_missing * length(y))) %>% 
                        sort
y[missing_obs] <- NA

# compute indices for observed and missing values
rows <- rep(1:n_ind, times = n_t)
cols <- rep(1:n_t, each = n_ind)

r_obs <- rows[!is.na(c(y))]
c_obs <- cols[!is.na(c(y))]

r_miss <- rows[is.na(c(y))]
c_miss <- cols[is.na(c(y))]



# Fit model ---------------------------------------------------------------
stan_d <- list(n = n_ind,
               k_obs = sum(!is.na(y)),
               n_t = n_t,
               y_obs = y[!is.na(y)],
               r_obs = r_obs,
               c_obs = c_obs, 
               k_miss = sum(is.na(y)), 
               r_miss = r_miss, 
               c_miss = c_miss
)

m_init <- stan_model("max-sims/mod.stan")

# increasing adapt_delta tends to avoid boundary issues for sigma_mu1
# but may not be necessary with many individuals
m_fit <- sampling(m_init, data = stan_d, 
                  control = list(adapt_delta = 0.99))
m_fit

traceplot(m_fit, pars = c('alpha', 'beta', 'sigma_y', "sigma_mu1"))

# posterior correlation still persists but sampler seems to deal with it
pairs(m_fit, pars = c('alpha', 'beta', 'sigma_y', "sigma_mu1"))

post <- extract(m_fit)

# check parameter recovery
par(mfrow = c(2, 2))
plot(density(post$beta))
abline(v = beta, lty = 2)
plot(density(post$alpha))
abline(v = alpha, lty = 2)
plot(density(post$sigma_y))
abline(v = sigma_y, lty = 2)
plot(density(post$sigma_mu1))
abline(v = sigma_1, lty = 2)


# plot modeled growth trajectories for each tree
y_df <- y_true %>%
  reshape2::melt(varnames = c("id", "t"), 
                 variable.name = "size") %>%
  tbl_df %>%
  arrange(t, id) %>%
  mutate(idx = 1:n(), 
         observed = !(idx %in% missing_obs))


p1 <- post %>%
  `[[`("y_full") %>%
  apply(c(2,3), FUN = function(x) c(quantile(x, c(0.025, 0.5, 0.975)))) %>%
  reshape2::melt(varnames = c("quantile", "id", "t"), value.name = "sz") %>%
  spread(key = quantile, value = sz) %>%
  tbl_df %>%
  ggplot(aes(x = t, group = id)) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.3, fill = "red") +
  theme_minimal() + 
  geom_point(aes(x = t, y = value, color = observed), data = y_df, size = .6) + 
  xlab("Timestep") + 
  ylab("Size")

p1 + 
  facet_wrap(~id)

p1 + 
  facet_wrap(~id, scales = "free")


