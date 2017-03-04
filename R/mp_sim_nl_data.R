library(tidyverse)
source("./R/fn_mxdf.R")

#################
## simulate data for n trees using non linear growth model
#################

n_t <- 14 # time steps
n <- 75 # number of trees

# state process error term
sigma_epsilon <- 0.1
epsilon <- rnorm(n_t * n, 0, sigma_epsilon) %>% 
  matrix(n, n_t)

z <- matrix(NA, n, n_t)
# set true initial size
z[,1] <- rnorm(n, 8, 9)^2 %>% abs()

# model parameters
a <- 3
b <- 0.25
#c <- -0.11

for (i in 1:n){
  for (t in 2:n_t) {
    z[i,t] <- z[i,t-1] + a * z[i, t-1]^b + exp(epsilon[i,t])
  }
}

# size v time
mx2df(z) %>%
  ggplot(aes(c, y, color = as.factor(r))) +
  geom_line(alpha = 0.8) +
  theme_minimal() +
  theme(legend.position = "none")

# growth v time
mx2df(z) %>%
  ggplot(aes(c, grw, color = as.factor(r))) +
  geom_line(alpha = 0.8) +
  theme_minimal() +
  theme(legend.position = "none")


# observed data, with some fixed observation error
y <- rnorm(n_t * n, mean = z, sd = 0.1) %>% 
  matrix(n, n_t)


# simulate missing obsrevations
#missing_obs <- sample(length(y), size = length(y)/3) %>% sort()
#y[missing_obs] <- NA

matplot(t(y), type = "l")

y_df <- mx2df(y)

y_init <- y_df %>%
  group_by(r) %>%
  summarise(init = min(y)) %>%
  select(init)

###########
###########

library(rstan)
options(mc.cores = parallel::detectCores() - 1)

stan_d <- list(n = n,
               k = length(y_df$y),
               n_t = n_t,
               z1 = y_init$init,
               y = y_df$y,
               r = y_df$r,
               c = y_df$c)

m_fit <- stan("./R/mp_ar1.stan", data = stan_d, chains = 3)

traceplot(m_fit, pars = c('beta', 'sigma_epsilon'))
pairs(m_fit, pars = c('beta', 'sigma_epsilon'))

post <- extract(m_fit)

plot(density(post$beta))
plot(density(post$sigma_epsilon))

# plot modeled growth trajectories for each tree

post %>%
  `[[`("z") %>%
  apply(c(2,3), FUN = function(x) c(quantile(x, c(0.025, 0.5, 0.975)))) %>%
  reshape2::melt(varnames = c("quantile", "id", "t"), value.name = "sz") %>%
  spread(key = quantile, value = sz) %>%
  tbl_df %>%
  ggplot(aes(x = t, y = `50%`, group = id)) +
  geom_line(alpha = 0.5) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.1) +
  theme_minimal()


