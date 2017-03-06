library(tidyverse)
source("./R/fn_mxdf.R")

#################
## simulate data for n trees using non linear growth model
#################

n_t <- 14 # time steps
n <- 100 # number of trees

# state process error term
sigma_epsilon_n <- 0.2
#sigma_epsilon_t <- 0.1
epsilon_n <- rnorm(n, 0, sigma_epsilon_n)

z <- matrix(NA, n, n_t)
zdiff <- matrix(NA, n, n_t)
# set true initial size
z[,1] <- runif(n, 1, 50)
zdiff[,1] <- 0

# model parameters
a <- 1.2
b <- 0.2

for (i in 1:n){
  for (t in 2:n_t) {
    zdiff[i,t] <-  a * z[i, t-1]^b * exp(epsilon_n[i])
    z[i,t] <- z[i,t-1] + zdiff[i,t]
  }
}

# observed data, with some fixed observation error
y <- rnorm(n_t * n, mean = z, sd = 0.01) %>% 
  matrix(n, n_t)

# simulate missing obsrevations
#missing_obs <- sample(length(y), size = length(y)/3) %>% sort()
#y[missing_obs] <- NA

y_df <- mx2df(y)

# sz v grw
y_df %>%
  ggplot(aes(prev_sz, grw, color = as.factor(r))) +
  geom_line(alpha = 0.8) +
  ggtitle(paste("alpha = ", a, "; beta = ", b)) +
  theme_minimal() +
  theme(legend.position = "none")

# size v time
y_df %>%
  ggplot(aes(c, y, color = as.factor(r))) +
  geom_line(alpha = 0.8) +
  ggtitle(paste("alpha = ", a, "; beta = ", b)) +
  theme_minimal() +
  theme(legend.position = "none")

#tim v grw
y_df %>%
  ggplot(aes(c, grw, color = as.factor(r))) +
  geom_line(alpha = 0.8) +
  theme_minimal() +
  theme(legend.position = "none")


y_init <- y_df %>%
  group_by(r) %>%
  summarise(init = min(y)) %>%
  select(init)

##################
##################

library(rstan)
options(mc.cores = parallel::detectCores() - 1)

stan_d <- list(n = max(y_df$r),
               k = length(y_df$y),
               n_t = max(y_df$c),
               delta_y = y_df$grw,
               dy1 = rep(0, length(y_init$init)),
               z = y,
               r = y_df$r,
               c = y_df$c)

m_fit <- stan("./R/mp_diff.stan", data = stan_d, chains = 3)
m_fit

watch <- c('alpha', 'beta', 'sigma_epsilon_n', 'dy[2,10]')
traceplot(m_fit, pars = watch)
pairs(m_fit, pars = watch)

post <- extract(m_fit)
plot(density(post$beta))
plot(density(post$alpha))
