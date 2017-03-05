library(tidyverse)
source("./R/fn_mxdf.R")

#################
## simulate data for n trees using non linear growth model
#################

n_t <- 14 # time steps
n <- 75 # number of trees

# state process error term
sigma_epsilon <- 0.075
epsilon <- rnorm(n_t * n, 0, sigma_epsilon) %>% 
  matrix(n, n_t)

z <- matrix(NA, n, n_t)
log_z_hat <- matrix(NA, n, n_t)
# set true initial size
z[,1] <- rnorm(n, 8, 8) %>% abs()

# model parameters
a <- 0.5
b <- 0.3

for (i in 1:n){
  for (t in 2:n_t) {
    log_z_hat[i,t] <- a + b * log(z[i, t-1])
    z[i,t] <- z[i,t-1] + exp(log_z_hat[i,t]) + epsilon[i,t]
  }
}



# observed data, with some fixed observation error
y <- rnorm(n_t * n, mean = z, sd = 0.1) %>% 
  matrix(n, n_t)

# simulate missing obsrevations
#missing_obs <- sample(length(y), size = length(y)/3) %>% sort()
#y[missing_obs] <- NA

y_df <- mx2df(y)

# sz v grw
y_df %>%
  ggplot(aes(prev_sz, grw, color = as.factor(r))) +
  geom_line(alpha = 0.8) +
  ggtitle(a,b) +
  theme_minimal() +
  theme(legend.position = "none")

# size v time
y_df %>%
  ggplot(aes(c, y, color = as.factor(r))) +
  geom_line(alpha = 0.8) +
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



