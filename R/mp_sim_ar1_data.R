library(tidyverse)
source("./R/fn_mxdf.R")

#################
## simulate data for n trees using simple AR1 model
#################

n_t <- 14 # time steps
n <- 100 # number of trees
# state process error term
sigma_epsilon <- 0.01
epsilon_n <- rnorm(n, 0, sigma_epsilon)

z <- matrix(NA, n, n_t)
# set true initial size
z[,1] <- rnorm(n, 9, 9) %>% abs()
# growth rate
beta <- 0.05

# deterministic growth function
for (i in 1:n){
  for (t in 2:n_t) {
    z[i,t] <- z[i,t-1] + beta * z[i, t-1] + epsilon_n[i]
  }
}


# observed data, with some fixed observation error
y <- rnorm(n_t * n, mean = z, sd = 0.1) %>% 
  matrix(n, n_t)

# simulate missing obsrevations
#missing_obs <- sample(length(y), size = length(y)/3) %>% sort()
#y[missing_obs] <- NA

# reformat data from matrix to data frame, long form
y_df <- mx2df(y)

y_init <- y_df %>%
  group_by(r) %>%
  summarise(init = min(y)) %>%
  select(init)

#################
### plot data from y_df
################

# size v time
y_df %>%
  ggplot(aes(c, y, color = as.factor(r))) +
  geom_line(alpha = 0.8) +
  theme_minimal() +
  theme(legend.position = "none")

# growth v previous size
y_df %>%
  ggplot(aes(prev_sz, grw, color = as.factor(r))) +
  geom_line(alpha = 0.5) +
  theme_minimal() +
  theme(legend.position = "none")

