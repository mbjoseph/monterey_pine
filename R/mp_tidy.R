library(tidyverse)
library(rethinking)
source("./R/mp_load.R")
source("./R/fn_mxdf.R")

###########
# functions
###########

# mp_fun: replace zeroes and negative values with NAs, remove cases with
# less than two observations

mp_fun <- function(x) {
  m <- ifelse(x <= 0, NA, x)
  n <- apply(m, 1, function(w) sum(!is.na(w)) > 1) %>% which()
  m[n,]
}

 
########################
########################

fate <- lapply(fate, FUN = mp_fun)
fate_l <- lapply(fate, FUN = mx2df)


  y_init <- y_df %>%
  group_by(r) %>%
  summarise(init = min(y)) %>%
  select(init)



#############
############

fate_df <- bind_rows(fate_l, .id = "id")

fate_df %>%
  filter(timelag < 3) %>%
  ggplot(aes(log(prev_sz), log(grw), color = as.factor(timelag))) +
  geom_point(alpha = 0.5, size = 2) + 
  facet_grid(id ~ .) +
  theme_minimal()


fate_df %>%
  filter(timelag == 2) %>%
  ggplot(aes(as.factor(wx), log(grw))) +
  geom_jitter(width = 0.1, alpha = 0.2, size = 2) + 
  facet_grid(id ~ .) +
  theme_minimal()

fate_df %>%
  filter(id == 'ef') %>%
  ggplot(aes(c, log(y), color = as.factor(r))) +
  geom_line(alpha = 0.8) +
  theme_minimal() +
  theme(legend.position = "none")

lm(grw ~ prev_sz + as.factor(wx), data = fate_df) %>% rethinking::precis()
