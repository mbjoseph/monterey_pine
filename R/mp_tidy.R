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
  ggplot(aes(log(prev_sz), grw, color = as.factor(timelag))) +
  geom_point(alpha = 0.5, size = 2) + 
  facet_grid(id ~ .) +
  theme_minimal()


fate_df %>%
  filter(timelag == 2) %>%
  ggplot(aes(as.factor(wx), grw)) +
  geom_jitter(width = 0.1, alpha = 0.2, size = 2) + 
  facet_grid(id ~ .) +
  theme_minimal()

lm(grw ~ prev_sz + as.factor(wx), data = fate_df) %>% rethinking::precis()

## long form database reformat
# d <- fate_df %>%
#   mutate(id = as.numeric(rownames(fate_df))) %>%
#   gather(1:14, key = "year", value = "dbh") %>%
#   mutate(year = as.numeric(year), id = as.factor(id), dbh = round(dbh, 2)) %>%
#   arrange(id) %>%
#   na.omit()
# 
# df <- filter(d, !d[,1] %in% which(as.vector(table(d$id) == 1))) %>%
#   group_by(id) %>% 
#   mutate(prev_dbh = lag(dbh), x = lag(year), delta = (year - x)-1) %>%
#   select(id, t = year, delta, dbh, prev_dbh) %>%
#   na.omit()
# 
