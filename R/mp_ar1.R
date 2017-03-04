library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)

stan_d <- list(n = nrow(fate),
               k = length(y_df$y),
               n_t = ncol(fate),
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