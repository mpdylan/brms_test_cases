# Synthetic data tests of the new AR(1) correlation structure.
# Includes tests of a gaussian model and a binomial model.

library(brms)
library(tidyverse)
library(gtools)

# Generating the data. We're working with 4 setups: a simple time interval,
# a time series that skips several time points, one that has multiple observations
# at the same time point, and one that has both of those features.

set.seed(24601)

t <- Sys.time()

# Simple time interval
t1 <- seq(1, 24)

# Interval with a gap
t2 <- c(seq(1, 12), seq(16, 27))

# Some multiple observations
t3 <- c(seq(1, 6), 6, 6, seq(7, 12), 12, 12, 12, seq(13, 19))

# Multiple observations and a gap
t4 <- c(seq(1, 6), 6, 6, 6, seq(7, 11), seq(14, 23))

# Generate "true" parameter values, even for skipped time points
mu_true_1 <- 2 + arima.sim(list(ar=0.8), 27, sd = 0.2)
mu_true_2 <- 2 + arima.sim(list(ar=0.8), 27, sd = 0.2)
mu_true_3 <- 2 + arima.sim(list(ar=0.8), 27, sd = 0.2)
mu_true_4 <- 2 + arima.sim(list(ar=0.8), 27, sd = 0.2)

# Observed y values
y1 <- 2 + rnorm(24, mean = mu_true_1[t1], sd = 0.2)
y2 <- 2 + rnorm(24, mean = mu_true_2[t2], sd = 0.2)
y3 <- 2 + rnorm(24, mean = mu_true_3[t3], sd = 0.2)
y4 <- 2 + rnorm(24, mean = mu_true_4[t4], sd = 0.2)

dt <- data.frame(t=c(t1, t2, t3, t4),
                 y=c(y1, y2, y3, y4), grp = c(rep(1, 24),
                                              rep(2, 24), 
                                              rep(3, 24),
                                              rep(4, 24)))

# Formulas

fmla <- bf(y ~ 1 + ar(p=1, gr = grp, time = t, latent = T))

# New data frames for prediction. These contain the old data
# and some placeholder rows for points to be filled in and forecast points.
newdf_1 <- rbind(dt[1:24,], data.frame(t=seq(25, 36), y=NA, grp=1))
newdf_2 <- rbind(dt[25:48,], data.frame(t=c(13, 14, 15, seq(28, 36)), y=NA, grp = 2))
newdf_3 <- rbind(dt[49:72,], data.frame(t=seq(20, 31), y=NA, grp=3))
newdf_4 <- rbind(dt[73:96,], data.frame(t=c(seq(12, 13), seq(24, 33)), y=NA, grp=4))

fit_latent <- brm(
  fmla,
  dt,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99, max_treedepth=13),
  save_pars = save_pars(all=TRUE)
)

preds_1 <- fitted(fit_latent, newdf_1)
preds_2 <- fitted(fit_latent, newdf_2)
preds_3 <- fitted(fit_latent, newdf_3)
preds_4 <- fitted(fit_latent, newdf_4)

preds_plot_1 <- cbind(newdf_1, preds_1)
preds_plot_2 <- cbind(newdf_2, preds_2)
preds_plot_3 <- cbind(newdf_3, preds_3)
preds_plot_4 <- cbind(newdf_4, preds_4)

p <- rbind(preds_plot_1, preds_plot_2, preds_plot_3, preds_plot_4) %>% 
  ggplot() + 
  geom_point(aes(x=t, y=y)) + 
  geom_line(aes(x=t, y=Estimate)) + 
  geom_ribbon(aes(x=t, ymin=Q2.5, ymax=Q97.5), color = NA, alpha = 0.2) + 
  facet_wrap(~grp, nrow=2,ncol=2)

ggsave('plots/synthetic/synthetic_gaussian.png', p, width = 9, height = 7)

# With a binomial model

p_true_1 <- inv.logit(arima.sim(list(ar=0.8), 27, sd = 0.2))
p_true_2 <- inv.logit(arima.sim(list(ar=0.8), 27, sd = 0.2))
p_true_3 <- inv.logit(arima.sim(list(ar=0.8), 27, sd = 0.2))
p_true_4 <- inv.logit(arima.sim(list(ar=0.8), 27, sd = 0.2))

n <- 50

y1 <- rbinom(24, size = n, prob = p_true_1[t1])
y2 <- rbinom(24, size = n, prob = p_true_2[t2])
y3 <- rbinom(24, size = n, prob = p_true_3[t3])
y4 <- rbinom(24, size = n, prob = p_true_4[t4])

dt <- data.frame(t=c(t1, t2, t3, t4),
                 y=c(y1, y2, y3, y4), 
                 grp = c(rep(1, 24),
                         rep(2, 24), 
                         rep(3, 24),
                         rep(4, 24)),
                 n=n)

fmla <- bf(y | trials(n) ~ 1 + ar(p=1, gr = grp, time = t, latent = T))

# New data frames for prediction. These contain the old data
# and some placeholder rows for points to be filled in and forecast points.
newdf_1 <- rbind(dt[1:24,], data.frame(t=seq(25, 36), y=NA, grp=1, n=n))
newdf_2 <- rbind(dt[25:48,], data.frame(t=c(13, 14, 15, seq(28, 36)), y=NA, grp=2, n=n))
newdf_3 <- rbind(dt[49:72,], data.frame(t=seq(20, 31), y=NA, grp=3, n=n))
newdf_4 <- rbind(dt[73:96,], data.frame(t=c(seq(12, 13), seq(24, 33)), y=NA, grp=4, n=n))

fit_latent <- brm(
  fmla,
  dt,
  family = binomial(),
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99, max_treedepth=13),
  save_pars = save_pars(all=TRUE)
)

preds_1 <- fitted(fit_latent, newdf_1)
preds_2 <- fitted(fit_latent, newdf_2)
preds_3 <- fitted(fit_latent, newdf_3)
preds_4 <- fitted(fit_latent, newdf_4)

preds_plot_1 <- cbind(newdf_1, preds_1)
preds_plot_2 <- cbind(newdf_2, preds_2)
preds_plot_3 <- cbind(newdf_3, preds_3)
preds_plot_4 <- cbind(newdf_4, preds_4)

preds_plot_1 %>% ggplot() + 
  geom_point(aes(x=t, y=y)) + 
  geom_line(aes(x=t, y=Estimate)) + 
  geom_ribbon(aes(x=t, ymin=Q2.5, ymax=Q97.5), color = NA, alpha = 0.2)

preds_plot_2 %>% ggplot() + 
  geom_point(aes(x=t, y=y)) + 
  geom_line(aes(x=t, y=Estimate)) + 
  geom_ribbon(aes(x=t, ymin=Q2.5, ymax=Q97.5), color = NA, alpha = 0.2)

preds_plot_3 %>% ggplot() + 
  geom_point(aes(x=t, y=y)) + 
  geom_line(aes(x=t, y=Estimate)) + 
  geom_ribbon(aes(x=t, ymin=Q2.5, ymax=Q97.5), color = NA, alpha = 0.2)

preds_plot_4 %>% ggplot() +
  geom_point(aes(x=t, y=y)) +
  geom_line(aes(x=t, y=Estimate)) +
  geom_ribbon(aes(x=t, ymin=Q2.5, ymax=Q97.5), color = NA, alpha = 0.2)

p <- rbind(preds_plot_1, preds_plot_2, preds_plot_3, preds_plot_4) %>% 
  ggplot() + 
  geom_point(aes(x=t, y=y)) + 
  geom_line(aes(x=t, y=Estimate)) + 
  geom_ribbon(aes(x=t, ymin=Q2.5, ymax=Q97.5), color = NA, alpha = 0.2) + 
  facet_wrap(~grp, nrow=2,ncol=2)

ggsave('plots/synthetic/synthetic_binomial.png', p, width = 9, height = 7)

elapsed <- Sys.time() - t