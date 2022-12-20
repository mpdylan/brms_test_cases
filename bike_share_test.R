library(tidyverse)

setwd('/home/dylan/Documents/code/brms/')
devtools::install()
library(brms)

setwd('../brms_testing/')
bikes <- readr::read_csv('bike_share_day.csv')

# To save computation time, we cut the training data down to the first
# 100 days of observations.
truncated <- bikes %>% filter(instant <= 100)


model <- brm(
  cnt ~ 1 + windspeed + temp + ar(p=1, time = instant, latent = T),
  data = truncated,
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 1000,
  control = list(adapt_delta = 0.98, max_treedepth = 13),
  save_pars = save_pars(all = T)
)

truncated_ext <- bikes %>% filter(instant <= 120) %>% mutate(cnt = c(truncated$cnt,
                                                                     rep(NA, 20)))

system.time(preds <- fitted(model, truncated_ext))

cbind(truncated_ext, preds) %>% ggplot() +
  geom_point(aes(x=instant, y=cnt)) +
  geom_line(aes(x=instant, y=Estimate))

# Next, we test the ability to interpolate a gap in the time series.
# First we create a subset of the data with 10 entries missing.
truncated_gap <- bikes %>% filter(instant <= 100 | instant %in% seq(111, 150))

# We also split by type of day (working or non-working). Registered riders
# ride more frequently on working days (presumably, they registered to use
# the bike share for commuting). The relationship is reversed for casual riders.
reg_model <- brm(
  registered ~ 1 + windspeed + temp + ar(p=1, time = instant, gr = workingday, latent = T),
  data = truncated_gap,
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 1000,
  control = list(adapt_delta = 0.98, max_treedepth = 13),
  save_pars = save_pars(all = T)
)

truncated_fill <- bikes %>% filter(instant <= 160) %>% 
  mutate(registered = c(bikes$registered[1:100],
                    rep(NA, 10),
                    bikes$registered[111:150],
                    rep(NA, 10)))

system.time(preds <- fitted(reg_model, truncated_fill))

cbind(truncated_fill, preds) %>% ggplot() +
  geom_point(aes(x=instant, y=registered, color = factor(workingday))) +
  geom_line(aes(x=instant, y=Estimate))

# Finally, test the functionality of multiple observations per timepoint.
# To do this, we aggregate by week. Most weeks have 5 working days and 2
# non-working days, but this is not universal.

bikes$week <- bikes$instant %/% 7 + 1

truncated_weekly <- bikes %>% filter(week <= 26)

# We drop the wind speed and temperature predictors for clarity of visualization
# (so that there is only one predicted value per week/group).
weekly_model <- brm(
  registered ~ 1 + ar(p=1, time = week, gr = workingday, latent = T),
  data = truncated_weekly,
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 1000,
  control = list(adapt_delta = 0.95, max_treedepth = 13),
  save_pars = save_pars(all = T)
)

truncated_weekly_ext <- bikes %>% filter(week <= 30) %>% 
  mutate(registered = c(truncated_weekly$registered,
                        rep(NA, 28)))

system.time(preds <- fitted(weekly_model, truncated_weekly_ext))

# Weekly plot, split by working/non-working days
cbind(truncated_weekly_ext, preds) %>% ggplot() +
  geom_point(aes(x=week, y=registered, color = factor(workingday))) +
  geom_line(aes(x=week, y=Estimate, color = factor(workingday)))

# Split out into daily observations
cbind(truncated_weekly_ext, preds) %>% ggplot() +
  geom_point(aes(x=instant, y=registered, color = factor(workingday))) +
  geom_line(aes(x=instant, y=Estimate, color = factor(workingday)))

