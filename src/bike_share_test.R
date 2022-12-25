# Test case for latent AR(1) models in brms using records from the Capital Bikeshare
# program in Washington, D.C.
# Data available from https://www.kaggle.com/datasets/contactprad/bike-share-daily-data
# The data record the number of registered, non-registered ("casual"), and total
# riders on each day of the year in 2011-2012, along with some basic weather information.
# The goal is to model the number of riders per day.
library(brms)
library(tidyverse)

t <- Sys.time()

bikes <- readr::read_csv('data/bike_share_day.csv')

# To save computation time, we cut the training data down to the first
# 100 days of observations.
truncated <- bikes %>% filter(instant <= 100)

# Use temperature and windspeed as predictors, plus ar(1) to model
# the background use rate
model <- brm(
  cnt ~ 1 + windspeed + temp + ar(p=1, time = instant, latent = T),
  data = truncated,
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 1000,
  control = list(adapt_delta = 0.97, max_treedepth = 13),
  save_pars = save_pars(all = T)
)

# Extend the data frame by the next 20 days of predictors / time points
truncated_ext <- bikes %>% 
  filter(instant <= 120) %>% 
  mutate(cnt = c(truncated$cnt,
                 rep(NA, 20)))

system.time(preds <- fitted(model, truncated_ext))

p <- cbind(truncated_ext, preds) %>% ggplot() +
  geom_point(aes(x=instant, y=cnt)) +
  geom_line(aes(x=instant, y=Estimate)) +
  geom_ribbon(aes(x=instant, ymin=Q2.5, ymax=Q97.5), color = NA, alpha = 0.2)

ggsave('plots/bikes/bikes_simple.png', p, width = 7, height = 5)

# Next, we test the ability to interpolate a gap in the time series.
# First we create a subset of the data with 10 entries missing.
truncated_gap <- bikes %>% 
  filter(instant <= 100 | instant %in% seq(121, 150))

# We also split by type of day (working or non-working). Registered riders
# ride more frequently on working days (presumably, they registered to use
# the bike share for commuting). The relationship is reversed for casual riders.
reg_model <- brm(
  registered ~ 1 + windspeed + temp + 
    ar(p=1, time = instant, gr = workingday, latent = T),
  data = truncated_gap,
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 1000,
  control = list(adapt_delta = 0.97, max_treedepth = 13),
  save_pars = save_pars(all = T)
)

# Create a data frame with rows for the gap to be filled, and a little
# forecasting for good measure
truncated_fill <- bikes %>% filter(instant <= 160) %>% 
  mutate(registered = c(bikes$registered[1:100],
                    rep(NA, 20),
                    bikes$registered[121:150],
                    rep(NA, 10)))

system.time(preds <- fitted(reg_model, truncated_fill))

p <- cbind(truncated_fill, preds) %>% ggplot() +
  geom_point(aes(x=instant, y=registered, color = factor(workingday))) +
  geom_line(aes(x=instant, y=Estimate)) +
  geom_ribbon(aes(x=instant, ymin=Q2.5, ymax=Q97.5), color = NA, alpha = 0.2) +
  guides(color=guide_legend(title="workday"))

ggsave('plots/bikes/bikes_gap.png', p, width=7, height=5)

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
  control = list(adapt_delta = 0.97, max_treedepth = 13),
  save_pars = save_pars(all = T)
)

truncated_weekly_ext <- bikes %>% filter(week <= 30) %>% 
  mutate(registered = c(truncated_weekly$registered,
                        rep(NA, 28)))

system.time(preds <- fitted(weekly_model, truncated_weekly_ext))

# Weekly plot, split by working/non-working days
p <- cbind(truncated_weekly_ext, preds) %>% ggplot() +
  geom_point(aes(x=week, y=registered, color = factor(workingday))) +
  geom_line(aes(x=week, y=Estimate, color = factor(workingday))) +
  guides(color=guide_legend(title="workday"))

ggsave('plots/bikes/bikes_weekly.png', p, width = 7, height = 5)

# Split out into daily observations
p <- cbind(truncated_weekly_ext, preds) %>% ggplot() +
  geom_point(aes(x=instant, y=registered, color = factor(workingday))) +
  geom_line(aes(x=instant, y=Estimate, color = factor(workingday))) +
  guides(color=guide_legend(title="workday"))

ggsave('plots/bikes/bikes_weekly_disagg.png', p, width = 7, height = 5)

# Finally, adding back in the linear predictors:

weekly_model <- brm(
  registered ~ 1 + windspeed + temp + 
    ar(p=1, time = week, gr = workingday, latent = T),
  data = truncated_weekly,
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 1000,
  control = list(adapt_delta = 0.97, max_treedepth = 13),
  save_pars = save_pars(all = T)
)

system.time(preds <- fitted(weekly_model, truncated_weekly_ext))

# Weekly plot, split by working/non-working days
p <- cbind(truncated_weekly_ext, preds) %>% ggplot() +
  geom_point(aes(x=week, y=registered, color = factor(workingday))) +
  geom_line(aes(x=week, y=Estimate, color = factor(workingday))) +
  guides(color=guide_legend(title="workday"))

ggsave('plots/bikes/bikes_weekly_weather.png', p, width = 7, height = 5)

# Split out into daily observations
p <- cbind(truncated_weekly_ext, preds) %>% ggplot() +
  geom_point(aes(x=instant, y=registered, color = factor(workingday))) +
  geom_line(aes(x=instant, y=Estimate, color = factor(workingday))) +
  guides(color=guide_legend(title="workday"))

ggsave('plots/bikes/bikes_weekly_weather_disagg.png', p, width = 7, height = 5)

Sys.time() - t