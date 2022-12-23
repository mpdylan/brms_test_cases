# Demonstrating explicit latent parameterization of arma(1,1) residuals
# in a Poisson model, using weekly counts of chickenpox cases in several
# Hungarian cities. Data source: UCI Machine Learning Repository
# https://archive.ics.uci.edu/ml/datasets/Hungarian+Chickenpox+Cases

library(brms)
library(tidyverse)
library(reshape)

# Utility function for transforming the dates into integer time codes
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

set.seed(1953)

t <- Sys.time()

pox <- readr::read_csv('data/hungary_chickenpox.csv')

# Select one year of data from each of three cities, to reduce computation
# Start one year in so we can test backcasting
pox_truncated <- reshape::melt(as.data.frame(pox), variable_name = "City", id.vars = "Date") %>% 
  mutate(week_t = encode_ordinal(Date)) %>%
  filter(City %in% c('BUDAPEST', 'KOMAROM', 'CSONGRAD'), week_t <= 104, week_t >= 53) 

# Fit model
model <-
  brm(
    value ~ (1 | City) + arma(p=1, q=1, time = week_t, gr = City, latent = T),
    data = pox_truncated,
    family = poisson(),
    chains = 4,
    cores = 4,
    warmup = 1000,
    iter = 2000,
    control = list(adapt_delta = 0.99, max_treedepth = 14),
    save_pars = save_pars(all = TRUE)
  )

# Prep data frame for predicts; 8 weeks back and 8 weeks ahead
pox_pred <- data.frame(week_t = c(45:112, 45:112, 45:112), 
                       City = c(rep('BUDAPEST', 68), rep('KOMAROM', 68), rep('CSONGRAD', 68)))

pred_for_plot <- cbind(pox_pred, fitted(model, newdata = pox_pred))

p <- pred_for_plot %>% ggplot() + 
  geom_line(aes(x=week_t, y=Estimate, color=City)) +
  geom_vline(aes(xintercept = 52), linetype = 'dashed') +
  geom_vline(aes(xintercept = 104), linetype = 'dashed')

ggsave('plots/chickenpox/chickenpox.png', p, width = 9, height = 7)

elapsed <- Sys.time() - t

# Can extract the latent parameters, split by city
ac_latent(model)
