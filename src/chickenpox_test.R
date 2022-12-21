library(brms)
library(tidyverse)
library(reshape)

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

set.seed(348)

pox <- readr::read_csv('data/hungary_chickenpox.csv')

mask <- as.logical(ifelse(runif(208) < 0.4, 0, 1))

pox_filtered <- reshape::melt(as.data.frame(pox), variable_name = "City", id.vars = "Date") %>% 
  mutate(week_t = encode_ordinal(Date)) %>%
  filter(City %in% c('BUDAPEST', 'KOMAROM'), DateCode <= 104) %>%
  filter(mask)

model <-
  brm(
    value ~ (1 | City) + ar(1, time = week_t, gr = City, latent = T),
    data = pox_filtered,
    family = poisson(),
    chains = 4,
    cores = 4,
    warmup = 1000,
    iter = 3000,
    control = list(adapt_delta = 0.98, max_treedepth = 12),
    save_pars = save_pars(all = TRUE)
  )

pox_pred <- reshape::melt(as.data.frame(pox), variable_name = "City", id.vars = "Date") %>% 
  mutate(DateCode = encode_ordinal(Date)) %>%
  filter(City %in% c('BUDAPEST', 'KOMAROM'), DateCode <= 104) %>%
  mutate(value = ifelse(mask, value, NA))

system.time(pred_for_plot <- cbind(pox_pred, fitted(model, newdata = pox_pred)))

pred_for_plot %>% ggplot() + 
  geom_point(aes(x=DateCode, y=value, color = City)) + 
  geom_line(aes(x=DateCode, y=Estimate, color=City)) 
  
