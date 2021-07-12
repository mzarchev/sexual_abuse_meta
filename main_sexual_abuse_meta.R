#######################################
### Fit all random-intercept models ###
#######################################

library(tidyverse)
library(brms)

# Function with appropriate priors to fit to models
brmsModel <- function(df){
  brm(data = df, family = binomial,
      n|trials(N) ~ (1|Author), 
      prior = c(prior(normal(0, 2), class = Intercept),
                prior(cauchy(0, 1), class = sd)),
      iter = 10000, warmup = 2000, cores = 4, chains = 4,
      control = list(adapt_delta = .99),
      seed = 14)
}


# Fitting the models ------------------------------------------------------

# Fit for all sample types
completeDF <-
  read.csv("allData.csv") %>%  
  select(-c(Lifetime, Adulthood)) %>% 
  mutate(study = paste0(str_to_lower(Population), "_", Author)) %>% 
  pivot_longer(c(Past.year, Adult), names_to = "period", values_to = "prev") %>%
  group_by(Group, period) %>%
  mutate(n = round((prev/100)*N)) %>%
  drop_na(prev) %>% 
  nest() %>% 
  mutate(title = paste0("Sexual abuse prevalence in ", Group, " samples during ", period)) %>% 
  select(Group, period, title, data) %>%
  mutate(mod = data %>% map(brmsModel))

# Filter only populations with enough studies (>3 studies)
models_DF <- 
  completeDF %>%
  ungroup() %>% 
  filter(Group == "Mixed" | Group == "SA" | Group == "IPVmix" & period == "Adult") %>% 
  mutate(plot = map(fit, forest_plot)) %>% 
  select(-plotForest)

# Save
saveRDS(models_DF, "models_DF.RData")


