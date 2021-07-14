#######################################
### Fit all random-intercept models ###
#######################################

library(tidyverse)
library(brms)

# Function with appropriate priors to fit to models
brmsModel <- function(df){
  brm(data = df, family = binomial,
      n|trials(N) ~ (1|study), 
      prior = c(prior(normal(0, 2), class = Intercept),
                prior(cauchy(0, 1), class = sd)),
      iter = 1000, warmup = 200, cores = 4, chains = 2,
      control = list(adapt_delta = .99),
      seed = 14)
}

# Fitting the models ------------------------------------------------------

completeDF <-
  # Read in data
  read.csv("allData.csv") %>%
  # Prep variables for model fitting
  select(-c(Lifetime, Adulthood)) %>% 
  mutate(study = paste0(str_to_lower(Population), "_", Author)) %>% 
  pivot_longer(c(Past.year, Adult), names_to = "period", values_to = "prev") %>%
  mutate(n = round((prev/100)*N)) %>%
  drop_na(prev) %>%
  # Group by sample type
  group_by(Population, period) %>%
  nest() %>% 
  # Include populations with enough studies (>3 studies)
  filter(Population == "Mixed" | Population == "SA" | 
         Population == "IPVmix" & period == "Adult" |
         Population == "Psychosis" & period == "Adult"
         ) %>% 
  mutate(title = paste0("Sexual abuse prevalence in ", Population, " samples during ", period)) %>% 
  select(Population, period, title, data) %>%
  # Fit model for each sample type
  mutate(mod = map(data, brmsModel))

# Save
saveRDS(completeDF, "models_DF.RData")


