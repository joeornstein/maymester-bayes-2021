## Fit a big logistic regression model to the CCES data

library(tidyverse)
library(rethinking)

## Load and clean data --------------------------------------------

source('day-14-clean-cces.R') # a function to clean up CCES

# load and clean the training data
d <- 'data/CCES-Train.csv' %>%
  read_csv %>% 
  clean_CCES

# put it into a nice list for ulam()
dat <- make_list_for_ulam(d)

## Prior predictive simulation ------------------------------


big_model <- ulam(
  alist(
    democratic2016 ~ dbinom(1, p), # binomial likelihood
    logit(p) <- b*age_std + a1[gender] + a2[has_children] + a3[employment_status] + a4[race] + a5[educ] + a6[LGBTQ] + a7[state] + a8[religious_importance] + a9[urbancity] + a10[military_service] + a11[investor] + a12[homeowner], # log-odds is a function of a bunch of varying intercepts
    b ~ dnorm(0, 0.2), # nice regularizing priors for each parameter
    a1[gender] ~ dnorm(0, 0.2),
    a2[has_children] ~ dnorm(0, 0.2),
    a3[employment_status] ~ dnorm(0, 0.2),
    a4[race] ~ dnorm(0, 0.2),
    a5[educ] ~ dnorm(0, 0.2),
    a6[LGBTQ] ~ dnorm(0, 0.2),
    a7[state] ~ dnorm(0, 0.2),
    a8[religious_importance] ~ dnorm(0, 0.2),
    a9[urbancity] ~ dnorm(0, 0.2),
    a10[military_service] ~ dnorm(0, 0.2),
    a11[investor] ~ dnorm(0, 0.2),
    a12[homeowner] ~ dnorm(0, 0.2)
  ), data = dat, chains = 1, sample_prior = TRUE # just sampling the prior!
)
