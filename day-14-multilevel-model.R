## Build a multilevel model predicting 2016 vote choice

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

## Fit the big multilevel model -------------------------------

starttime <- Sys.time() # start the clock

big_multilevel_model <- ulam(
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
    a7[state] ~ dnorm(s_mean, sigma), # adaptive prior
    s_mean ~ dnorm(0, 0.2),
    sigma ~ dexp(1),
    a8[religious_importance] ~ dnorm(0, 0.2),
    a9[urbancity] ~ dnorm(0, 0.2),
    a10[military_service] ~ dnorm(0, 0.2),
    a11[investor] ~ dnorm(0, 0.2),
    a12[homeowner] ~ dnorm(0, 0.2)
  ), data = dat, chains = 1, iter = 3000, log_lik = TRUE
)

# save to disk
save(big_multilevel_model, file = 'models/big_multilevel_model.RData')

stoptime <- Sys.time() # stop the clock
stoptime - starttime # took about 26 minutes to fit


## Diagnostics -----------------------------------------------

precis(big_multilevel_model, depth = 2)

traceplot(big_multilevel_model, pars = 'a3[1]')
traceplot(big_multilevel_model, pars = 'a6[2]')

plot(precis(big_multilevel_model, depth = 2, pars = 'a7'))


## Compare WAIC --------------------------------------------

load('models/big_model.RData')
load('models/big_multilevel_model.RData')

compare(big_model, big_multilevel_model) # big_multilevel_model has lower WAIC, but not by much!


## Compare model predictions -------------------------------------------------

# load and clean the test data
test <- 'data/CCES-Test.csv' %>%
  read_csv %>% 
  clean_CCES

p <- link(big_model, test)
p_multilevel <- link(big_multilevel_model, test)

test <- test %>% 
  mutate(p_no_pooling = apply(p, 2, mean),
         p_multilevel = apply(p_multilevel, 2, mean))

# Look at some predictions from a state with low sample size
test %>% 
  filter(abb == 'AK') %>% 
  select(p_no_pooling, p_multilevel)

# And with a large sample size
test %>% 
  filter(abb == 'CA') %>% 
  select(p_no_pooling, p_multilevel)

test %>%
  # compute the absolute difference between model predictions
  mutate(abs_difference = abs(p_no_pooling - p_multilevel)) %>% 
  # group by state
  group_by(abb) %>% 
  # get each state's sample size and the mean absolute difference between predictions
  summarize(sample_size = n(),
            mean_abs_difference = mean(abs_difference)) %>% 
  # plot it
  ggplot() +
  geom_text(aes(x=sample_size, y=mean_abs_difference, label = abb)) +
  theme_bw() +
  labs(x = 'Number of Respondents from State in CCES Test Set',
       y = 'Average Difference in Predictions')
