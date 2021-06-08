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

hist(
  inv_logit(
    rnorm(1e5, 0, 0.2) +
      rnorm(1e5, 0, 0.2) +
      rnorm(1e5, 0, 0.2) +
      rnorm(1e5, 0, 0.2) +
      rnorm(1e5, 0, 0.2) +
      rnorm(1e5, 0, 0.2) +
      rnorm(1e5, 0, 0.2) +
      rnorm(1e5, 0, 0.2) +
      rnorm(1e5, 0, 0.2) +
      rnorm(1e5, 0, 0.2) +
      rnorm(1e5, 0, 0.2) +
      rnorm(1e5, 0, 0.2)
  )
)


## Fit the big model -------------------------------

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
  ), data = dat, chains = 1, iter = 3000, log_lik = TRUE
)

# save to disk
save(big_model, file = 'models/big_model.RData')


## Diagnostics ----------------------------

traceplot( big_model )

# check on some of the posteriors
plot(precis(big_model, depth = 2, pars = 'a1' ))
plot(precis(big_model, depth = 2, pars = 'a6' ))
plot(precis(big_model, depth = 2, pars = 'a8' ))
plot(precis(big_model, depth = 2, pars = 'a10' ))
plot(precis(big_model, depth = 2, pars = 'a12' ))


## Append predictions to the test set --------------------------------

test <- 'data/CCES-Test.csv' %>%
  read_csv %>% 
  mutate(race_original = race) %>% 
  clean_CCES

p <- link( big_model, test )

test <- test %>% 
  mutate(p_democratic2016 = apply(p, 2, mean),
         PI_lower = apply(p, 2, PI, prob = 0.95)[1,],
         PI_upper = apply(p, 2, PI, prob = 0.95)[2,])

write_csv(test, path = 'data/CCES-Test-Predictions.csv')


## Load and evaluate predictions -----------------------

test <- read_csv('data/CCES-Test-Predictions.csv')

# posterior interval for caseid 410524330
test %>% 
  filter(caseid == 410524330) %>% 
  select(PI_lower, PI_upper)

# compute deviance
test <- test %>% 
  mutate(probability = democratic2016 * p_democratic2016 +
           (1 - democratic2016) * (1-p_democratic2016))

# sum the log probabilities and multiply by -2
sum(log(test$probability)) * -2

# calibration plot
test %>% 
  ggplot(mapping = aes(x=p_democratic2016,
                       y=democratic2016)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  theme_bw() + 
  labs(x = 'Predicted Probability of Voting Democratic',
       y = 'Voted Democratic?')


