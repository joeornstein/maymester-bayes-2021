## If our multilevel model can generate posterior predictions over outcomes,
## can it also generate posterior predictions over predictor variables?
## Yes it can. It's called Bayesian imputation.

library(tidyverse)
library(rethinking)

## Load and clean data --------------------------------------------

source('day-14-clean-cces.R') # a function to clean up CCES

# load and clean the training data
d <- 'data/CCES-Train.csv' %>%
  read_csv %>% 
  clean_CCES


## Simulate missing data -------------------

# suppose that some of our respondents were missing data on age

set.seed(42)
# randomly remove ~1% of the age values
d$age_std[ which( runif( nrow(d), 0, 1 ) < 0.01 ) ] <- NA


## Fit the big multilevel model -------------------------------

dat <- make_list_for_ulam(d) # put it into a nice list for ulam()

starttime <- Sys.time() # start the clock

bayesian_imputation_model <- ulam(
  alist(
    democratic2016 ~ dbinom(1, p), # binomial likelihood
    logit(p) <- b*age_std + a1[gender] + a2[has_children] + a3[employment_status] + a4[race] + a5[educ] + a6[LGBTQ] + a7[state] + a8[religious_importance] + a9[urbancity] + a10[military_service] + a11[investor] + a12[homeowner], # log-odds is a function of a bunch of varying intercepts
    
    # HERE'S THE CHANGE. Previously age_std was just a variable. 
    # Now, for some observations, it's a **parameter to be estimated**
    age_std ~ dnorm( nu, sigma ),
    nu ~ dnorm(0, 1),
    sigma ~ dexp(1),
    
    # Everything else is the same as before!
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
save(bayesian_imputation_model, file = 'models/bayesian_imputation_model.RData')

stoptime <- Sys.time() # stop the clock
stoptime - starttime # took about 28 minutes to fit


## Diagnostics -----------------------------------------------

traceplot(bayesian_imputation_model, 
          pars = c('a3[1]', 'a6[2]', 'age_std_impute[38]'))

plot(precis(bayesian_imputation_model, depth = 2, pars = 'a7'))

## Check out the imputed values  -----------------------------------

# sample from the posterior
posterior <- extract.samples(bayesian_imputation_model)

# get the mean and standard deviation of age in the original dataset
mean_age <- mean(2018 - d$birthyr)
sd_age <- sd(2018 - d$birthyr)

d2 <- d %>% 
  # get the observations with missing age
  filter(is.na(age_std)) %>% 
  # get the mean imputed age for each observation + 95% posterior intervals
  mutate(actual_age = 2018 - birthyr,
         imputed_age_std = apply(posterior$age_std_impute, 2, mean),
         imputed_age = imputed_age_std * sd_age + mean_age,
         imputed_age_PI_lower = apply(posterior$age_std_impute, 2, PI, prob = 0.95)[1,],
         imputed_age_PI_upper = apply(posterior$age_std_impute, 2, PI, prob = 0.95)[2,])

ggplot(d2, 
       aes(x=imputed_age, y=actual_age)) +
  geom_point() +
  theme_bw()
