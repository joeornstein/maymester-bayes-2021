## Suppose we were missing data from the corruption dataset
## Can Bayesian models help???

## Sure we can!!


library(tidyverse)
library(rethinking)

d <- read_csv('data/clean_data.csv') %>% 
  # normalize gdp per capita
  mutate(log_gdp = log2(gdp_per_capita),
         log_gdp_demeaned = scale(log_gdp),
         log_gdp_demeaned2 = log_gdp_demeaned ^ 2,
         log_gdp_demeaned3 = log_gdp_demeaned ^ 3) %>% 
  # create an index variable for regime
  mutate(regime = if_else(democracy == 1, 2, 1)) %>% 
  # filter out the missing democracy values
  filter(!is.na(regime),
         !is.na(cpi_score))

## Fit a model without the missing values -------------------------

m1 <- quap(
  alist(
    cpi_score ~ dnorm( mu, sigma ),
    mu <- a[regime] + b*log_gdp_demeaned,
    a[regime] ~ dnorm(50, 10),
    b ~ dnorm(10, 5),
    sigma ~ dunif(0, 25)
  ), data = d %>% na.omit
)

precis(m1, depth = 2)

## One cool trick: treat the missing values as parameters to be estimated ---------------

dat <- list(
  cpi_score = d$cpi_score,
  regime = d$regime,
  log_gdp_demeaned = d$log_gdp_demeaned
)

m2 <- ulam(
  alist(
    cpi_score ~ dnorm( mu, sigma ),
    mu <- a[regime] + b*log_gdp_demeaned,
    a[regime] ~ dnorm(50, 10),
    b ~ dnorm(10, 5),
    
    # HERE'S THE CHANGE
    log_gdp_demeaned ~ dnorm(nu, sigma_gdp),
    nu ~ dnorm(0, 0.5),
    sigma_gdp ~ dexp(1),
    
    sigma ~ dunif(0, 25)
  ), data = dat
)

precis(m2, depth = 2)

## Let's see how it did.....

posterior <- extract.samples(m2)

# append the imputed values
d2 <- d %>% 
  filter(is.na(log_gdp_demeaned)) %>% 
  mutate(imputed_log_gdp_mean = apply(posterior$log_gdp_demeaned_impute, 2, mean),
         imputed_log_gdp_PI_lower = apply(posterior$log_gdp_demeaned_impute, 2, PI, prob = 0.95)[1,],
         imputed_log_gdp_PI_upper = apply(posterior$log_gdp_demeaned_impute, 2, PI, prob = 0.95)[2,])
