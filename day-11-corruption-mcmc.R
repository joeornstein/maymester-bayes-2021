# fit our corruption model with MCMC

library(tidyverse)
library(rethinking)

## Load Data -----------------

d <- read_csv('data/clean_data.csv') %>% 
  # drop observations with missing variables
  na.omit() %>% 
  # normalize gdp per capita
  mutate(log_gdp = log2(gdp_per_capita),
         log_gdp_demeaned = log_gdp - mean(log_gdp),
         log_gdp_demeaned2 = log_gdp_demeaned ^ 2,
         log_gdp_demeaned3 = log_gdp_demeaned ^ 3) %>% 
  # create an index variable for regime
  mutate(regime = if_else(democracy == 1, 2, 1))

# put the data into a list to make ulam() happy
dat <- list(
  cpi_score = d$cpi_score,
  regime = as.integer(d$regime),
  log_gdp_demeaned = d$log_gdp_demeaned,
  log_gdp_demeaned2 = d$log_gdp_demeaned2,
  log_gdp_demeaned3 = d$log_gdp_demeaned3
)

## Fit with quap() -------------------------------------

m1 <- quap(
  alist(
    cpi_score ~ dnorm( mu, sigma ),
    mu <- a[regime] + b[regime]*log_gdp_demeaned + b2[regime]*log_gdp_demeaned2 + b3[regime]*log_gdp_demeaned3,
    a[regime] ~ dnorm(50, 10),
    b[regime] ~ dnorm(10, 5),
    b2[regime] ~ dunif(-5, 5),
    b3[regime] ~ dunif(-5, 5),
    sigma ~ dunif(0, 25)
  ), data = d
)

## Fit with ulam() ----------------------------

m2 <- ulam(
  alist(
    cpi_score ~ dnorm( mu, sigma ),
    mu <- a[regime] + b[regime]*log_gdp_demeaned + b2[regime]*log_gdp_demeaned2 + b3[regime]*log_gdp_demeaned3,
    a[regime] ~ dnorm(50, 10),
    b[regime] ~ dnorm(10, 5),
    b2[regime] ~ dnorm(0, 2.5), #dunif(-5, 5),
    b3[regime] ~ dnorm(0, 2.5), #dunif(-5, 5),
    sigma ~ dunif(0, 25)
  ), data = dat, chains = 3, cores = 3, log_lik = TRUE
)

## Compare the results -------------------------------

precis(m1, depth = 2)
precis(m2, depth = 2)

## Diagnostics ------------------------------

traceplot( m2 )

trankplot( m2 )

compare(m1, m2)


## Compare cubic and linear model ----------------------

m3 <- quap(
  alist(
    cpi_score ~ dnorm( mu, sigma ),
    mu <- a[regime] + b[regime]*log_gdp_demeaned,
    a[regime] ~ dnorm(50, 10),
    b[regime] ~ dnorm(10, 5),
    sigma ~ dunif(0, 25)
  ), data = d
)

compare(m1, m3)
