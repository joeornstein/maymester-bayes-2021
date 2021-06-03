## Fitting the language diversity model with MCMC

library(tidyverse)
library(rethinking)


## Load and clean up the data ---------------------------

data(nettle)
d <- nettle %>% 
  mutate(Y = log2(num.lang / k.pop),
         # center predictors
         M = mean.growing.season - mean(mean.growing.season),
         S = sd.growing.season - mean(sd.growing.season),
         A = log2(area) - mean(log2(area)))

# Convert data into a list for ulam()
dat <- list(
  Y = d$Y,
  M = d$M,
  S = d$S,
  A = d$A
)

# first fit with quap() to compare
m1 <- quap(
  alist(
    Y ~ dnorm(mu, sigma),
    mu <- a + bM*M + bA*A,
    a ~ dnorm(-8, 2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 1),
    sigma ~ dexp(0.25)
  ), data = d
)

## Sample from the posterior with ulam() -----------------------

# feed it the same model, but use the cleaned up list for data argument
m2 <- ulam(
  alist(
    Y ~ dnorm(mu, sigma),
    mu <- a + bM*M + bA*A,
    a ~ dnorm(-8, 2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 1),
    sigma ~ dexp(0.25)
  ), data = dat, chains = 1
)

precis(m1, depth = 2)
precis(m2, depth = 2)


## Some diagnostics -------------------

pairs(m2) 
# The MCMC samples look pretty Gaussian, which is why it agrees with quap()

traceplot( m2 )
# The chain started off in a weird place (thanks to our uninformative priors)
# but it quickly converged

