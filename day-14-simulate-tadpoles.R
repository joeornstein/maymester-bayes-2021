## Simulate tadpole mortality

library(tidyverse)
library(rethinking)

# hyperparameters
a_bar <- 1.5
sigma <- 1.5
nponds <- 60
Ni <- rep(20, nponds)

# simulate probability of survival in each pond
set.seed(42)
a_pond <- rnorm(nponds, a_bar, sigma)

# create a dataframe to hold our information
dsim <- data.frame(
  pond = 1:nponds, # pond id
  Ni = Ni, # number of tadpoles
  true_a = a_pond, # true log-odds of survival
  true_p = inv_logit(a_pond) # true probability of survival
)

# kill tadpoles
dsim$Si <- rbinom(n = nponds,
                  size = dsim$Ni,
                  prob = dsim$true_p)

dat <- list(
  Si = dsim$Si,
  Ni = dsim$Ni,
  pond = dsim$pond
)

# now fit some models
no_pooling_model <- ulam(
  alist(
    Si ~ dbinom( Ni, p ), # likelihood
    logit(p) <- a[pond], # link function
    a[pond] ~ dnorm(0, 1) # prior on a[pond]
  ), data = dat
)

# diagnostics
traceplot( no_pooling_model, 'a[2]' )

plot(no_pooling_model, depth = 2)

# fit multilevel model
multilevel_model <- ulam(
  alist(
    Si ~ dbinom( Ni, p ), # likelihood
    logit(p) <- a[pond], # link function
    a[pond] ~ dnorm(a_bar, sigma), # adaptive prior on a[pond]
    a_bar ~ dnorm(0, 1.5), # prior on a_bar
    sigma ~ dexp(1) # prior on sigma
  ), data = dat
)

traceplot( multilevel_model, 'a[2]' )

plot(multilevel_model, depth = 2)

## add posterior predictions to the dataframe

# predicted probability from no pooling model
no_pooling_posterior <- extract.samples(no_pooling_model)
multilevel_posterior <- extract.samples(multilevel_model)

dsim$no_pooling_p <- apply( inv_logit(no_pooling_posterior$a), 2, mean)
dsim$multilevel_p <- apply( inv_logit(multilevel_posterior$a), 2, mean)
dsim$empirical_p <- dsim$Si / dsim$Ni
dsim$complete_pooling_p <- sum(dsim$Si) / sum(dsim$Ni) # percent of all tadpoles that survived
