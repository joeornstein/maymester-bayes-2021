## Let's build a logistic model to predict who votes for Clinton in 2016

# Your goal is to build a model using the 'CCES-Train' dataset with
# democratic2016 as the outcome.

# You'll summarize your model fit with posterior predictive plots,
# then use the posterior to predict Clinton support in a holdout sample
# 'CCES-Test' (no peeking!)

# Feel free to use whatever methods you have at your disposal to build
# a good model

# (NOTE: I've ignored sampling weights for the purpose of this exercise)

# Here's some code to get you started...

library(tidyverse)
library(rethinking)

## Load data -------------------------------------

d <- read_csv('data/CCES-Train.csv') %>% 
  # reformat variables for model
  mutate(Y = democratic2016,
         # index variable for region
         R = case_when(region == 'Midwest' ~ 1,
                       region == 'Northeast' ~ 2,
                       region == 'South' ~ 3,
                       region == 'West' ~ 4),
         sex = if_else(gender == 'Female', 1, 2))

# convert the data to a list for ulam()
dat <- list(
  Y = d$Y, # outcome variable (Democratic vote in 2016)
  R = d$R, # index variable for region
  sex = d$sex # index variable for sex
)


## Fit model ----------------------------

# here's what happened with I added more variables but didn't adjust my priors!!
m1 <- ulam(
  alist(
    Y ~ dbinom(1, p), # binomial likeliood
    logit(p) <- aR[R] + aS[sex], # log-odds vary by region and sex
    aR[R] ~ dnorm(0, 1.5),
    aS[sex] ~ dnorm(0, 1.5)
  ), data = dat, chains = 1
)

# huge posterior intervals, low n_eff, bad Rhat
precis(m1, depth = 2)

# wildly correlated parameters
pairs( m1 )

# patterns in the trace plot
traceplot( m1 )

# WHAT TO DO??

# Don't panic. The Folk Theorem of Statistical Computing (p. 299)
# suggests that it's just something wrong with your model.
# Probably your terrible priors.

priors <- extract.prior(m1)

# what's the prior distribution of p for a Southern Woman?
southern_women_prior <- inv_logit(priors$aR[,3] + priors$aS[,1])
dens(southern_women_prior, adj = 0.1)
# a bit wide!

# and the prior on the difference between Southern men and women?
southern_men_prior <- inv_logit(priors$aR[,3] + priors$aS[,2])
dens(southern_men_prior - southern_women_prior)
# centered on zero, but...pretty wide.
# could women really be 50% less likely than men to vote for Clinton?

## Try narrower priors and more iterations -------------------------

m2 <- ulam(
  alist(
    Y ~ dbinom(1, p), # binomial likeliood
    logit(p) <- aR[R] + aS[sex], # log-odds vary by region and sex
    aR[R] ~ dnorm(0, 0.2),
    aS[sex] ~ dnorm(0, 0.2)
  ), data = dat, chains = 1, iter = 3000
)

# posterior intervals reasonable
precis(m2, depth = 2)

# correlations less nuts
pairs( m2 )

# patterns gone
traceplot( m2 )

# what did those priors look like?
priors <- extract.prior(m2)

# what's the prior distribution of p for a Southern Woman?
southern_women_prior <- inv_logit(priors$aR[,3] + priors$aS[,1])
dens(southern_women_prior)

# and the prior on the difference between Southern men and women?
southern_men_prior <- inv_logit(priors$aR[,3] + priors$aS[,2])
dens(southern_men_prior - southern_women_prior)


## Plot posterior probabilities ------------------------------

# draw samples from the posterior
posterior <- extract.samples(m1)

# take the region-intercept samples
posterior$aR %>% 
  # apply inverse logit to convert log-odds into probabilities
  inv_logit %>% 
  # make it a dataframe
  as.data.frame %>% 
  # rename those index variables their original human-readable names
  set_names(c('Midwest', 'Northeast', 'South', 'West')) %>% 
  # pivot longer for plotting
  pivot_longer(cols = everything(),
               names_to = 'region',
               values_to = 'posterior_probability') %>% 
  ggplot() +
  # plot four densities for each region
  geom_density(aes(x = posterior_probability,
                 fill = region), alpha = 0.2) +
  # make it pretty
  theme_bw() + 
  labs(x = 'Posterior Probability',
       y = NULL,
       fill = 'Region')

# plot the sex-specific intercept posteriors
posterior$aS %>% 
  # apply inverse logit to convert log-odds into probabilities
  inv_logit %>% 
  # make it a dataframe
  as.data.frame %>% 
  # rename those index variables their original human-readable names
  set_names(c('Female', 'Male')) %>% 
  # pivot longer for plotting
  pivot_longer(cols = everything(),
               names_to = 'sex',
               values_to = 'posterior_probability') %>% 
  ggplot() +
  # plot four densities for each region
  geom_density(aes(x = posterior_probability,
                   fill = sex), alpha = 0.2) +
  # make it pretty
  theme_bw() + 
  labs(x = 'Posterior Probability',
       y = NULL,
       fill = 'Sex')



# NOTE: Once you make your model more complex, you may choose some
# other way to best plot your posterior fit

## Predict the test set p_i + 95% posterior interval -----------------------

# load and clean the test dataset the same way we did for the training set
test <- read_csv('data/CCES-Test.csv') %>% 
  # reformat variables for model
  mutate(# index variable for region
         R = case_when(region == 'Midwest' ~ 1,
                       region == 'Northeast' ~ 2,
                       region == 'South' ~ 3,
                       region == 'West' ~ 4))

# use the link() function to get 10,000 samples of p for each observation
posterior_p <- link(m1, data = test)

# add predictions and posterior intervals to the test set
test <- test %>% 
  mutate(p = apply(posterior_p, 2, mean),
         p_lower = apply(posterior_p, 2, PI, prob = 0.95)[1,],
         p_upper = apply(posterior_p, 2, PI, prob = 0.95)[2,])
