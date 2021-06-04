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
                       region == 'West' ~ 4))

# convert the data to a list for ulam()
dat <- list(
  Y = d$Y, # outcome variable (Democratic vote in 2016)
  R = d$R # index variable for region
)


## Fit model ----------------------------

m1 <- ulam(
  alist(
    Y ~ dbinom(1, p), # binomial likeliood
    logit(p) <- a[R], # log-odds vary by region
    a[R] ~ dnorm(0, 1.5) # sensible priors for log-odds (see page 336)
  ), data = dat, chains = 1
)

# summarize the output
precis(m1, depth = 2)

# MCMC diagnostics
traceplot( m1 )


## Plot posterior probabilities ------------------------------

# draw samples from the posterior
posterior <- extract.samples(m1)

# take the region-intercept samples
posterior$a %>% 
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
