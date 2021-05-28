## Adding variables to our Bayesian linear regression

library(tidyverse)
library(rethinking)

## Load Data -----------------------

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

## Remind ourselves what the one variable model looked like ---------

m1 <- quap(
  alist(
    cpi_score ~ dnorm( mu, sigma ),
    mu <- a + b*log_gdp_demeaned + b2*log_gdp_demeaned2 + b3*log_gdp_demeaned3,
    a ~ dnorm(50, 10),
    b ~ dnorm(10, 5),
    b2 ~ dunif(-5, 5),
    b3 ~ dunif(-5, 5),
    sigma ~ dunif(0, 25)
  ), data = d
)

summary(m1)

## Plot our posterior predictions --------------

mu <- link( m1, d )
simulated_cpi <- sim( m1, d )

p1 <- d %>% 
  # add predictions and posterior intervals to dataframe
  mutate(prediction = apply(mu, 2, mean),
         PI_lower = apply(mu, 2, PI, prob = 0.95)[1,],
         PI_upper = apply(mu, 2, PI, prob = 0.95)[2,],
         sim_lower = apply(simulated_cpi, 2, PI, prob = 0.95)[1,],
         sim_upper = apply(simulated_cpi, 2, PI, prob = 0.95)[2,]) %>% 
  ggplot() + 
  # raw data
  geom_point(mapping = aes(x=log_gdp_demeaned,
                           y=cpi_score,
                           color = factor(regime)),
             alpha = 0.5) +
  # MAP (mean posterior estimate of mu)
  geom_line(mapping = aes(x = log_gdp_demeaned,
                          y = prediction)) +
  # Add posterior interval around mu (uncertainty re: mu)
  geom_ribbon(mapping = aes(x = log_gdp_demeaned,
                            ymax = PI_upper,
                            ymin = PI_lower),
              alpha = 0.5) +
  # Add posterior predictive interval (uncertainty re: CPI)
  geom_ribbon(mapping = aes(x = log_gdp_demeaned,
                            ymax = sim_upper,
                            ymin = sim_lower),
              alpha = 0.2) +
  # making it pretty
  theme_bw() +
  labs(title = 'Posterior Predictions of Corruption',
       x = 'Log GDP Per Capita (Demeaned)',
       y = 'Corruption Perceptions Index',
       color = 'Regime Type')
p1


## Add Regime Type to the model ------------------------------

# come up with some sensible priors
# cpi_score ~ Normal( mu, sigma )
# mu_i = a[regime]_i + b*GDP + b2*GDP^2 + b3*GDP^3
# b priors can stay the same
# a_1 ~ Normal(50, 10)
# a_2 ~ Normal(50, 10)

m2 <- quap(
  alist(
    cpi_score ~ dnorm( mu, sigma ),
    mu <- a[regime] + b*log_gdp_demeaned + b2*log_gdp_demeaned2 + b3*log_gdp_demeaned3,
    a[regime] ~ dnorm(50, 10),
    # do this if you want separate priors for each intercept
    #a[1] ~ dnorm(50, 10),
    #a[2] ~ dnorm(50,10),
    b ~ dnorm(10, 5),
    b2 ~ dunif(-5, 5),
    b3 ~ dunif(-5, 5),
    sigma ~ dunif(0, 25)
  ), data = d
)

precis(m2, depth = 2)

## Compare posterior distributions for a_1 and a_2 ---------------

# draw 10,000 samples
posterior <- extract.samples(m2)

# get the difference between democracies and autocracies
difference <- posterior$a[,2] - posterior$a[,1]

# plot the difference
dens(difference)

# posterior interval
PI(difference, prob = 0.95)

# probability that the difference is negative
sum(difference < 0) / length(difference) # ZERO

## Question 1: Heteroskedasticity? --------------------

# draw 10,000 samples from the posterior, estimate mu and simulate CPI
mu <- link( m2, d )
simulated_cpi <- sim( m2, d )

d %>% 
  mutate(prediction = apply(mu, 2, mean),
         residual = cpi_score - prediction) %>% 
  ggplot() +
  # add residual points
  geom_point(mapping = aes(x=log_gdp_demeaned, y = residual)) +
  # make it pretty
  theme_bw() +
  labs(title = 'Prediction Errors',
       y = 'Residual',
       x = 'Log GDP Per Capita (Demeaned)') +
  # add zero line
  geom_hline(yintercept = 0, linetype = 'dashed')
  
## Question 2: Confounding? ------------------

# does conditioning on regime type attenuate the correlation between
# GDP per capita and CPI?

# previous model
summary(m1)

# new model
summary(m2)

# how confident are we that beta is positive?
posterior <- extract.samples(m2)
sum(posterior$b < 0) / length(posterior)

## Plot the posterior predictive intervals -------------------

mu <- link( m2, d )
simulated_cpi <- sim( m2, d )

p2 <- d %>% 
  # add predictions and posterior intervals to dataframe
  mutate(regime_type = if_else(democracy == 1, 'Democracy', 'Autocracy'),
         prediction = apply(mu, 2, mean),
         PI_lower = apply(mu, 2, PI, prob = 0.95)[1,],
         PI_upper = apply(mu, 2, PI, prob = 0.95)[2,],
         sim_lower = apply(simulated_cpi, 2, PI, prob = 0.95)[1,],
         sim_upper = apply(simulated_cpi, 2, PI, prob = 0.95)[2,]) %>% 
  ggplot() + 
  # raw data
  geom_point(mapping = aes(x=log_gdp_demeaned,
                           y=cpi_score,
                           color = regime_type),
             alpha = 0.5) +
  # MAP (mean posterior estimate of mu)
  geom_line(mapping = aes(x = log_gdp_demeaned,
                          y = prediction,
                          color = regime_type)) +
  # Add posterior interval around mu (uncertainty re: mu)
  geom_ribbon(mapping = aes(x = log_gdp_demeaned,
                            ymax = PI_upper,
                            ymin = PI_lower,
                            fill = regime_type),
              alpha = 0.5) +
  # Add posterior predictive interval (uncertainty re: CPI)
  geom_ribbon(mapping = aes(x = log_gdp_demeaned,
                            ymax = sim_upper,
                            ymin = sim_lower,
                            fill = regime_type),
              alpha = 0.2) +
  # making it pretty
  theme_bw() +
  labs(title = 'Posterior Predictions of Corruption',
       x = 'Log GDP Per Capita (Demeaned)',
       y = 'Corruption Perceptions Index',
       color = 'Regime Type',
       fill = 'Regime Type')
p2


## What would the frequentist model tell us? -----------------

# bayesian linear regression gave us these posteriors
precis(m2, depth = 2)


# frequentist model w/ lm()
lm1 <- lm(cpi_score ~ log_gdp_demeaned + 
            log_gdp_demeaned2 + log_gdp_demeaned3 +
            factor(democracy),
          data = d)
summary(lm1)

