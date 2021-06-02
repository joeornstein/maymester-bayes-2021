## Add varying slopes to our corruption model

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

## Estimate Varying Slopes Model ------------------

# start with linear version of the model
m1 <- quap(
  alist(
    cpi_score ~ dnorm( mu, sigma ),
    mu <- a[regime] + b[regime]*log_gdp_demeaned,
    a[regime] ~ dnorm(50, 10),
    b[regime] ~ dnorm(10, 5),
    sigma ~ dunif(0, 25)
  ), data = d
)

# inspect the posteriors
precis(m1, depth = 2)

# draw samples from the posterior
posterior <- extract.samples(m1)

# difference between beta_1 and beta_2
dens(posterior$b[,1] - posterior$b[,2])

## Plot the posterior over the data --------------------------

mu <- link( m1, d )
simulated_cpi <- sim( m1, d )

p1 <- d %>% 
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
p1

## Estimate (Cubic) Varying Slopes Model ------------------

# start with linear version of the model
m2 <- quap(
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

# inspect the posteriors
precis(m2, depth = 2)

## Plot the posterior over the data --------------------------

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
