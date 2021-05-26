## Approximate Posterior Distribution using Quadratic Approximation

library(tidyverse)
library(rethinking)

## Load Data --------------------

d <- read_csv('data/cpi_gdp.csv') %>% 
  # remove countries with missing data
  na.omit() %>% 
  # standardize gdp per capita for ZJB model
  mutate(std_gdp = (gdp_per_capita - min(gdp_per_capita)) /
           (max(gdp_per_capita) - min(gdp_per_capita)))

## quap() ------------------------

# fit the model with quap()
m1 <- quap(
  alist(
    cpi_score ~ dnorm(mu, sigma),
    mu <- a + b * gdp_per_capita/1000,
    a ~ dnorm(10, 5),
    b ~ dnorm(-0.4, 0.15),
    sigma ~ dunif(0, 20) # dnorm(0, 10) # dexp(1) # dunif(0, 20)
  ), data = d
)

# summarize the posterior
summary(m1)

## Pull some samples from the posterior ------------------

posterior <- extract.samples(m1)

# plot the posterior
ggplot(posterior,
       aes(x=a, y=b)) +
  geom_point(alpha = 0.1) +
  theme_classic()


## Posterior Predictive Simulations ---------------

# create a vector of GDP values along the range of the x-axis
gdp.seq <- seq( from=min(d$gdp_per_capita) , to=max(d$gdp_per_capita) , length.out=30 )

# predict mu for each of those GDP values
mu <- link( m1 , data=data.frame(gdp_per_capita = gdp.seq) )

# take the average mu from the posterior samples
mu.mean <- apply( mu , 2 , mean )

# take the 89% posterior interval
mu.PI <- apply( mu , 2 , PI , prob=0.89 )

# the sim() function draws posterior samples, and uses them 
# to predict the *outcome*
sim.cpi <- sim( m1 , data=data.frame(gdp_per_capita = gdp.seq) )
cpi.PI <- apply( sim.cpi , 2 , PI , prob=0.89 )

# Plot the raw data
plot( cpi_score ~ gdp_per_capita , d , col=col.alpha(rangi2,0.5) )

# plot the MAP line
lines( gdp.seq , mu.mean )

# plotting the confidence interval of the mean
shade( mu.PI , gdp.seq )

# plots the 89% predictive interval
shade( cpi.PI , gdp.seq )
