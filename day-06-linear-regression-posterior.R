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


## Log-GDP Version of the model ----------------------------

# standardize GDP
d <- d %>% 
  mutate(log_gdp_per_capita = log(gdp_per_capita),
         log_gdp_demeaned = log_gdp_per_capita - mean(log_gdp_per_capita))

hist(d$log_gdp_demeaned)

# some prior predictive simulations
# draw random values from our proposed priors
alpha <- rnorm(1e6, mean=50, sd=10)
beta <- rnorm(1e6, mean=15, sd=5)

# plot a few of those prior predictions
plot( NULL , xlim=c(-3,2), ylim=c(0,100) ,
      xlab="Log GDP Per Capita (Demeaned)" , ylab="CPI Score" )
for ( i in 1:100 ) {
  curve( alpha[i] + beta[i]*x ,
         from=-3 , to=2 , add=TRUE ,
         col=col.alpha("black",0.2) )
}


# fit the model with quap()
m1 <- quap(
  alist(
    cpi_score ~ dnorm(mu, sigma),
    mu <- a + b * log_gdp_demeaned,
    a ~ dnorm(50, 10),
    b ~ dnorm(15, 5),
    sigma ~ dunif(0, 20) # dnorm(0, 10) # dexp(1) # dunif(0, 20)
  ), data = d
)

summary(m1)

# plot the posterior predictive intervals...

# create a vector of GDP values along the range of the x-axis
gdp.seq <- seq( from=min(d$log_gdp_demeaned) , to=max(d$log_gdp_demeaned) , length.out=30 )

# predict mu for each of those GDP values
mu <- link( m1 , data=data.frame(log_gdp_demeaned = gdp.seq) )

# take the average mu from the posterior samples
mu.mean <- apply( mu , 2 , mean )

# take the 89% posterior interval
mu.PI <- apply( mu , 2 , PI , prob=0.89 )

# the sim() function draws posterior samples, and uses them 
# to predict the *outcome*
sim.cpi <- sim( m1 , data=data.frame(log_gdp_demeaned = gdp.seq) )
cpi.PI <- apply( sim.cpi , 2 , PI , prob=0.89 )

# Plot the raw data
plot( cpi_score ~ log_gdp_demeaned , d , col=col.alpha(rangi2,0.5) )

# plot the MAP line
lines( gdp.seq , mu.mean )

# plotting the confidence interval of the mean
shade( mu.PI , gdp.seq )

# plots the 89% predictive interval
shade( cpi.PI , gdp.seq )
