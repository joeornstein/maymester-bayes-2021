## Gaussian model of height

library(tidyverse)
library(rethinking)

# load the height data
data("Howell1")
d <- Howell1

# subset just the adults
d2 <- d %>% 
  filter(age >= 18)

# prior of mu ~ Normal(178, 20)
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )

# prior of mu ~ Normal(167, 10)
curve( dnorm( x , 167 , 10 ) , from=100 , to=250 )


## Prior predictive simulation ------------------------

sample_mu <- rnorm( 1e4 , 167 , 10 ) # prior on mu (average height)
sample_sigma <- runif( 1e4 , 0 , 20 ) # prior on sigma (sd height)
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma ) # prior height
dens( prior_h )

# how many 8 foot adults would we have?
sum(prior_h > 250) / length(prior_h)

# according to the prior, what percent of adults would be less than 3 feet?
sum(prior_h < 100) / length(prior_h)

PI(prior_h, prob = 0.95)

## So our model is going to look like this:

# h_i ~ Normal(mu, sigma)
# mu ~ Normal(167, 10)
# sigma ~ Uniform(0, 20)


## Grid Approximation -------------------

# create a dataframe with all possible combinations of mu and sigma
mu.list <- seq( from=150, to=160 , length.out=100 )
sigma.list <- seq( from=7 , to=9 , length.out=100 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )

# compute the (log) likelihood
post$LL <- sapply( 1:nrow(post) , function(i) 
  sum(dnorm( d2$height , post$mu[i] , post$sigma[i] , log=TRUE ) ) )

# multiply the prior and the likelihood 
# (equivalent to adding log-likelihood and log-prior)
post$prod <- post$LL + 
  dnorm( post$mu , 167 , 2 , log = TRUE ) +
  dunif( post$sigma , 0 , 20 , log = TRUE )

# convert log-posterior to regular posterior
post$prob <- exp( post$prod - max(post$prod) )

# normalize, because not normalizing makes Joe uncomfortable
post$posterior <- post$prob / sum(post$prob)

# sample from that grid approximate posterior
sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                       prob=post$posterior )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]

# plot the posterior
tibble(sample.mu, sample.sigma) %>% 
  ggplot() +
  geom_point(aes(x=sample.mu, y=sample.sigma),
             alpha = 0.1, color = '#BA0C2F') +
  theme_classic() +
  labs(x = 'mu', y = 'sigma',
       title = 'Posterior Distribution')

# describe the data
mean(d2$height)
sd(d2$height)
