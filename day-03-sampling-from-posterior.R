## Sampling from the globe tossing posterior

p_grid <- seq(from=0, to=1, length.out=1000)

# prior
prob_p <- rep(1, 1000)

# likelihood
prob_data <- dbinom(x=6, size=9, prob=p_grid)

# posterior numerator
posterior <- (prob_p * prob_data)

# posterior (summed to 1)
posterior <- posterior / sum(posterior)

# plot it
plot(p_grid, posterior, type='l')

## Sample from the posterior ------------------------

samples <- sample(p_grid, size = 1e6, prob=posterior, replace=TRUE )

mean(samples)

plot( samples )

library(rethinking)

# plotting the density of your sampled posterior
dens( samples )

# P(p<0.5) from the grid approximate posterior
sum( posterior[p_grid < 0.5] )

# P(p<0.5) from the sampled posterior
sum( samples < 0.5 ) / length(samples)

# compute a posterior interval
PI( samples, prob = 0.95 )

# the MAP (maximum a posteriori estimate)
chainmode( samples )
mean( samples )


## Posterior Predictive Checks -------------------

# drawing random values for a fixed value of p
w <- rbinom( n=1e4, size=9, prob=0.6 )

simplehist( w )

# posterior predictive distribution
w <- rbinom( n=1e6, size=9, prob=samples )

simplehist( w )
