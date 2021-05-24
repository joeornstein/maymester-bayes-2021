## Practice problems from Chapter 3

library(rethinking)

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

# draw samples
samples <- sample(p_grid, size = 1e6, prob=posterior, replace=TRUE )

## Problems ---------------

# what percent of the posterior lies below 0.2?
sum( samples < 0.2 ) / length(samples)

# > 0.8
sum( samples > 0.8 ) / length(samples)

# Prob( p between 0.3 and 0.5 )
sum( samples > 0.3 & samples < 0.5 ) / length( samples )

# generate 99% confidence intervals
PI( samples, prob = 0.99 )
HPDI( samples, prob = 0.99 )


## HARD PROBLEMS -------------------

data(homeworkch3)

sum(birth1) + sum(birth2)

# grid approximate posterior
p_grid <- seq(0, 1, length.out=1000) # define p_grid

# define the prior
# p is defintely between 0.4 and 0.6
prior <- ifelse(p_grid > 0.4 & p_grid < 0.6, 1, 0)

plot(p_grid, prior, type = 'l')

# define the likelihood
likelihood <- dbinom(x=111, size=200, prob=p_grid)

# posterior
posterior <- prior * likelihood

# normalize
posterior <- posterior / sum(posterior)

# plot that posterior
plot(p_grid, posterior, type = 'l')

# draw samples from the posterior
samples <- sample( p_grid, size = 1e4, prob=posterior, replace=TRUE )

dens( samples )

# generate posterior intervals
HPDI(samples, prob = 0.5)
HPDI(samples, prob = 0.89)
HPDI(samples, prob = 0.97)

# posterior predictive distribution
predicted_boys <- rbinom(n=1e4, size=200, prob=samples)

hist(predicted_boys)

# 3H4. posterior predictive distribution but for 100 births
predicted_boys <- rbinom(n=1e4, size=100, prob=samples)

hist(predicted_boys)

# 3H5. posterior predictive distribution for 49 births
predicted_boys <- rbinom(n=1e4, size=49, prob=samples)

hist(predicted_boys)

# how "extreme" is 39 boys out of 49?
sum(predicted_boys >= 39) / length(predicted_boys)

# what percent of girls are born after boys?
sum(birth2[birth1 == 1])
21 / 51

# how "extreme" is 21 boys out of 51?
predicted_boys <- rbinom(n=1e4, size=51, prob=samples)

hist(predicted_boys)
sum(predicted_boys <= 21) / length(predicted_boys)
