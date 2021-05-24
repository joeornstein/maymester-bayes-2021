## Pulling marbles from bags

# create a bag of marbles. 1=blue, 0=white
bag <- c(1,1,1,0)

# draw 3 marbles from the bag
sample( x=bag, size=3, replace=TRUE )

# draws 3 marbles 10 times
replicate(10, sample( x=bag, size=3, replace=TRUE ))

# count the number of blues
sum( sample( x=bag, size=3, replace=TRUE ) )

# draw from the bag 5000 times, counting the number of blues
blues <- replicate(5000, sum( sample( x=bag, size=3, replace=TRUE ) ))
# (sampling distribution)

hist(blues)

# draw from the binomial distribution
blues_binomial <- rbinom(n=5000, size=3, prob=0.75)

hist(blues_binomial)

par(mfrow=c(1,2))
hist(blues)
hist(blues_binomial)

# What's the probability of drawing 2 blues from a bag with 3/4 blues?
dbinom(x=2, size=3, prob=0.75)
dbinom(x=3, size=3, prob=0.75)
dbinom(x=1, size=3, prob=0.75)

## Create the likelihood function --------------------

# these are our *conjectures*, all the possible values of p
p <- c(0, 0.25, 0.5, 0.75, 1)

# the *likelihood* tells us the probability of drawing 2 blues
# conditional on each of our conjectures.
likelihood <- dbinom(x=2, size=3, prob=p)

# plot the likelihood function
par(mfrow=c(1,1))
plot(p, likelihood)

## suppose we have a prior -------------------

# we know that the factory only produced bags with 
# 0 blue, 4 blue, or 2 blue
prior <- c(1/3, 0, 1/3, 0, 1/3)

posterior <- prior * likelihood

plot(p, posterior)

# another prior! we only think there are bags with 1 or 3 marbles
prior <- c(0, 1/2, 0, 1/2, 0)

posterior <- prior * likelihood

plot(p, posterior)

# what if we had no idea about the prior distribution?
prior <- c(1/5, 1/5, 1/5, 1/5, 1/5)

posterior <- prior * likelihood

plot(p, posterior)

prior <- c(0, 0, 0, 0, 1)
posterior <- prior * likelihood

plot(p, posterior)
