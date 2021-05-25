## Practice problems from Chapter 4

library(rethinking)

## 4M1: Prior simulation ---------------------

# here's the model:
# y_i ~ Normal(mu, sigma)
# mu ~ Normal(0, 10)
# sigma ~ exponential(1)

mu <- rnorm(1e6, mean = 0, sd = 10)
sigma <- rexp(1e6, rate = 1)

# y is distributed normally
y <- rnorm(1e6, mean=mu, sd=sigma)
hist(y) # there it is!

## 3M2 -----------------------

practice_model1 <- quap(
  alist(
    y ~ dnorm( mu , sigma ), # likelihood
    mu ~ dnorm( 0 , 10 ), # prior on mu
    sigma ~ dexp(1)
  ) # needs data!!!
)


## 4H1 ------------------------



