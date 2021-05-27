## BONUS FUN: What if your outcome is heteroskedastic? 

library(tidyverse)
library(rethinking)

# Up to now, we've built regression models assuming that sigma
# is constant for each observation. But...what if heteroskedasticity?

## Simulate some heteroskedastic data --------------------

set.seed(7) # set a random seed for reproducibility

N <- 300 # we'll draw N observations

d <- tibble(
  x = rnorm(N, 20, 5), # x is normally distributed
  y = rnorm(N, 0, 2*x) # mean(y) is unrelated to x, but sigma depends on x
)

ggplot(d,aes(x=x,y=y)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()


## quap() ------------------------

# fit the model with quap()
m1 <- quap(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + b*x,
    a ~ dnorm(0, 20),
    b ~ dnorm(0, 20),
    sigma ~ dunif(0, 100)
  ), data = d
)

# summarize the posterior
summary(m1)

## Try a model with non-constant sigma ---------------------

m2 <- quap(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + b*x,
    a ~ dnorm(0, 20),
    b ~ dnorm(0, 20),
    sigma <- b2*x,
    b2 ~ dexp(5)
  ), data = d
)

# summarize the posterior
summary(m2)

# this is sooooo much more satisfying than frequentist sandwich
# estimators, which adjust the standard errors but leave the 
# coefficients alone.
# Our Bayesian golem sees what's going on with sigma, and
# adjusts its posterior estimate of beta to compensate!

# for comparison, here's the OLS estimates
lm(y~x,d) %>% summary

# and OLS with Huber-White robust standard errors
estimatr::lm_robust(y~x, d) %>% summary


## Examine the posteriors ---------------------------

# posterior for equal variance model
posterior1 <- extract.samples(m1)

# posterior for unequal variance model
posterior2 <- extract.samples(m2)




## plot some posterior lines
p2 <- ggplot(data = d,
            mapping = aes(x=x,y=y)) + 
  geom_point(alpha = 0.5) +
  theme_bw()

p2

for(i in 1:40){
  p2 <- p2 +
    # equal variance model
    geom_abline(slope = posterior1$b[i],
                intercept = posterior1$a[i],
                alpha = 0.1, color = 'blue') +
    # unequal variance model
    geom_abline(slope = posterior2$b[i],
                intercept = posterior2$a[i],
                alpha = 0.1, color = 'red')
}
p2


# plot samples from posterior distribution over alpha and beta
p1 <- ggplot(posterior1,
             aes(x=a, y=b)) +
  geom_point(alpha = 0.1) +
  theme_bw()
ggExtra::ggMarginal(p1, type = 'histogram')