## Practice with regularizing priors

library(tidyverse)
library(rethinking)

# here's the data
sppnames <- c( "afarensis","africanus","habilis","boisei",
               "rudolfensis","ergaster","sapiens")
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg )

## Prep the data / priors --------------------------------

# standardize x and y variables
d <- d %>% 
  mutate(B = brainvolcc / max(brainvolcc),
         M = scale(masskg))

# priors??
# b ~ Normal(0.2, 0.1)
# a ~ Uniform(0, 1)

## fit the model ------------------------

m1 <- quap(
  alist(
    B ~ dnorm( mu , sigma ),
    mu <- a + b * M,
    a ~ dunif(0, 1),
    b ~ dnorm(0.2, 0.1),   # play with these priors! center them tightly around zero and see what happens?
    sigma ~ dunif(0, 0.5)
  ), data = d
)

summary(m1)

# extract samples from the posterior
mu <- link(m1, d)

p <- d %>% 
  mutate(prediction = apply(mu, 2, mean),
         PI_upper = apply(mu, 2, PI, prob = 0.95)[2,],
         PI_lower = apply(mu, 2, PI, prob = 0.95)[1,]) %>% 
  ggplot() +
  # plot the points
  geom_point(mapping = aes(x=M, y=B)) +
  # plot the MAP line
  geom_line(mapping = aes(x=M, y=prediction)) +
  # plot the 95% posterior interval
  geom_ribbon(mapping = aes(x = M, ymax = PI_upper, ymin = PI_lower),
              alpha = 0.4) +
  # make it pretty
  theme_bw() +
  labs(x = 'Standardized Body Mass',
       y = 'Standardized Brain Size')
p

## NOTE: in the code below I tried to be irresponsible
## and build an overfit model, but quap() didn't let me.
## One nice thing about Bayesian statistics is that sometimes
## it forces you to have good habits!

# ## Create an overfit model ------------------------------
# 
# # generate polynomials
# d <- d %>% 
#   mutate(B2 = B^2,
#          B3 = B^3,
#          B4 = B^4,
#          B5 = B^5)
# 
# # fit the model
# m2 <- quap(
#   alist(
#     M ~ dnorm( mu, sigma ),
#     mu <- a + b1*B + b2*B2 + b3*B3 + b4*B4 + b5*B5,
#     a ~ dunif(-1, 1),
#     b1 ~ dnorm(0.2, 0.1),
#     b2 ~ dnorm(0, 0.5),
#     b3 ~ dnorm(0, 0.5),
#     b4 ~ dnorm(0, 0.5),
#     b5 ~ dnorm(0, 0.5),
#     sigma ~ dunif(0, 4)
#   ), data = d
# )
# 
# summary(m2)
# 
# 
# # plot posterior predictions
# mu <- link(m2, d)
# 
# p <- d %>% 
#   mutate(prediction = apply(mu, 2, mean),
#          PI_upper = apply(mu, 2, PI, prob = 0.95)[2,],
#          PI_lower = apply(mu, 2, PI, prob = 0.95)[1,]) %>% 
#   ggplot() +
#   # plot the points
#   geom_point(mapping = aes(x=M, y=B)) +
#   # plot the MAP line
#   geom_line(mapping = aes(x=M, y=prediction)) +
#   # plot the 95% posterior interval
#   geom_ribbon(mapping = aes(x = M, ymax = PI_upper, ymin = PI_lower),
#               alpha = 0.4) +
#   # make it pretty
#   theme_bw() +
#   labs(x = 'Standardized Body Mass',
#        y = 'Standardized Brain Size')
# p
