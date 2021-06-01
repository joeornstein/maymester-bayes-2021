## Multicollinearity (with legs!)

library(rethinking)

## generate the leg data -----------------
N <- 100 # number of individuals
# set.seed(909)
height <- rnorm(N,10,2)# sim total height of each
leg_prop <- runif(N,0.4,0.5)# leg as proportion of height
leg_left <- leg_prop*height +# sim left leg as proportion + error
  rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height +    # sim right leg as proportion + error
  rnorm( N , 0 , 0.02 )
# combine into data frame
d <- data.frame(height,leg_left,leg_right)

## estimate a model with one leg as predictor ---------------------

m1 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*leg_left,
    a ~ dnorm(0, 10),
    b ~ dnorm(2, 5),
    sigma ~ dexp(3)
  ), data = d
)

summary(m1)

## estimate a model with *both* legs as predictors --------------

m2 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bL * leg_left + bR * leg_right,
    a ~ dnorm(0, 10),
    bL ~ dnorm(2, 5),
    bR ~ dnorm(2, 5),
    sigma ~ dexp(3)
  ), data = d
)

summary(m2)



