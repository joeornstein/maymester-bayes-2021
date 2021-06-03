## Section 10.2.3 simulation

library(tidyverse)

# when X and Z both cause Y, but there's no backdoor path,
# omitting Z can still bias your estimates, because Z pushes up 
# Y until it hits the ceiling (1)

N <- 1e3
# X and Z are random normal
X <- rnorm(N, 0, 2)
Z <- rnorm(N, 0, 2)

# Y1 is a continuous variable = X + Z + epsilon
Y1 <- X + Z + rnorm(N, 0 , 1)

# Y2 is a binary variable, with P(Y=1) determined by X + Z
Y2 <- if_else(runif(N,0,1) > inv_logit(X + Z), 0, 1)

# put it into a dataframe
d <- tibble(X, Z, Y1, Y2)

## first, the linear models ---------------------------

lm1 <- lm(Y1 ~ X, data = d)
lm2 <- lm(Y1 ~ X + Z, data = d)

summary(lm1)
summary(lm2)

# without a link function, omitting Z doesn't bias the coefficient on X,
# because there's no backdoor path!


## Now, logistic regressions (frequentist for now) -----------------

m1 <- glm(Y2 ~ X, data = d, family = 'binomial')
m2 <- glm(Y2 ~ X + Z, data = d, family = 'binomial')

summary(m1)
summary(m2)

# note that when we omit Z, the coefficient X is biased, even though
# there's no backdoor path!