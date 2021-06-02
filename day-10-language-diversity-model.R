## Is language diversity correlated with the characteristics of a country's growing season?

library(tidyverse)
library(rethinking)

## Load and clean up the data ---------------------------

data(nettle)
d <- nettle %>% 
  mutate(Y = log2(num.lang / k.pop),
         # center predictors
         M = mean.growing.season - mean(mean.growing.season),
         S = sd.growing.season - mean(sd.growing.season),
         A = log2(area) - mean(log2(area)))

## Get some sensible priors ------------------------

# Y_i ~ Normal(mu, sigma)
# mu = a + bM * M + bA * A
# a is the average log-languages-per-capita for a country with average land area and growing season
# a ~ Normal(-8, 2)
# bM is the average additional log-languages-per-capita in countries with 1 more month of growing
# bM ~ Normal(0, 0.5)
# bA is the average additional log-languages-per-capita in countries with twice the land area
# bA ~ Normal(0, 1)
# sigma is the spread of the outcome
# bA ~ Exponential(0.25) # has a mean of 4, but it's bounded at zero
# (expected value of an exponential is 1/rate)

# plot those priors
a <- rnorm( 1e4, -8, 2 )
bM <- rnorm( 1e4, 0, 0.5 )
bA <- rnorm( 1e4, 0, 1)

p <- ggplot(data = d,
       aes(x=M,y=Y)) +
  theme_bw() +
  # points are transparent
  geom_point(alpha = 0)
for(i in 1:50){
  p <- p + geom_abline(intercept = a[i],
                       slope = bM[i],
                       alpha = 0.1)
}
p

## Fit the linear model with quap() ---------------------------------

m1 <- quap(
  alist(
    Y ~ dnorm(mu, sigma),
    mu <- a + bM*M + bA*A,
    a ~ dnorm(-8, 2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 1),
    sigma ~ dexp(0.25)
  ), data = d
)

summary(m1)

# what's the probability that bM is positive?
samples <- extract.samples(m1)
sum(samples$bM > 0) / length(samples$bM)

## Fit the linear-interaction model with quap() -------------------

m2 <- quap(
  alist(
    Y ~ dnorm(mu, sigma),
    mu <- a + bM*M + bS*S + bMS*M*S + bA*A,
    a ~ dnorm(-8, 2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 1),
    bS ~ dnorm(0, 1),
    bMS ~ dnorm(0,1),
    sigma ~ dexp(0.25)
  ), data = d
)

summary(m2)

# what's the probability that bMS is positve?
samples <- extract.samples(m2)
sum(samples$bMS > 0) / length(samples$bMS)

## Plot the posterior predictions (triptych ggplot) --------------------------------------

d <- d %>% 
  # create variables that split M and S into three categories by quantile
  mutate(M_cat = case_when(M < quantile(M, 1/3) ~ 'Short Growing Seasons',
                           M < quantile(M, 2/3) ~ 'Medium Growing Seasons',
                           TRUE ~ 'Long Growing Seasons'),
         S_cat = case_when(S < quantile(S, 1/3) ~ 'Low Growing Season Variance',
                           S < quantile(S, 2/3) ~ 'Medium Growing Season Variance',
                           TRUE ~ 'High Growing Season Variance')) %>% 
  # reorder those factors to make appealing graphs
  mutate(M_cat = factor(M_cat, 
                        levels = c('Short Growing Seasons',
                                   'Medium Growing Seasons',
                                   'Long Growing Seasons')),
         S_cat = factor(S_cat,
                        levels = c('Low Growing Season Variance',
                                   'Medium Growing Season Variance',
                                   'High Growing Season Variance')))

# plot the raw data (faceted by growing season variance)
p1 <- ggplot(data = d,
       mapping = aes( x=M , y=Y )) +
  geom_point(alpha = 0.6) +
  facet_grid(~S_cat) +
  theme_bw() +
  labs(title = 'Modeling Language Diversity',
       x = 'Mean Growing Season (centered)',
       y = 'Log Languages Per Capita',
       caption = 'Lines are drawn from the posterior, holding other covariates at their means within each bin')


# plot the raw data (faceted by mean growing season)
p2 <- ggplot(data = d,
             mapping = aes( x=S , y=Y )) +
  geom_point(alpha = 0.6) +
  facet_grid(~M_cat) +
  theme_bw() +
  labs(title = 'Modeling Language Diversity',
       x = 'Standard Deviation of Growing Season (centered)',
       y = 'Log Languages Per Capita',
       caption = 'Lines are drawn from the posterior, holding other covariates at their means within each bin')

# add lines from the posterior
# man this is tricky, but here's what I would do:

# get the average values of the predictor variables within each S_cat category 
d2 <- d %>% 
  group_by(S_cat) %>% 
  summarize(S = mean(S),
            A = mean(A))

# get 10,000 samples from the posterior
posterior <- extract.samples(m2)

# for the first 200 samples, compute the slope and intercept, and add to the ggplot
for(i in 1:200){
  
  d2 <- d2 %>% 
    mutate(slope = posterior$bM[i] + posterior$bMS[i] * S, # dY/dM = bM + bMS*S
           intercept = posterior$a[i] + posterior$bA[i]*A + posterior$bS[i]*S)  # when M=0, mu = a + bA*A + bs*S
  
  # add to ggplot
  p1 <- p1 + 
    geom_abline(data = d2,
                mapping = aes(slope = slope,intercept =intercept),
                alpha = 0.05)
}
p1

# and you could do the same thing for p2
d2 <- d %>% 
  group_by(M_cat) %>% 
  summarize(M = mean(M),
            A = mean(A))

for(i in 1:200){
  
  d2 <- d2 %>% 
    mutate(slope = posterior$bS[i] + posterior$bMS[i] * M, # dY/dS = bS + bMS*M
           intercept = posterior$a[i] + posterior$bA[i]*A + posterior$bM[i]*M)  # when S=0, mu = a + bA*A + bM*M
  
  # add to ggplot
  p2 <- p2 + 
    geom_abline(data = d2,
                mapping = aes(slope = slope,intercept =intercept),
                alpha = 0.05)
}
p2
