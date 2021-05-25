## Quadratic Approximation

library(tidyverse)
library(rethinking)

data('Howell1')
d <- Howell1

# subset the adults
d2 <- d %>% 
  filter(age >= 18)

# use quadratic approximation to generate the posterior
m4.1 <- quap(
  alist(
    height ~ dnorm( mu, sigma ),
    mu ~ dnorm( mean=167, sd = 0.5 ),
    sigma ~ dunif( 0 , 20 )
  ),
  data = d2
)

summary(m4.1)

# draw samples
samples <- extract.samples(m4.1)

# plot the samples
ggplot(samples,
       mapping = aes(x=mu, y=sigma)) +
  geom_point(alpha = 0.1, color = '#BA0C2F') +
  theme_classic() +
  labs(x = 'mu', y = 'sigma',
       title = 'Posterior Distribution')


## Now we'll add a predictor variable to our model ---------------

# plot height against weight
p1 <- ggplot(data = d2,
             mapping = aes(x=weight, y=height)) +
  geom_point(alpha = 0.5) +
  theme_classic() + 
  labs(x = 'Weight', y = 'Height',
       title = 'Weights and Heights (Human Adults)')

p1 # well dang it looks linear
N <- 1000
a <- rnorm(N, 167, 10)
b <- rlnorm(N, 0, 0.7)

# plot the prior predictive distribution

# set up the chart
plot( NULL , xlim=range(d2$weight) , ylim=c(-100,400) ,
      xlab="weight" , ylab="height" )

# add a zero height line
abline( h=0 , lty=2 )

# add the tallest person ever recorded
abline( h=272 , lty=1 , lwd=0.5 )

# title
mtext( "b ~ dlnorm(0,0.7)" )

# plot the simulated prior lines
xbar <- mean(d2$weight)
for ( i in 1:N ) 
  curve( a[i] + b[i]*(x - xbar) ,from=min(d2$weight) , 
         to=max(d2$weight) , add=TRUE ,col=col.alpha("black",0.2) )


## Approximate the posterior with quap()

xbar <- mean(d2$weight)

m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(weight - xbar),
    a ~ dnorm(mean = 167, sd = 10),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 20)
  ), 
  data = d2
)

summary(m4.3)

posterior <- extract.samples(m4.3)

## Posterior predictive simulation ---------------------

weight <- 46.95


# compute the predicted weight from your samples
posterior$predicted_weight <- 
  posterior$a + posterior$b * (weight - xbar)

hist(posterior$predicted_weight)


# compute predicted weight from link() function
predicted_weights <- link(m4.3, 
                          data = data.frame(weight = 46.95))

hist(predicted_weights)

# get the posterior interval
PI(predicted_weights, prob = 0.89)


