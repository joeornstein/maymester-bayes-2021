## BONUS FUN: Posterior predictive simulations with ggplot

# I figured it out!

library(tidyverse)
library(rethinking)

## Load Data --------------------

d <- read_csv('data/cpi_gdp.csv') %>% 
  # remove countries with missing data
  na.omit() %>% 
  # standardize gdp per capita for ZJB model
  mutate(std_gdp = (gdp_per_capita - min(gdp_per_capita)) /
           (max(gdp_per_capita) - min(gdp_per_capita)),
         gdp_in_thousands = gdp_per_capita / 1000)

## fit the model with quap() ------------------------

m1 <- quap(
  alist(
    cpi_score ~ dnorm(mu, sigma),
    mu <- a + b * gdp_in_thousands,
    a ~ dnorm(10, 5),
    b ~ dnorm(-0.4, 0.15),
    sigma ~ dunif(0, 20)
  ), data = d
)


## sample from the posterior ----------------

posterior <- extract.samples(m1)

# plot alpha and beta
p1 <- ggplot(data = posterior,
             mapping = aes(x=a, y=b)) +
  geom_point(alpha = 0.1) +
  theme_classic()
p1

# plot the raw data
p2 <- ggplot(data = d,
             mapping = aes(x=gdp_in_thousands,
                           y=cpi_score)) +
  geom_point(alpha = 0.5) +
  theme_classic()

p2

# plot a bunch of lines from the posterior
for(i in 1:40){
  p2 <- p2 +
    geom_abline(slope = posterior$b[i],
                intercept = posterior$a[i],
                alpha = 0.1)
}
p2
