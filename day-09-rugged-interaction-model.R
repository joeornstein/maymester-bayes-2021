## Build the linear interaction model

library(tidyverse)
library(rethinking)

# load the data
data(rugged)
d <- rugged

# clean it up
dd <- d %>% 
  filter(!is.na(rgdppc_2000)) %>% 
  mutate(log_gdp = log(rgdppc_2000),
         log_gdp_std = log_gdp / mean(log_gdp),
         rugged_std = rugged / max(rugged),
         rugged_std = rugged_std - mean(rugged_std))

## fit some models ------------------

# bivariate relationship
m1 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a + b*rugged_std ,
    a ~ dnorm( 1 , 0.1 ) ,
    b ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ), data = dd
)

summary(m1)

# add our index variable for continent
dd <- dd %>% 
  mutate(cid = if_else(cont_africa == 1, 1, 2))

m2 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b*rugged_std ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ), data = dd
)

precis(m2, depth = 2)

# add varying slopes as well
m3 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*rugged_std ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ), data = dd
)

precis(m3, depth = 2)

