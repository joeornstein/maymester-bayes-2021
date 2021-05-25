## Prior Predictive Simulations

library(tidyverse)
library(rethinking)

# trying to figure out sensible priors for our CPI prediction model


# here's the model
# CPI_i ~ Normal(mu_i, sigma) [likelihood]
# mu_i = alpha + beta * GDP_i [linear model]
# alpha ~ PRIOR TO BE DETERMINED
# beta ~ PRIOR TO BE DETERMINED
# sigma ~ PRIOR TO BE DETERMINED

# load the data to get a sense of the range
d <- read_csv('data/cpi_gdp.csv') %>% 
  na.omit() # remove countries with missing data

minimum_gdp <- min(d$gdp_per_capita)
maximum_gdp <- max(d$gdp_per_capita)

maximum_cpi <- max(d$cpi_score)
mimimum_cpi <- min(d$cpi_score)

## Prior predictive simulations --------------------

# draw random values from our proposed priors
alpha <- rnorm(1e6, mean=15, sd=3)
beta <- rnorm(1e6, mean=0, sd=1)

# create a dataframe for dummy data
# gdp_seq <- seq(minimum_gdp, maximum_gdp, length.out = 100)

plot( NULL , xlim=range(d$gdp_per_capita) , ylim=c(0,100) ,
      xlab="GDP Per Capita PPP" , ylab="CPI Score" )
abline( h=mimimum_cpi , lty=2 )
abline( h=maximum_cpi , lty=1 , lwd=0.5 )
for ( i in 1:100 ) {
  curve( alpha[i] + beta[i]*x ,
         from=0 , to=maximum_gdp , add=TRUE ,
         col=col.alpha("black",0.2) )
}



