## Suppose we were missing data from the corruption dataset
## Can Bayesian models help???

## Sure we can!!


library(tidyverse)
library(rethinking)

d <- read_csv('data/clean_data.csv') %>% 
  # normalize gdp per capita
  mutate(regime_type = if_else(democracy == 1, 'Democracy', 'Autocracy'),
         log_gdp = log2(gdp_per_capita))

mean_log_gdp <- d$log_gdp %>% na.omit %>% mean
sd_log_gdp <- d$log_gdp %>% na.omit %>% sd

d <- d %>% 
  mutate(log_gdp_demeaned = (log_gdp - mean_log_gdp) / sd_log_gdp,
         log_gdp_demeaned2 = log_gdp_demeaned ^ 2,
         log_gdp_demeaned3 = log_gdp_demeaned ^ 3) %>% 
  # create an index variable for regime
  mutate(regime = if_else(democracy == 1, 2, 1)) %>% 
  # filter out the missing democracy values
  filter(!is.na(regime),
         !is.na(cpi_score))

## Fit a model without the missing values -------------------------

m1 <- quap(
  alist(
    cpi_score ~ dnorm( mu, sigma ),
    mu <- a[regime] + b*log_gdp_demeaned,
    a[regime] ~ dnorm(50, 10),
    b ~ dnorm(10, 5),
    sigma ~ dunif(0, 25)
  ), data = d %>% na.omit
)

precis(m1, depth = 2)

## One cool trick: treat the missing values as parameters to be estimated ---------------

dat <- list(
  cpi_score = d$cpi_score,
  regime = d$regime,
  log_gdp_demeaned = d$log_gdp_demeaned
)

m2 <- ulam(
  alist(
    cpi_score ~ dnorm( mu, sigma ),
    mu <- a[regime] + b*log_gdp_demeaned,
    a[regime] ~ dnorm(50, 10),
    b ~ dnorm(10, 5),
    
    # HERE'S THE CHANGE
    log_gdp_demeaned ~ dnorm(nu, sigma_gdp),
    nu ~ dnorm(0, 0.5),
    sigma_gdp ~ dexp(1),
    
    sigma ~ dunif(0, 25)
  ), data = dat
)

precis(m2, depth = 2)

## Let's see how it did.....

posterior <- extract.samples(m2)

# append the imputed values
d2 <- d %>% 
  filter(is.na(log_gdp_demeaned)) %>% 
  mutate(imputed_log_gdp_mean = apply(posterior$log_gdp_demeaned_impute, 2, mean),
         imputed_log_gdp_PI_lower = apply(posterior$log_gdp_demeaned_impute, 2, PI, prob = 0.95)[1,],
         imputed_log_gdp_PI_upper = apply(posterior$log_gdp_demeaned_impute, 2, PI, prob = 0.95)[2,])


## plot the data + imputed values -------------------

p <- ggplot() +
  # non-missing data
  geom_point(data = d,
             mapping = aes(x=log_gdp_demeaned,
                           y = cpi_score),
             alpha = 0.5) +
  # missing data
  geom_point(data = d2,
             mapping = aes(x=imputed_log_gdp_mean,
                           y=cpi_score),
             alpha = 0.5,
             color = 'steelblue') +
  geom_errorbarh(data = d2,
                mapping = aes(xmin=imputed_log_gdp_PI_lower,
                              xmax=imputed_log_gdp_PI_upper,
                              y=cpi_score),
                color = 'steelblue') +
  facet_wrap(~regime_type) +
  theme_bw() +
  labs(x='Log GDP Demeaned',
       y='CPI Score')
  
p

## Compare to the reported GDP values from World Bank ------------------

gdp <- read_csv('data/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_2357064.csv', 
                skip = 4) %>% 
  clean_names() %>% 
  filter(country_code %in% d2$iso3)


# rescale log gdp
d2 %>% 
  mutate(imputed_log_gdp_per_capita = 
           2^(imputed_log_gdp_mean * sd_log_gdp + mean_log_gdp),
         PI_lower = 
           2^(imputed_log_gdp_PI_lower * sd_log_gdp + mean_log_gdp),
         PI_upper = 
           2^(imputed_log_gdp_PI_upper * sd_log_gdp + mean_log_gdp)) %>% 
  select(country, iso3, imputed_log_gdp_per_capita,
         PI_lower, PI_upper)

