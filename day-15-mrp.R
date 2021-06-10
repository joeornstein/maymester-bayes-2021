## Multilevel Regression and Poststratification (Mr. P)

library(tidyverse)
library(janitor) # a package for cleaning up datasets (get it?)
library(rethinking)

## Step 1: Estimate a multilevel model predicting vote choice --------------

# load and clean the CCES dataset
d <- read_csv('data/CCES-Train.csv') %>% 
  mutate(age = 2018 - birthyr,
         age_std = (age - mean(age)) / sd(age),
         race_index = case_when(race == 'White' ~ 1,
                                race == 'Black' ~ 2,
                                race == 'Hispanic' ~ 3,
                                race == 'Native American' ~ 4,
                                race == 'Asian' ~ 5,
                                TRUE ~ 6),
         state_index = as.integer(as.factor(inputstate)))

dat <- list(
  democratic2016 = d$democratic2016,
  age_std = d$age_std,
  race_index = d$race_index,
  state_index = d$state_index
)

starttime <- Sys.time() # start the clock

# fit multilevel model
mrp_model <- ulam(
  alist(
    democratic2016 ~ dbinom(1, p),
    # logit link with varying slope on age by race (probably steeper for white people)
    logit(p) <- b[race_index]*age_std + aR[race_index] + aS[state_index],
    b[race_index] ~ dnorm(0, 0.3),
    aR[race_index] ~ dnorm(0, 0.5),
    aS[state_index] ~ dnorm(state_bar, state_sigma), # adaptive prior
    state_bar ~ dnorm(0, 0.2),
    state_sigma ~ dexp(1)
  ), data = dat, chains = 1, iter = 5000
)

save(mrp_model, file = 'models/mrp-model.RData') # save fitted model

stoptime <- Sys.time() # stop the clock
stoptime - starttime

traceplot( mrp_model, pars = c('b[1]', 'aR[1]', 'aS[1]'))

precis(mrp_model, depth = 2)
plot(mrp_model, depth = 2)



## Step 2: Poststratify predictions using Census data -------------------

# create a *poststratification frame* from 
# Census data on population by race, age, and state
psframe <- read_csv('data/mrp/sc-est2019-alldata6.csv') %>% 
  clean_names() %>% 
  # keep the breakdowns by Hispanic origin but not by sex
  filter(origin != 0,
         sex == 0) %>% 
  # keep the variables we want
  select(fips = state,
         statename = name,
         origin, race, age,
         popestimate2016) %>% 
  # format the covariates like what we fed the model
  mutate(race_index = case_when(origin == 2 ~ 3,
                          race == 1 ~ 1,
                          race == 2 ~ 2,
                          race %in% c(3, 5) ~ 4,
                          race == 4 ~ 5,
                          TRUE ~ 6)) %>% 
  mutate(state_index = as.integer(as.factor(fips))) %>% 
  # keep only the 18+
  filter(age >= 18) %>% 
  # standardize age
  mutate(age_std = (age - mean(d$age)) / sd(d$age)) %>% 
  select(fips, statename, state_index, race_index, age, age_std, popestimate2016)

# now psframe should have the population count for every
# category of person by state, race, and age
head(psframe)

# append the model's posterior predictions to the psframe
predictions <- link(mrp_model, psframe)

psframe <- psframe %>% 
  mutate(p_democratic2016 = apply(predictions, 2, mean),
         PI_lower = apply(predictions, 2, PI, prob = 0.95)[1,],
         PI_upper = apply(predictions, 2, PI, prob = 0.95)[2,])

# now take the weighted average prediction for each state!
poststratified_estimates <- psframe %>% 
  group_by(fips, statename) %>% 
  summarize(voting_age_population = sum(popestimate2016),
            # predicted vote share for each state is weighted average of predictions by each group's population
            predicted_pct_democratic2016 = 
              sum(p_democratic2016 * popestimate2016) /
              voting_age_population,
            # posterior intervals
            PI_lower = sum(PI_lower * popestimate2016) / voting_age_population,
            PI_upper = sum(PI_upper * popestimate2016) / voting_age_population) %>% 
  ungroup


## Evaluate the poststratified estimates ------------------------

# notice that the width of our posterior intervals is wider for
# smaller states. reflects the additional uncertainty in those varying intercepts
fig1 <- poststratified_estimates %>% 
  mutate(statename = fct_reorder(statename, -predicted_pct_democratic2016)) %>% 
  ggplot(aes(x = predicted_pct_democratic2016,
           y = statename,
           xmin = PI_lower, xmax = PI_upper)) +
  geom_point() +
  geom_linerange() +
  theme_bw() +
  geom_vline(xintercept = 0.5, linetype = 'dashed') +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(x = 'Estimate',
       y = 'State',
       title = 'Multilevel Regression and Poststratification (MrP) Estimates and Posterior Intervals',
       caption = 'State-level poststratified estimates from the CCES of Clinton 2016 support')

fig1
ggsave('figures/mrp-figure-1.png', width = 8, height = 5,
       scale = 1.1)

# as expected, the estimates *way* overpredict Clinton support.
# but are they at least correlated with the actual results?
results <- read_csv('data/mrp/1976-2020-president.csv') %>% 
  # keep year 2016 and Clinton/Trump vote totals
  filter(year == 2016,
         candidate %in% c('TRUMP, DONALD J.', 'CLINTON, HILLARY')) %>% 
  select(state_fips, state, candidate, candidatevotes) %>% 
  # combine vote totals in states where they're listed under different parties
  group_by(state_fips, state, candidate) %>% 
  summarize(candidatevotes = sum(candidatevotes)) %>% 
  # compute Clinton's percent of two-party vote share in each state
  group_by(state_fips, state) %>% 
  summarize(pct = candidatevotes / sum(candidatevotes),
            candidate = candidate) %>% 
  filter(candidate == 'CLINTON, HILLARY') %>% 
  select(fips = state_fips, state, pct_clinton = pct) %>% 
  ungroup

# merge poststratified estimates with the actual results
poststratified_estimates <- poststratified_estimates %>% 
  mutate(fips = as.numeric(fips)) %>% 
  left_join(results %>% 
              select(-state), 
            by = 'fips')

# plot the correlation
fig2 <- poststratified_estimates %>% 
  ggplot(aes(x=predicted_pct_democratic2016,
             y=pct_clinton,
             xmax = PI_upper, xmin = PI_lower)) +
  geom_point(alpha = 0.6) +
  geom_linerange(alpha = 0.1) +
  theme_classic() +
  labs(x = 'MrP Estimate',
       y = 'Clinton Two-Party Vote Share (2016)',
       title = 'Multilevel Regression and Poststratification (MrP)',
       caption = 'Poststratified estimates and posterior intervals from CCES plotted against 2016 election results')

fig2
# Holy smokes! It's remarkably good for a model that only knows
# your age, race, and state of residence.
# I was honestly expecting it to be worse!
ggsave('figures/mrp-figure-2.png', width = 8, height = 5)


## Challenge Questions! ------------------------

# 1. What's with the terrible estimate for North Dakota? How could we improve the model?

# 2. What's with the terrible estimate for Vermont? How could we improve the model?
