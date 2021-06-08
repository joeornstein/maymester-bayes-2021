## Let's evaluate some of the logistic model submissions

# 1. Do the priors make sense?
# 2. Do the MCMCs converge?
# 3. Do the posteriors make sense?
# 4. Do they produce good predictions?

# Names are anonymized to protect the innocent...

library(tidyverse)
library(rethinking)





## Hubert, Mack, and Chad ---------------------------------

# load and clean the data
d <- read_csv('data/CCES-Train.csv') %>% 
  mutate(Y = democratic2016, 
         famincome_quart = case_when(faminc_new %in% c(1:4) ~ 1,
                                     faminc_new %in% c(5:8) ~ 2,
                                     faminc_new %in% c(9:12) ~ 3,
                                     faminc_new %in% c(13:16) ~ 4),
         gender_num = case_when(gender == "Male" ~ 1,
                                gender == "Female" ~ 2),
         educ_num_v2 = case_when(educ == 'No HS' ~ 1,
                                 educ == 'High school graduate' ~ 2,
                                 educ == 'Some college' ~ 3,
                                 educ == '2-year' ~ 4,
                                 educ == '4-year' ~ 5,
                                 educ == 'Post-grad' ~ 6))

dat <- list(
  Y = d$Y,
  # index variables for family income, gender, and education
  famincome_quart = d$famincome_quart,
  gender_num = d$gender_num,
  educ_num_v2 = d$educ_num_v2
)

# fit the Hubert, Mack, and Chad model
model_HMC <- ulam(
  alist(
    Y ~ dbinom(1, p), # binomial likelihood
    logit(p) <- aF[famincome_quart] + aG[gender_num] + aE[educ_num_v2],
    aF[famincome_quart] ~ dnorm(2, 1),
    aG[gender_num] ~ dnorm(1.5, 1),
    aE[educ_num_v2] ~ dnorm(10, 2.5)
  ), data = dat, chains = 1, iter = 3000, log_lik = TRUE
)
# took...about 9 minutes to estimate

# save the model
save(model_HMC, file = 'models/model_HMC.RData')

# plot out the posterior intervals for each parameter
plot(model_HMC, depth = 2)
# really? no difference between males and females?
# no difference between respondents with different educational levels?

# MCMC diagnostics
traceplot( model_HMC )
# clearly some autocorrelation in the draws; and strangely small
# effective sample sizes for such a long chain!

# The problem is those priors. Let's do a prior predictive simulation...
# the guts of their model is...
# logit(p_democrat) = aF + aG + aE
p_democrat <- inv_logit(rnorm(1e5, 0, 0.2) +
                          rnorm(1e5, 0, 0.2) +
                          rnorm(1e5, 0, 0.2))
dens(p_democrat)

# plot prior on difference between men and women
prior_aF <- rnorm(1e5, 0, 0.2)
prior_aE <- rnorm(1e5, 0, 0.2)
prior_aG1 <- rnorm(1e5, 0, 0.2)
prior_aG2 <- rnorm(1e5, 0, 0.2)

prior_democrat_men <- inv_logit(prior_aF + prior_aE + prior_aG1)
prior_democrat_women <- inv_logit(prior_aF + prior_aE + prior_aG2)

# plot the difference between those two priors
dens(prior_democrat_men - prior_democrat_women)

# plot the logistic function, to get a sense for how log odds map onto probabilities
plot(seq(-10,10,0.1), inv_logit(seq(-10,10,0.1)), type = 'l',
     xlab = 'Log-Odds', ylab = 'Probability')

## Challenge: redefine some sensible priors and refit the model ---------------

model_HMC2 <- ulam(
  alist(
    Y ~ dbinom(1, p), # binomial likelihood
    logit(p) <- aF[famincome_quart] + aG[gender_num] + aE[educ_num_v2],
    aF[famincome_quart] ~ dnorm(0, 0.2),
    aG[gender_num] ~ dnorm(0, 0.2),
    aE[educ_num_v2] ~ dnorm(0, 0.2)
  ), data = dat, chains = 1, iter = 5000, log_lik = TRUE
)

# save the model
save(model_HMC2, file = 'models/model_HMC2.RData')

# plot the posteriors
plot(model_HMC2, depth = 2)

precis(model_HMC2, depth = 2)

traceplot( model_HMC2 )

# try dropping a predictor and see how MCMC does
# (worried about collinearity between income and education)
model_HMC3 <- ulam(
  alist(
    Y ~ dbinom(1, p), # binomial likelihood
    logit(p) <- aF[famincome_quart] + aG[gender_num],
    aF[famincome_quart] ~ dnorm(0, 0.2),
    aG[gender_num] ~ dnorm(0, 0.2)
  ), data = dat, chains = 1, ter = 5000, log_lik = TRUE
)

save(model_HMC3, file = 'models/model_HMC3.RData')

precis(model_HMC3, depth = 2)

## Here's a model from Sally & Mason -----------------------------

d <- read_csv('data/CCES-Train.csv') %>% 
  # reformat variables for model
  mutate(Y = democratic2016,
         R = case_when(region == 'Midwest' ~ 1,
                       region == 'Northeast' ~ 2,
                       region == 'South' ~ 3,
                       region == 'West' ~ 4),
         A = 2016 - birthyr,
         Fe = case_when(gender == 'Female' ~ 1,
                        gender == 'Male' ~ 0),
         C = case_when(child18 == 'Yes' ~ 1,
                       child18 == 'No' ~ 0),
         Em = case_when(employ == 'Full-Time' ~ 1,
                        employ == 'Unemployed' ~ 0,
                        employ == 'Retired' ~ 0,
                        employ == 'Part-Time' ~ 1,
                        employ == 'Permanently disabled' ~ 0,
                        employ == 'Other' ~ 0,
                        employ == 'Homemaker' ~ 0,
                        employ == 'Temporarily laid off' ~ 0,
                        TRUE ~ 0),
         Race = case_when (race == 'Black' ~ 1,
                           race == 'White' ~ 2,
                           race == 'Hispanic' ~ 3,
                           race == 'Asain' ~ 4,
                           race == 'Mixed' ~ 5,
                           race == 'Other' ~ 6,
                           TRUE ~ 6),
         Educ = case_when (educ ==  'High school graduate' ~ 0,
                           educ == 'Some college' ~ 1,
                           educ == '2-year' ~ 2,
                           educ == '4-year' ~ 3,
                           educ == 'Post-grad' ~ 4,
                           educ == 'No HS' ~ 0),
         LGBTQ = case_when(sexuality == 'Heterosexual' ~ 0,
                           sexuality == 'Gay' ~ 1,
                           sexuality == 'Lesbian' ~ 1,
                           sexuality == 'Bisexual' ~ 1,
                           sexuality == 'Prefer not to say' ~ 0,
                           sexuality == 'Other' ~ 1))

# despite the warnings of our sage Richard McElreath,
# they each attempted a version of the model with dummy variables
# let's see what this implies for our priors.....

dat <- list(
  Y = d$Y, # outcome variable (Democratic vote in 2016)
  R = d$R, # index variable for region
  A = d$A,
  Fe = d$Fe,
  C = d$C,
  Em = d$Em,
  Race = d$Race,
  Educ = d$Educ,
  LGBTQ = d$LGBTQ)

# here's their model
model_SM <- ulam(
  alist(
    Y ~ dbinom(1, p), # binomial likeliood
    logit(p) <- a[R] + b1[R]*Fe + b2[R]*A + b3[R]*Educ + b4[R]*C + b5[R]*Em + b6[R]*Race + b7[R]*LGBTQ, # log-odds vary by region
    a[R] ~ dnorm(0, 1.5),
    b1[R] ~ dnorm(0, .5),
    b2[R] ~ dnorm(0, .5),
    b3[R] ~ dnorm(0, .5),
    b4[R] ~ dnorm(0, .5),
    b5[R] ~ dnorm(0, .5),
    b6[R] ~ dnorm(0, .5),
    b7[R] ~ dnorm(0, .5)
  ), data = dat, chains = 4, log_lik=TRUE
)

save(model_SM, 'models/model_SM.RData')

# plot the priors for various groups
p_democratic_male_unemployed_nochildren_straight <- inv_logit(rnorm(1e5, 0, 1.5))
dens(p_democratic_male_unemployed_nochildren_straight)

p_democratic_female_employed_children_LGBTQ <- inv_logit(rnorm(1e5, 0, 1.5) +
                                                           rnorm(1e5, 0, 0.5) +
                                                           rnorm(1e5, 0, 0.5) +
                                                           rnorm(1e5, 0, 0.5) +
                                                           rnorm(1e5, 0, 0.5))
dens(p_democratic_female_employed_children_LGBTQ)


p_democratic_female_employed_children_LGBTQ_40yo <- inv_logit(rnorm(1e5, 0, 1.5) +
                                                           rnorm(1e5, 0, 0.5) +
                                                           rnorm(1e5, 0, 0.5) +
                                                           rnorm(1e5, 0, 0.5) +
                                                           rnorm(1e5, 0, 0.5) +
                                                             rnorm(1e5, 0, 0.5) * 40)
dens(p_democratic_female_employed_children_LGBTQ_40yo)

## Updated model ---------------------------------------------

d <- read_csv('data/CCES-Train.csv') %>% 
  # reformat variables for model
  mutate(Y = democratic2016,
         R = case_when(region == 'Midwest' ~ 1,
                       region == 'Northeast' ~ 2,
                       region == 'South' ~ 3,
                       region == 'West' ~ 4),
         A = scale(2018 - birthyr),
         gender = case_when(gender == 'Female' ~ 1,
                        gender == 'Male' ~ 2),
         children = case_when(child18 == 'Yes' ~ 2,
                       child18 == 'No' ~ 1),
         employment = case_when(employ == 'Full-time' ~ 1,
                        employ == 'Unemployed' ~ 2,
                        employ == 'Retired' ~ 3,
                        employ == 'Part-time' ~ 1,
                        employ == 'Permanently disabled' ~ 2,
                        employ == 'Other' ~ 2,
                        employ == 'Homemaker' ~ 4,
                        employ == 'Temporarily laid off' ~ 2,
                        TRUE ~ 2),
         Race = case_when (race == 'Black' ~ 1,
                           race == 'White' ~ 2,
                           race == 'Hispanic' ~ 3,
                           race == 'Asian' ~ 4,
                           race == 'Mixed' ~ 5,
                           race == 'Other' ~ 6,
                           TRUE ~ 6),
         Educ = case_when (educ ==  'High school graduate' ~ 2,
                           educ == 'Some college' ~ 3,
                           educ == '2-year' ~ 4,
                           educ == '4-year' ~ 5,
                           educ == 'Post-grad' ~ 6,
                           educ == 'No HS' ~ 1),
         LGBTQ = case_when(sexuality == 'Heterosexual' ~ 1,
                           sexuality == 'Gay' ~ 2,
                           sexuality == 'Lesbian' ~ 2,
                           sexuality == 'Bisexual' ~ 2,
                           sexuality == 'Prefer not to say' ~ 2,
                           sexuality == 'Other' ~ 2))

# make it a nice list for ulam()
dat <- list(
  Y = d$Y,
  R = d$R,
  gender = d$gender,
  A = as.numeric(d$A),
  Educ = d$Educ,
  children = d$children,
  employment = d$employment,
  Race = d$Race,
  LGBTQ = d$LGBTQ
)

# prior predictive simulation
(rnorm(1e5, 0, 0.2) + 
    rnorm(1e5, 0, 0.2) + 
    rnorm(1e5, 0, 0.2) + 
    rnorm(1e5, 0, 0.2) + 
    rnorm(1e5, 0, 0.2) + 
    rnorm(1e5, 0, 0.2) + 
    rnorm(1e5, 0, 0.2) + 
    rnorm(1e5, 0, 0.2)) %>% 
  inv_logit %>% 
  dens

model_SM2 <- ulam(
  alist(
    Y ~ dbinom(1, p), # binomial likelihood
    logit(p) <- a1[R] + a2[gender] + b1[R]*A + a3[Educ] + a4[children] + a5[employment] + a6[Race] + a7[LGBTQ],
    a1[R] ~ dnorm(0, 0.2),
    a2[gender] ~ dnorm(0, .2),
    b1[R] ~ dnorm(0, .2),
    a3[Educ] ~ dnorm(0, .2),
    a4[children] ~ dnorm(0, .2),
    a5[employment] ~ dnorm(0, .2),
    a6[Race] ~ dnorm(0, .2),
    a7[LGBTQ] ~ dnorm(0, .2)
  ), data = dat, chains = 1, log_lik=TRUE
)

# took about 6 minutes to fit

# save the model
save(model_SM2, file = 'models/model_SM2.RData')

plot(model_SM2, depth = 2)

traceplot( model_SM2 )
