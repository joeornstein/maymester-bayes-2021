## Function to clean up CCES data

library(tigris)

clean_CCES <- function(d){
  d %>% 
    mutate(age_std = scale(2018 - birthyr), # standardize age
           gender = if_else(gender == 'Female', 1, 2), # index variable for gender
           has_children = if_else(child18 == 'Yes', 2, 1), # index variable for children under 18
           employment_status = as.integer(as.factor(employ)), #index variable for employment status
           race = as.integer(as.factor(race)), # index variable for race
           educ = case_when( educ == 'No HS' ~ 1, # index variable for education
                             educ == 'High school graduate' ~ 2,
                             educ == 'Some college' ~ 3,
                             educ == '2-year' ~ 4,
                             educ == '4-year' ~ 5,
                             educ == 'Post-grad' ~ 6),
           LGBTQ = if_else(sexuality == 'Heterosexual', 1, 2), # index variable for LGBTQ
           state = as.integer(as.factor(inputstate)), # index variable for state
           religious_importance = case_when(pew_religimp == 'Not at all important' ~ 1, # index variable for religious importance
                                            pew_religimp == 'Not too important' ~ 2,
                                            pew_religimp == 'Somewhat important' ~ 3,
                                            pew_religimp == 'Very important' ~ 4),
           urbancity = case_when(urbancity %in% c('Rural area', 'Other') ~ 1,
                                 urbancity == 'Town' ~ 2,
                                 urbancity == 'Suburb' ~ 3,
                                 urbancity == 'City' ~ 4),
           military_service = if_else(milstat_5 == 'Never Served', 1, 2),
           investor = if_else(investor == 'Yes', 2, 1),
           homeowner = if_else(ownhome == 'Own', 2, 1)) %>% 
    # merge with state abbreviations
    left_join(fips_codes %>% 
                mutate(inputstate = as.numeric(state_code)) %>%
                select(inputstate, abb = state) %>% 
                unique,
              by = 'inputstate')
}

make_list_for_ulam <- function(d){
  list(
    democratic2016 = d$democratic2016,
    age_std = d$age_std,
    gender = d$gender,
    has_children = d$has_children,
    employment_status = d$employment_status,
    race = d$race,
    educ = d$educ,
    LGBTQ = d$LGBTQ,
    state = d$state,
    religious_importance = d$religious_importance,
    urbancity = d$urbancity,
    military_service = d$military_service,
    investor = d$investor,
    homeowner = d$homeowner
  )
}
