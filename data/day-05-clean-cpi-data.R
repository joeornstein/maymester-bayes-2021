## Clean up CPI, GDP per capita, and regime type data

library(tidyverse)
library(readxl)
library(janitor)

# load Corruption Perceptions Index
# from: https://www.transparency.org/en/cpi/2020/
cpi <- read_xlsx('data/CPI2020_GlobalTablesTS_210125.xlsx', skip = 1) %>% 
  clean_names() %>% 
  select(country, iso3, cpi_score = cpi_score_2020) %>% 
  # clean up Cote D'Ivoire name
  mutate(country = if_else(iso3 == 'CIV', "Cote D'Ivoire", country))

# load gdp per capita dataset
# from: https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
gdp <- read_csv('data/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_2357064.csv', 
                skip = 4) %>% 
  clean_names() %>% 
  select(iso3 = country_code,
         gdp_per_capita = x2019)

# load Polity V dataset
# from: http://www.systemicpeace.org/inscrdata.html
polity <- read_xls('data/p5v2018.xls') %>% 
  # keep most recent scores (2018)
  filter(year == 2018) %>% 
  # let's code the regime as a democracy if polity > 5.
  # I generally don't advise discretizing numeric variables,
  # but we want practice with categorical variables 
  # so here we go...
  mutate(democracy = if_else(polity2 > 5, 1, 0)) %>% 
  select(country, democracy) %>% 
  # rename some countries to match the names in the CPI dataset
  mutate(country = case_when(country == 'UAE' ~ 'United Arab Emirates',
                             country == 'United States' ~ 'United States of America',
                             country == 'Korea South' ~ 'Korea, South',
                             country == 'Korea North' ~ 'Korea, North',
                             country == 'Cape Verde' ~ 'Cabo Verde',
                             country == 'Czech Republic' ~ 'Czechia',
                             country == 'Slovak Republic' ~ 'Slovakia',
                             country == 'Timor Leste' ~ 'Timor-Leste',
                             country == 'Bosnia' ~ 'Bosnia and Herzegovina',
                             country == 'Swaziland' ~ 'Eswatini',
                             country == 'Myanmar (Burma)' ~ 'Myanmar',
                             country == 'Congo Brazzaville' ~ 'Congo',
                             country == 'Congo Kinshasa' ~ 'Democratic Republic of the Congo',
                             country == 'Guinea-Bissau' ~ 'Guinea Bissau',
                             country == 'Sudan-North' ~ 'Sudan',
                             TRUE ~ country))

d <- cpi %>% 
  left_join(gdp, by = 'iso3') %>% 
  left_join(polity, by = 'country')

write_csv(d, 'data/clean_data.csv')
