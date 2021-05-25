## Clean up CPI and GDP per capita data

library(tidyverse)
library(readxl)
library(janitor)

# load Corruption Perceptions Index
# from: https://www.transparency.org/en/cpi/2020/
cpi <- read_xlsx('data/CPI2020_GlobalTablesTS_210125.xlsx', skip = 1) %>% 
  clean_names() %>% 
  select(country, iso3, cpi_score = cpi_score_2020)

# load gdp per capita dataset
# from: https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
gdp <- read_csv('data/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_2357064.csv', 
                skip = 4) %>% 
  clean_names() %>% 
  select(iso3 = country_code,
         gdp_per_capita = x2019)

d <- left_join(cpi, gdp, by = 'iso3')

write_csv(d, 'data/cpi_gdp.csv')
