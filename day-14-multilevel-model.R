## Build a multilevel model predicting 2016 vote choice

library(tidyverse)
library(rethinking)

## Load and clean data --------------------------------------------

source('day-14-clean-cces.R') # a function to clean up CCES

# load and clean the training data
train <- 'data/CCES-Train.csv' %>%
  read_csv %>% 
  clean_CCES


