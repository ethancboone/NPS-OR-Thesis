
# Introduction ------------------------------------------------------------

# We will start by loading in required packages and our custom generated 
# functions. 

# Required libraries
library(acled.api)
library(hablar)
library(data.table)
library(dplyr)


# Custom Functions
source("functions.R")

# Data Cleaning/Preparation -----------------------------------------------

# This section focuses on cleaning and preparing the ACLED data. This entails
# formatting the dataframe as required for further on analysis. It also entails
# assigning grid numbers to each rounded lat and lon pair. 

# Loading in ACLED dataframe. 
raw_data <- acled.api(
  email.address = "ethan.boone@nps.edu",
  access.key = "your_key",
  region = get.api.regions()[[1]]$code,
  start.date = "2018-01-01",
  end.date = Sys.Date(),
  add.variables = c("actor1","actor2", "latitude", "longitude")) %>%
  retype()

raw_data <- mutate(raw_data, 
                   lon = round(longitude),
                   lat = round(latitude))

# Writing our most recent pull to a csv to have a cached copy
write.csv(raw_data, file = paste(
  paste("raw_acled",year(Sys.Date()),month(Sys.Date()),sep = "_"),
  "csv",
  sep = "."
  ),
  )

# If error using api, we will load in most recent copy. 
if (!exists("raw_data")) {
  raw_data <- fread("raw_acled_2022_10.csv")
}


  
# Checking if our grid square dataframe has been constructed. 
if (!exists("grid.df")) {
  grid.df <- make_grids()
}

# Adding the grids to our raw data.
raw_grid <- left_join(raw_data, grid.df)

# Creating our dataframe with predictors
clean_data <- acled_cleaning(raw_grid)


# Time Series Forecasting -------------------------------------------------

# This section focuses on the time-series forecasting of the predictors. 

fcast_events <- events_forecast(data = raw_grid, 
                                   last_month = month(max(raw_grid$event_date)),
                                   max_year = year(max(raw_grid$event_date)))

fcast_stats <- stats_forecast(data = raw_grid,
                              last_month = month(max(raw_grid$event_date)),
                              max_year = year(max(raw_grid$event_date)))

# With the above fcast dataframes, we need to combine them and then clean them.
# Given that non of the predictors that were fed into the model should have 
# negative values we will change any negative value to zero. 

fcast_df <- left_join(fcast_stats, fcast_events)

# Prediction Model --------------------------------------------------------

# This section calls the function that uses a random forest to provide a 
# a prediction on the number of violent events with fatalities. 


