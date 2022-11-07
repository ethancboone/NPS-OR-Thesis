library(docstring)


# make_grids --------------------------------------------------------------

make_grids <- function() {
  #' @title make_grids
  #'
  #' @description This function returns a dataframe which has the associated grid number for 
  #'each latitude and longitude pair. The first grid starts at the coordinate 
  #'(-90, -180) and increases in longitude before moving up in latitude.
  #'
  #' @return A dataframe of grid numbers for lat and lon pairs
  #' @export
  #'
  #' @examples 
  #' grid.df <- make_grids()
  #' head(grid.df)

  lats <- seq(-90,89)
  lons <- seq(-180,179)
  grid.df <- as.data.frame(expand.grid(lons,lats))
  colnames(grid.df) <- c("lon","lat")
  grid.df$grid_loc <- seq(0,dim(grid.df)[1]-1)
  
  return(grid.df)
}


# acled_cleaning ----------------------------------------------------------

acled_cleaning <- function(acled) {
  #' acled_cleaning
  #'
  #' @description This function cleans the raw dataframe collected from ACLED. 
  #' It returns the clean ACLED dataframe, which includes the predictors which
  #' are feature engineered. 
  #' 
  #' @param df Dataframe/Tibble. This is the raw ACLED dataframe.
  #'
  #' @return
  #' @export
  #'
  #' @examples
  #' clean_data <- acled_cleaning(raw_data)
  
  library(dplyr)
  library(tidyr)
  library(lubridate)
  
  stats_data <- acled %>%
    group_by(date = floor_date(event_date, "month"), grid = grid_loc) %>%
    summarise(unique_actor1 = n_distinct(actor1),
              unique_actor2 = n_distinct(actor2), 
              count_event_types = n_distinct(event_type),
              count_sub_event = n_distinct(sub_event_type),
              avg_fatalities = mean(fatalities),
              total_fatalities = sum(fatalities),
              range_fatalities = max(fatalities) - min(fatalities),
              event_count = n(),
              admin1_count = n_distinct(admin1),
              admin2_count = n_distinct(admin2),
              admin3_count = n_distinct(admin3)
    ) %>%
    ungroup() %>%
    complete(date, grid, fill = list(unique_actor1 = 0, 
                                     unique_actor2 = 0,
                                     count_event_types = 0,
                                     count_sub_event = 0,
                                     avg_fatalities = 0,
                                     total_fatalities = 0,
                                     range_fatalities = 0,
                                     event_count = 0,
                                     admin1_count = 0,
                                     admin2_count = 0,
                                     admin3_count = 0
    ))
  
  event_data <- group_by(acled, 
                         date = floor_date(event_date, "month"), 
                         grid = grid_loc, 
                         event_type) %>%
    summarise(counts = n()) %>%
    ungroup() %>%
    complete(date, grid, event_type, fill = list(counts = 0)) %>%
    pivot_wider(names_from = event_type, values_from = counts)
  
  clean_df <- cbind(stats_data, event_data[,-c(1,2)])
  
  return(clean_df)
}


# events_forecast ------------------------------------------------------

events_forecast <- function(data, last_month, max_year) {
  
  #' @title events_forecast
  #'
  #' @description This function is used to create a forecast of the different 
  #' event types for each grid. The time-series is forecasted using the 
  #' auto.arima() function. It returns a dataframe which contains the predicted
  #' values for the upcoming month. 
  #' 
  #' @param data Dataframe. This is the raw ACLED dataframe that has The path to the file that contains the dataframe with 
  #' predictors.
  #' @param last_month The month, as an integer, before the one you want 
  #' forecast (i.e. 6 if you want function to forecast 7 (July)).
  #' @param max_year The furthest year to be considered for analysis. It is 
  #' recommended to not go past 2018. 
  #'
  #' @return
  #' @export
  #'
  #' @examples
  
  # Loading required libraries
  library(dplyr)
  library(lubridate)
  library(data.table)
  library(tidyr)
  library(ggplot2)
  library(tidyverse)
  library(tidyquant)
  library(timetk)
  library(sweep)
  library(forecast)
  
  data <- data[data$year >= max_year, ]
  
  # Creating our dataframe that has the counts of each event type
  data.months <- data %>%
    group_by(date = floor_date(event_date, "month"), grid = grid_loc, event_type) %>%
    summarise(event_count = n()) %>%
    ungroup() %>%
    complete(date, grid, event_type, fill = list(event_count = 0)) %>%
    pivot_wider(names_from = event_type, values_from = event_count)
  
  # Cleaning the column names to prevent errors when fed into functions
  colnames(data.months) <- gsub("[: /-:]","_",colnames(data.months))
  
  # Given that we provided a last_month parameter, we need to make a cuttoff 
  # date that allows use to forecast one month ahead of the last month.
  
  # We need to make a conditional that checks if the last_month parameter is 
  # input as 12 (i.e. December)
  ifelse(last_month != 12,
         cutoff_date <- as.Date(paste(last_month + 1, 1, max_year, sep = "/"), format = "%m/%d/%Y"),
         cutoff_date <- as.Date(paste(1, 1, max_year + 1, sep = "/"), format = "%m/%d/%Y")
         )

  # From the new cutoff_date parameter, we will now subset the data.months 
  # dataframe accordingly. We also need to make sure that we are only selecting 
  # the numeric columns for this forecasting. 
  
  data.months <- filter(data.months, date < cutoff_date) %>%
    select(date, grid, where(is.numeric))
  
  # Converting our data.months dataframe to a long dataframe. The columns of 
  # this are date, grid, predictors, and value. 
  
  data_long <- pivot_longer(data.months, 
                            cols = 3:dim(data.months)[2],
                            names_to = "predictors")
  
  # This nests the predictors and value columns from data_long into one column 
  # called data. This is required to create the time-series object for each grid 
  # in the following step. 
  
  monthly_nest <- data_long %>%
    group_by(grid, predictors) %>% 
    nest()
  
  # Here we are creating a time-series object for each of the grids. We are
  # starting in the same year as the minimum year found in the data.months 
  # dataframe. 
  
  monthly_ts <- monthly_nest %>%
    mutate(data.ts = map(.x       = data, 
                         .f       = tk_ts, 
                         select   = -date, 
                         start    = year(min(data.months$date)),
                         freq     = 12))
  
  # Creating variable that will be used to track our progress 
  num_processed <- 0
  
  # Creating dataframe to hold our results. 
  combined <- data.frame()
  
  # NOTE: 
  # This for loop takes awhile. It could potentially be quicker by making all 
  # the commands within it as a function and then executed using one of the 
  # apply methods. That is a problem for a later time, as it still works in its 
  # current form. 
  
  grids <- sample(unique(data.months$grid),5)
  
  for (grid in grids) {
    
    grid_ts <- monthly_ts[monthly_ts$grid == grid,] %>% 
      ungroup() %>%
      select(-grid) %>%
      group_by(predictors)
    
    grid_fit <- grid_ts %>%
      mutate(fit.arima = map(data.ts, auto.arima))
    
    grid_fit_arima <- grid_fit %>%
      mutate(augment = map(fit.arima, sw_augment, timetk_idx = TRUE, rename_index = "date")) %>%
      unnest(augment)
    
    grid_fcast <- grid_fit_arima %>%
      mutate(fcast.arima = map(fit.arima, forecast, h = 1))
    
    grid_fcast_tidy <- grid_fcast %>%
      mutate(sweep = map(fcast.arima, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>%
      unnest(sweep)
    
    # This is the dataframe that gives us the forecasted values for the upcoming
    # month.
    fcast_df <- grid_fcast_tidy %>%
      select(predictors, index, key, value) %>%
      filter(key == "forecast") %>%
      distinct(index, value, key, .keep_all = FALSE) %>%
      pivot_wider(names_from = predictors, values_from = value) %>% 
      mutate(grid=grid) %>%
      select(date = index, grid, everything())
    
    # We will append each fcast_df to the combined dataframe. The dataframe 
    # combined will hold the forecast values for each grid. 
    
    combined <- rbind(combined, fcast_df)
    
    # This statement gives the progress of the function so far. 
    num_processed <- num_processed + 1
    ifelse(num_processed %% 25 == 0,
           print(paste("Grids fit:", 
                       num_processed, 
                       "of", 
                       length(unique(data.months$grid)),
                       sep = " ")),
           print("")
           )
  }
  return(combined)
}



# stats_forecast ----------------------------------------------------------

stats_forecast <- function(data, last_month, max_year) {
  
  #' @title stats_forecast
  #'
  #' @description This function is used to create a forecast of the next months
  #' feature engineered predictors. The time-series is forecasted using the 
  #' auto.arima() function. This function returns a dataframe which contains the 
  #' next months forecasted values. 
  #' 
  #' @param data Dataframe. This is the raw ACLED dataframe that has The path to the file that contains the dataframe with 
  #' predictors.
  #' @param last_month The month, as an integer, before the one you want 
  #' forecast (i.e. 6 if you want function to forecast 7 (July)).
  #' @param max_year The furthest year to be considered for analysis. It is 
  #' recommended to not go past 2018. 
  #'
  #' @return
  #' @export
  #'
  #' @examples
  
  
  # Loading required libraries
  library(dplyr)
  library(lubridate)
  library(data.table)
  library(tidyr)
  library(ggplot2)
  library(tidyverse)
  library(tidyquant)
  library(timetk)
  library(sweep)
  library(forecast)
  
  data <- data[data$year >= max_year, ]
  
  # Creating our dataframe that has the featured engineered information from the 
  # ACLED dataset. 
  data.months <- data %>%
    group_by(date = floor_date(event_date, "month"), grid = grid_loc) %>%
    summarise(unique_actor1 = n_distinct(actor1),
              unique_actor2 = n_distinct(actor2), 
              count_event_types = n_distinct(event_type),
              mode_event_type = max(event_type),
              count_sub_event = n_distinct(sub_event_type),
              mode_sub_event = max(sub_event_type),
              avg_fatalities = mean(fatalities),
              total_fatalities = sum(fatalities),
              range_fatalities = max(fatalities) - min(fatalities),
              event_count = n(),
              admin1_count = n_distinct(admin1),
              admin2_count = n_distinct(admin2),
              admin3_count = n_distinct(admin3)
    ) %>%
    ungroup() %>%
    complete(date, grid, fill = list(unique_actor1 = 0, 
                                     unique_actor2 = 0,
                                     count_event_types = 0,
                                     mode_event_type = "None",
                                     count_sub_event = 0,
                                     mode_sub_event = "None",
                                     avg_fatalities = 0,
                                     total_fatalities = 0,
                                     range_fatalities = 0,
                                     event_count = 0,
                                     admin1_count = 0,
                                     admin2_count = 0,
                                     admin3_count = 0
    )) %>%
    group_by(date, grid) %>%
    mutate_if(is.character, factor)
  
  # Given that we provided a last_month parameter, we need to make a cuttoff 
  # date that allows use to forecast one month ahead of the last month.
  
  # We need to make a conditional that checks if the last_month parameter is 
  # input as 12 (i.e. December)
  ifelse(last_month != 12,
         cutoff_date <- as.Date(paste(last_month + 1, 1, max_year, sep = "/"), format = "%m/%d/%Y"),
         cutoff_date <- as.Date(paste(1, 1, max_year + 1, sep = "/"), format = "%m/%d/%Y")
  )
  
  # From the new cutoff_date parameter, we will now subset the data.months 
  # dataframe accordingly. We also need to make sure that we are only selecting 
  # the numeric columns for this forecasting. 
  
  data.months <- filter(data.months, date < cutoff_date) %>%
    select(date, grid, where(is.numeric))
  
  # Converting our data.months dataframe to a long dataframe. The columns of 
  # this are date, grid, predictors, and value. 
  
  data_long <- pivot_longer(data.months, 
                            cols = 3:dim(data.months)[2],
                            names_to = "predictors")
  
  # This nests the predictors and value columns from data_long into one column 
  # called data. This is required to create the time-series object for each grid 
  # in the following step. 
  
  monthly_nest <- data_long %>%
    group_by(grid, predictors) %>% 
    nest()
  
  # Here we are creating a time-series object for each of the grids. We are
  # starting in the same year as the minimum year found in the data.months 
  # dataframe. 
  
  monthly_ts <- monthly_nest %>%
    mutate(data.ts = map(.x       = data, 
                         .f       = tk_ts, 
                         select   = -date, 
                         start    = year(min(data.months$date)),
                         freq     = 12))
  
  # Creating variable that will be used to track our progress 
  num_processed <- 0
  
  # Creating dataframe to hold our results. 
  combined <- data.frame()
  
  # NOTE: 
  # This for loop takes awhile. It could potentially be quicker by making all 
  # the commands within it as a function and then executed using one of the 
  # apply methods. That is a problem for a later time, as it still works in its 
  # current form. 
  
  grids <- sample(unique(data.months$grid), 5)
  
  for (grid in grids) {
    
    grid_ts <- monthly_ts[monthly_ts$grid == grid,] %>% 
      ungroup() %>%
      select(-grid) %>%
      group_by(predictors)
    
    grid_fit <- grid_ts %>%
      mutate(fit.arima = map(data.ts, auto.arima))
    
    grid_fit_arima <- grid_fit %>%
      mutate(augment = map(fit.arima, sw_augment, timetk_idx = TRUE, rename_index = "date")) %>%
      unnest(augment)
    
    grid_fcast <- grid_fit_arima %>%
      mutate(fcast.arima = map(fit.arima, forecast, h = 1))
    
    grid_fcast_tidy <- grid_fcast %>%
      mutate(sweep = map(fcast.arima, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>%
      unnest(sweep)
    
    # This is the dataframe that gives us the forecasted values for the upcoming
    # month.
    fcast_df <- grid_fcast_tidy %>%
      select(predictors, index, key, value) %>%
      filter(key == "forecast") %>%
      distinct(index, value, key, .keep_all = FALSE) %>%
      pivot_wider(names_from = predictors, values_from = value) %>% 
      mutate(grid=grid) %>%
      select(date = index, grid, everything())
    
    # We will append each fcast_df to the combined dataframe. The dataframe 
    # combined will hold the forecast values for each grid. 
    
    combined <- rbind(combined, fcast_df)
    
    # This statement gives the progress of the function so far. 
    num_processed <- num_processed + 1
    ifelse(num_processed %% 25 == 0,
           print(paste("Grids fit:", 
                       num_processed, 
                       "of", 
                       length(unique(data.months$grid)),
                       sep = " ")),
           print("")
    )
  }
  return(combined)
}


# build_responses ---------------------------------------------------------


build_responses <- function(acled_raw) {
  
  #' build_responses
  #'
  #' @description This function creates a dataframe that holds the response 
  #' variables for the prediction models. 
  #' 
  #' @param acled_raw Dataframe. Input the raw ACLED dataframe with grids for 
  #' this parameter.  
  #'
  #' @return
  #' @export
  #'
  #' @examples
  
  # Building our response dataframe from the raw ACLED data frame with grids.
  violent_count_df <- group_by(acled, 
                               date = floor_date(event_date, "month"), 
                               grid = grid_loc) %>%
    filter(event_type %in% c("Violence against civilians","Battles","Explosions/Remote violence"),
           fatalities > 0) %>%
    summarise(viol_event_count = n()) %>%
    ungroup() %>%
    complete(date, grid, fill = list(viol_event_count = 0)) %>%
    group_by(grid) %>%
    mutate(change = viol_event_count - lag(viol_event_count),
           trend = as.numeric(change > 0)) %>%
    ungroup()
  
  # Properly preparing our classification response to be a two level factor
  violent_count_df$trend <- as.factor(violent_count_df$trend)
  
  # Dropping the change column
  violent_count_df <- violent_count_df[,-c(4)]
  
  return(violent_count_df)
}


# extra_features ----------------------------------------------------------

extra_features <- function(data, acled_raw) {
  #' extra_features
  #' 
  #' @description This function creates some extra features to add to our 
  #' predictive model. These include the following:
  #' - Border: Binary (Y/N)
  #' - Month: Factor (12 Levels)
  #' - Quarter: Factor (4 Levels)
  #' 
  #' These are added onto any specified dataframe. It should be ensured that
  #' the supplied dataframe already has grids.
  #'
  #' @param data Dataframe. User-specified dataframe that the generated features 
  #' will be added to. 
  #' @param acled_raw Dataframe. This is the raw ACLED dataframe with grids. 
  #'
  #' @return
  #' @export
  #'
  #' @examples
  #' train.complete <- extra_features(train, acled_grid)
  #' str(train.complete)
  
  loc.features <- group_by(acled_raw, grid = grid_loc) %>%
    summarise(border = as.factor(as.numeric(n_distinct(country) > 1)))
  
  data <- left_join(data,loc.features)
  
  data <- mutate(data,
                 month = as.factor(month(date)),
                 quarter = as.factor(quarter(date))
  )
  
  data <- left_join(data, loc_info)
  
  return(data)
}

# classification_prediction ---------------------------------------------------

classification_prediction <- function(master_acled, violent_count_df, 
                                  rh_data, gecon) {
  
  #' classification_prediction
  #'
  #' @description This function completes the classification prediction. 
  #' 
  #' @param master_acled Dataframe. This is the clean ACLED feature engineered 
  #' dataframe. 
  #' @param violent_count_df Dataframe. This is the dataframe that contains the 
  #' responses for the prediction models. Obtained from the build_responses() 
  #' function. 
  #' @param rh_data Dataframe. This is the completed forecasted dataframe. 
  #' @param gecon (T/F). If True, the classification will include predictors 
  #' from the G-Econ dataset. 
  #'
  #' @return
  #' @export
  #'
  #' @examples
  
  
  # Loading in required libraries
  library(randomForest)
  library(e1071)
  library(class)
  library(xgboost)
  library(caret)
  
  # Making our master dataframes
  # Master train dataframe:
  master_train <- left_join(master_acled, violent_count_df)
  master_train <- filter(master_train, date < unique(rh_data$date))
  master_train$trend[is.na(master_train$trend)] <- 0
  master_train$viol_event_count[is.na(master_train$viol_event_count)] <- 0 
  
  master_train <- extra_features(master_train)
  
  gc()
  
  # Master test dataframe
  violent_count_df$date <- as.IDate(violent_count_df$date)
  master_test <- left_join(rh_data, violent_count_df)
  master_test$trend[is.na(master_test$trend)] <- 0
  master_test$viol_event_count[is.na(master_test$viol_event_count)] <- 0 
  
  master_test <- extra_features(master_test)
  
  gc()
  
  if (gecon) {
    gecon <- fread("GECON.csv")
    
    gecon <- group_by(gecon, Lon = LONGITUDE, Lat = LAT) %>%
      summarise(last(across(contains("DIS")))) %>%
      left_join(loc_info) %>%
      ungroup() %>%
      na.omit() %>%
      select(-Lon, -Lat) 
    
    master_train <- left_join(gecon, master_train)
    master_test <- left_join(gecon, master_test)
    
    gc()
  }
  
  
  # Creating our train and validation datasets
  sample.class <- sample(dim(master_train)[1], size = round(0.5*dim(master_train)[1]))
  
  # Creating a train, validation, and test dataframe that includes the binary response
  train.class <- select(master_train, -viol_event_count)[sample.class,]
  valid.class <- select(master_train, -viol_event_count)[-sample.class,]
  test.class <- select(master_test, -viol_event_count) %>%
    ungroup()
  
  levels(test.class$month) <- seq(1,12) 
  levels(test.class$quarter) <- seq(1,4) 
  
  extra.rows <- dim(train.class)[1] %% 10
  
  if (extra.rows != 0) {
    train.class <- train.class[-c(1:extra.rows),]
  }
  
  train.class.split <- split(train.class, rep(1:10))
  
  gc()
  
  train.class1 <- train.class.split$'1'
  train.class2 <- train.class.split$'2'
  train.class3 <- train.class.split$'3'
  train.class4 <- train.class.split$'4'
  train.class5 <- train.class.split$'5'
  train.class6 <- train.class.split$'6'
  train.class7 <- train.class.split$'7'
  train.class8 <- train.class.split$'8'
  train.class9 <- train.class.split$'9'
  train.class10 <- train.class.split$'10'
  
  gc()
  
  # Random Forest Section
  print("Training Random Forest Model")
  rf.class1 <- randomForest(trend~., data = select(train.class1, -date, -grid))
  print("RF1")
  gc()
  rf.class2 <- randomForest(trend~., data = select(train.class2, -date, -grid))
  print("RF2")
  gc()
  rf.class3 <- randomForest(trend~., data = select(train.class3, -date, -grid))
  print("RF3")
  gc()
  rf.class4 <- randomForest(trend~., data = select(train.class4, -date, -grid))
  print("RF4")
  gc()
  rf.class5 <- randomForest(trend~., data = select(train.class5, -date, -grid))
  print("RF5")
  gc()
  rf.class6 <- randomForest(trend~., data = select(train.class6, -date, -grid))
  print("RF6")
  gc()
  rf.class7 <- randomForest(trend~., data = select(train.class7, -date, -grid))
  print("RF7")
  gc()
  rf.class8 <- randomForest(trend~., data = select(train.class8, -date, -grid))
  print("RF8")
  gc()
  rf.class9 <- randomForest(trend~., data = select(train.class9, -date, -grid))
  print("RF9")
  gc()
  rf.class10 <- randomForest(trend~., data = select(train.class10, -date, -grid))
  print("RF10")
  gc()
  
  rf.class <- combine(rf.class1,
                      rf.class2,
                      rf.class3,
                      rf.class4,
                      rf.class5,
                      rf.class6,
                      rf.class7,
                      rf.class8,
                      rf.class9,
                      rf.class10)
  
  gc()
  rf.class.valid.prob <- predict(rf.class, 
                                 newdata = select(valid.class, -date, -grid), 
                                 type = "prob")[,1]
  rf.class.valid.pred <- predict(rf.class, select(valid.class, -date, -grid), type = "response")
  
  rf.class.prob <- predict(rf.class, select(test.class, -date, -grid), type = "prob")[,1]
  rf.class.pred <- predict(rf.class, select(test.class, -date, -grid), type = "response")
  
  # Creating dataframes to hold our data
  
  # Validation dataframe
  rf.valid.df <- cbind(select(valid.class, date, grid, trend),
                       rf.class.valid.pred) %>%
    mutate(model = "RF",
           ouput = "Validation")
  
  colnames(rf.valid.df) <- c("date","grid","actual","prediction", "model", "output")
  
  # Test dataframe
  rf.test.df <- cbind(select(test.class, date, grid, trend),
                      rf.class.pred) %>%
    mutate(model = "RF",
           ouput = "Test")
  
  colnames(rf.test.df) <- c("date","grid","actual","prediction", "model", "output")
  
  rf.df <- rbind(rf.valid.df, rf.test.df)
  
  print("Finished with Random Forest")
  
  # Logistic Regression Section
  print("Training Logistic Regression")
  logit.class <- glm(trend ~.,family=binomial(link='logit'),data=select(train.class, -date, -grid))
  
  logit.class.valid.pred <- predict(logit.class, select(valid.class, -date, -grid), type = "response")
  logit.class.valid.pred <- as.factor(as.numeric(logit.class.valid.pred > 0.5, 1, 0))
  
  logit.class.pred <- predict(logit.class, select(test.class, -date, -grid), type = "response")
  logit.class.pred <- as.factor(as.numeric(logit.class.pred > 0.5, 1, 0))
  
  # Creating a dataframe to hold our output data. 
  
  # Validation dataframe
  logit.valid.df <- cbind(select(valid.class, date, grid, trend),
                          logit.class.valid.pred) %>%
    mutate(model = "Logit",
           output = "Validation")
  
  colnames(logit.valid.df) <- c("date","grid","actual","prediction", "model", "output")
  
  # Test dataframe
  logit.test.df <- cbind(select(test.class, date, grid, trend),
                         logit.class.pred) %>%
    mutate(model = "Logit",
           output = "Test")
  
  colnames(logit.test.df) <- c("date","grid","actual","prediction", "model", "output")
  
  # Combining validation and test into one dataframe 
  logit.df <- rbind(logit.valid.df,logit.test.df)
  
  print("Finished with Logistic Regression")
  # Combining all dataframe
  
  results.df <- rbind(rf.df,logit.df)
  
  return(results.df)
}


# regression_prediction ---------------------------------------------------

regression_prediction <- function(master_acled, violent_count_df, rh_data, gecon) {
  
  #' regression_prediction
  #'
  #' @description This function completes the regression prediction. 
  #' 
  #' @param master_acled Dataframe. This is the clean ACLED feature engineered 
  #' dataframe. 
  #' @param violent_count_df Dataframe. This is the dataframe that contains the 
  #' responses for the prediction models. Obtained from the build_responses() 
  #' function. 
  #' @param rh_data Dataframe. This is the completed forecasted dataframe. 
  #' @param gecon (T/F). If True, the classification will include predictors 
  #' from the G-Econ dataset. 
  #'
  #' @return
  #' @export
  #'
  #' @examples

  
  # Loading in required libraries
  library(randomForest)
  library(e1071)
  library(class)
  library(xgboost)
  library(caret)
  
  # Making our master dataframes
  # Master train dataframe:
  master_train <- left_join(master_acled, violent_count_df)
  master_train <- filter(master_train, date < unique(rh_data$date))
  master_train$trend[is.na(master_train$trend)] <- 0
  master_train$viol_event_count[is.na(master_train$viol_event_count)] <- 0 
  
  master_train <- extra_features(master_train)
  
  gc()
  
  # Master test dataframe
  violent_count_df$date <- as.IDate(violent_count_df$date)
  master_test <- left_join(rh_data, violent_count_df)
  master_test$trend[is.na(master_test$trend)] <- 0
  master_test$viol_event_count[is.na(master_test$viol_event_count)] <- 0 
  
  master_test <- extra_features(master_test)
  
  gc()
  
  if (gecon) {
    gecon <- fread("GECON.csv")
    
    gecon <- group_by(gecon, Lon = LONGITUDE, Lat = LAT) %>%
      summarise(last(across(contains("DIS")))) %>%
      left_join(loc_info) %>%
      ungroup() %>%
      na.omit() %>%
      select(-Lon, -Lat) 
    
    master_train <- left_join(gecon, master_train)
    master_test <- left_join(gecon, master_test)
    
    gc()
  }
  
  
  # Creating our train and validation datasets
  sample.class <- sample(dim(master_train)[1], size = round(0.5*dim(master_train)[1]))
  
  # Creating a train, validation, and test dataframe that includes the binary response
  train.class <- select(master_train, -trend)[sample.class,]
  valid.class <- select(master_train, -trend)[-sample.class,]
  test.class <- select(master_test, -trend) %>%
    ungroup()
  
  levels(test.class$month) <- seq(1,12) 
  levels(test.class$quarter) <- seq(1,4) 
  
  extra.rows <- dim(train.class)[1] %% 10
  
  if (extra.rows != 0) {
    train.class <- train.class[-c(1:extra.rows),]
  }
  
  train.class.split <- split(train.class, rep(1:10))
  
  gc()
  
  train.class1 <- train.class.split$'1'
  train.class2 <- train.class.split$'2'
  train.class3 <- train.class.split$'3'
  train.class4 <- train.class.split$'4'
  train.class5 <- train.class.split$'5'
  train.class6 <- train.class.split$'6'
  train.class7 <- train.class.split$'7'
  train.class8 <- train.class.split$'8'
  train.class9 <- train.class.split$'9'
  train.class10 <- train.class.split$'10'
  
  gc()
  
  # Random Forest Section
  print("Training Random Forest Model")
  rf.class1 <- randomForest(viol_event_count~., data = select(train.class1, -date, -grid))
  print("RF1")
  gc()
  rf.class2 <- randomForest(viol_event_count~., data = select(train.class2, -date, -grid))
  print("RF2")
  gc()
  rf.class3 <- randomForest(viol_event_count~., data = select(train.class3, -date, -grid))
  print("RF3")
  gc()
  rf.class4 <- randomForest(viol_event_count~., data = select(train.class4, -date, -grid))
  print("RF4")
  gc()
  rf.class5 <- randomForest(viol_event_count~., data = select(train.class5, -date, -grid))
  print("RF5")
  gc()
  rf.class6 <- randomForest(viol_event_count~., data = select(train.class6, -date, -grid))
  print("RF6")
  gc()
  rf.class7 <- randomForest(viol_event_count~., data = select(train.class7, -date, -grid))
  print("RF7")
  gc()
  rf.class8 <- randomForest(viol_event_count~., data = select(train.class8, -date, -grid))
  print("RF8")
  gc()
  rf.class9 <- randomForest(viol_event_count~., data = select(train.class9, -date, -grid))
  print("RF9")
  gc()
  rf.class10 <- randomForest(viol_event_count~., data = select(train.class10, -date, -grid))
  print("RF10")
  gc()
  
  rf.class <- combine(rf.class1,
                      rf.class2,
                      rf.class3,
                      rf.class4,
                      rf.class5,
                      rf.class6,
                      rf.class7,
                      rf.class8,
                      rf.class9,
                      rf.class10)
  
  gc()
  
  rf.class.valid.pred <- predict(rf.class, select(valid.class, -date, -grid), type = "response")
  
  rf.class.pred <- predict(rf.class, select(test.class, -date, -grid), type = "response")
  
  # Creating dataframes to hold our data
  
  # Validation dataframe
  rf.valid.df <- cbind(select(valid.class, date, grid, viol_event_count),
                       rf.class.valid.pred) %>%
    mutate(model = "RF",
           ouput = "Validation")
  
  colnames(rf.valid.df) <- c("date","grid","actual","prediction", "model", "output")
  
  # Test dataframe
  rf.test.df <- cbind(select(test.class, date, grid, viol_event_count),
                      rf.class.pred) %>%
    mutate(model = "RF",
           ouput = "Test")
  
  colnames(rf.test.df) <- c("date","grid","actual","prediction", "model", "output")
  
  rf.df <- rbind(rf.valid.df, rf.test.df)
  
  print("Finished with Random Forest")
  
  # Logistic Regression Section
  print("Training Logistic Regression")
  logit.class <- lm(viol_event_count ~.,data=select(train.class, -date, -grid))
  
  logit.class.valid.pred <- predict(logit.class, select(valid.class, -date, -grid), type = "response")
  
  logit.class.pred <- predict(logit.class, select(test.class, -date, -grid), type = "response")
  
  # Creating a dataframe to hold our output data. 
  
  # Validation dataframe
  logit.valid.df <- cbind(select(valid.class, date, grid, viol_event_count),
                          logit.class.valid.pred) %>%
    mutate(model = "Logit",
           output = "Validation")
  
  colnames(logit.valid.df) <- c("date","grid","actual","prediction", "model", "output")
  
  # Test dataframe
  logit.test.df <- cbind(select(test.class, date, grid, viol_event_count),
                         logit.class.pred) %>%
    mutate(model = "Logit",
           output = "Test")
  
  colnames(logit.test.df) <- c("date","grid","actual","prediction", "model", "output")
  
  # Combining validation and test into one dataframe 
  logit.df <- rbind(logit.valid.df,logit.test.df)
  
  print("Finished with Logistic Regression")
  # Combining all dataframe
  
  results.df <- rbind(rf.df,logit.df)
  
  return(results.df)
}
