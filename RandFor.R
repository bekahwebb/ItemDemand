library(vroom)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(dplyr)
library(recipes)
library(tidymodels)
library(forecast)
library(parsnip)
library(modeltime)
library(timetk)
library(prophet)

# Load data
item_demand_test <- vroom('test.csv')  # Rows: 45000 Columns: 4  
item_demand_train <- vroom('train.csv') # Rows: 913000 Columns: 4 


# Ensure 'date' is in the correct format
#item_demand_train$date <- as.Date(item_demand_train$date)

# Select the first 5 stores and 5 items
stores <- 1:5
items <- 1:5

# Initialize a list to store the best tuning parameters
best_tunes <- list()

# Loop through each store and item
for (s in stores) { 
  for (i in items) { 
    # Filter training data for the current store-item combination
    storeItemTrain <- item_demand_train %>% 
      filter(store == s, item == i)
    
    # Time series split (we use initial_time_split here for simplicity)
    ts_splits <- time_series_cv(storeItemTrain, 
                               assess = "3 months", 
                               cumulative = TRUE)
    # Define recipe
    rf_recipe <- recipe(sales ~ ., data = storeItemTrain) %>%
      step_date(date, features = c("month", "year")) %>%
      step_dummy(all_nominal_predictors())
    
    # Define random forest model with tuning
    rf_model <- rand_forest(
      mtry = tune(),
      min_n = tune(),
      trees = 500
    ) %>%
      set_engine("ranger") %>%
      set_mode("regression")
    
    # Workflow
    rf_workflow <- workflow() %>%
      add_recipe(rf_recipe) %>%
      add_model(rf_model)
    
    # Hyperparameter grid
    rf_grid <- grid_regular(
      mtry(range = c(2, 10)),
      min_n(range = c(5, 30)),
      levels = 5
    )
   
    # Tune the model
    rf_tuned <- tune_grid(
      rf_workflow,
      resamples = ts_splits,  # Use the rset object created by time_series_cv
      grid = rf_grid,
      metrics = metric_set(rmse)
    )
    
    # Find the best parameters
    best_params <- select_best(rf_tuned, metric = "rmse")
    
    # Save the best parameters for this store-item combination
    best_tunes[[paste0("Store_", s, "_Item_", i)]] <- best_params
  }
}

# Display best tuning parameters
best_tunes
#$Store_1_Item_1
# A tibble: 1 × 3
#mtry min_n .config              
#<int> <int> <chr>                
# 1     4    23 Preprocessor1_Model17