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

# Select 5 stores and 5 items
stores <- c(1, 2, 3, 4, 5)
items <- c(1, 10, 20, 30, 40)

# Prepare an empty list to store results
results <- list()

# Iterate through each store and item
for (store_id in stores) {
  for (item_id in items) {
    
    # Filter the dataset for the specific store and item
    storeItem <- item_demand_train %>%
      filter(store == store_id, item == item_id)
    
    # Skip if no data for this combination
    if (nrow(storeItem) == 0) next
    
    # Create recipe
    store_recipe <- recipe(sales ~ ., data = storeItem) %>%
      step_date(date, features = c("dow", "month")) %>%  # Weekly and monthly trends
      step_mutate(date_dow = factor(date_dow),
                  date_month = factor(date_month)) %>%
      step_rm(c(store, item))
    
    # Prep recipe
    prepped_recipe <- prep(store_recipe)
    baked <- bake(prepped_recipe, new_data = storeItem)
    
    # Define random forest model
    my_time_mod <- rand_forest(mtry = tune(),
                               min_n = tune(),
                               trees = 500) %>%
      set_engine("ranger") %>%
      set_mode("regression")
    
    # Create workflow
    wf <- workflow() %>%
      add_recipe(store_recipe) %>%
      add_model(my_time_mod)
    
    # Tuning grid
    tuningGridForest <- grid_regular(
      min_n(),
      mtry(range = c(1, 10)),
      levels = 4
    )
    
    # Cross-validation folds
    folds <- vfold_cv(storeItem, v = 4, repeats = 1)
    
    # Tune the model
    CV_results <- wf %>%
      tune_grid(
        resamples = folds,
        grid = tuningGridForest,
        metrics = metric_set(smape),
        control = control_grid(verbose = TRUE)
      )
    
    # Extract best results
    bestTune <- CV_results %>%
      show_best(n = 1, metric = "smape")
    
    # Store the results
    results[[paste0("Store_", store_id, "_Item_", item_id)]] <- list(
      "BestTune" = bestTune,
      "CV_Results" = CV_results
    )
    
    print(paste0("Finished tuning for Store: ", store_id, ", Item: ", item_id))
  }
}

# View all results
results
# results under a 15.3 smape score
# $Store_5_Item_10
# $Store_5_Item_10$BestTune
# # A tibble: 1 × 8
# mtry min_n .metric .estimator  mean     n std_err .config              
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1     7    27 smape   standard    11.6     4   0.199 Preprocessor1_Model11

# $Store_4_Item_30
# $Store_4_Item_30$BestTune
# # A tibble: 1 × 8
# mtry min_n .metric .estimator  mean     n std_err .config              
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1     1     2 smape   standard    13.5     4   0.172 Preprocessor1_Model01
# 
# $Store_4_Item_20
# $Store_4_Item_20$BestTune
# # A tibble: 1 × 8
# mtry min_n .metric .estimator  mean     n std_err .config              
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1     4    27 smape   standard    12.1     4   0.156 Preprocessor1_Model07

# $Store_4_Item_10
# $Store_4_Item_10$BestTune
# # A tibble: 1 × 8
# mtry min_n .metric .estimator  mean     n std_err .config              
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1     7    14 smape   standard    10.1     4   0.237 Preprocessor1_Model10

# $Store_3_Item_40
# $Store_3_Item_40$BestTune
# # A tibble: 1 × 8
# mtry min_n .metric .estimator  mean     n std_err .config              
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1    10    40 smape   standard    15.0     4   0.241 Preprocessor1_Model16
# $Store_3_Item_30
# $Store_3_Item_30$BestTune
# # A tibble: 1 × 8
# mtry min_n .metric .estimator  mean     n std_err .config              
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1     7    27 smape   standard    13.6     4   0.154 Preprocessor1_Model11
# $Store_3_Item_20
# $Store_3_Item_20$BestTune
# # A tibble: 1 × 8
# mtry min_n .metric .estimator  mean     n std_err .config
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>
#   1     4    27 smape   standard    12.1     4   0.285 Preprocessor1_Model07
# $Store_3_Item_10
# $Store_3_Item_10$BestTune
# # A tibble: 1 × 8
# mtry min_n .metric .estimator  mean     n std_err .config              
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1    10    27 smape   standard    9.83     4   0.221 Preprocessor1_Model15
# $Store_2_Item_40
# $Store_2_Item_40$BestTune
# # A tibble: 1 × 8
# mtry min_n .metric .estimator  mean     n std_err .config              
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1    10    40 smape   standard    14.5     4  0.0279 Preprocessor1_Model16
# $Store_2_Item_30
# $Store_2_Item_30$BestTune
# # A tibble: 1 × 8
# mtry min_n .metric .estimator  mean     n std_err .config              
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1     4    27 smape   standard    12.4     4   0.205 Preprocessor1_Model07
# $Store_2_Item_20
# $Store_2_Item_20$BestTune
# # A tibble: 1 × 8
# mtry min_n .metric .estimator  mean     n std_err .config
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>
#   1    10    27 smape   standard    11.3     4   0.178 Preprocessor1_Model15
# $Store_2_Item_10
# $Store_2_Item_10$BestTune
# # A tibble: 1 × 8
# mtry min_n .metric .estimator  mean     n std_err .config              
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1     4    14 smape   standard    9.20     4   0.167 Preprocessor1_Model06
# $Store_1_Item_30
# $Store_1_Item_30$BestTune
# # A tibble: 1 × 8
# mtry min_n .metric .estimator  mean     n std_err .config              
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1     7    27 smape   standard    14.9     4   0.218 Preprocessor1_Model11
#

# $Store_1_Item_20
# $Store_1_Item_20$BestTune
# # A tibble: 1 × 8
# mtry min_n .metric .estimator  mean     n std_err .config              
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1     4    40 smape   standard    13.3     4   0.138 Preprocessor1_Model08
# $Store_1_Item_10
# $Store_1_Item_10$BestTune
# # A tibble: 1 × 8
# mtry min_n .metric .estimator  mean     n std_err .config              
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1    10    27 smape   standard    11.1     4   0.398 Preprocessor1_Model15
#The average tuning parameters and SMAPE mean score are as follows:

#Average mtry: 6.6
#Average min_n: 26.2
#Average SMAPE Mean: 12.302