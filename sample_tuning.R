library(tidyverse)
library(vroom)
library(prophet)
library(foreach)
library(doParallel)
library(Metrics)

# Set up parallel processing (using 4 cores as an example)
cl <- makeCluster(4)  # Adjust this to match the number of cores you want to use
registerDoParallel(cl)

# Load data
item_demand_test <- vroom('test.csv')  # Rows: 45000 Columns: 4  
item_demand_train <- vroom('train.csv')  # Rows: 913000 Columns: 4 

# Ensure 'date' is in the correct format
item_demand_train$date <- as.Date(item_demand_train$date)
item_demand_test$date <- as.Date(item_demand_test$date)

# Get the number of stores and items
nStores <- max(item_demand_train$store)
nItems <- max(item_demand_train$item)

# Set the number of store-item combinations for tuning (let's tune just 5 combinations to start)
tune_combinations <- sample(1:nStores, 5)  # Randomly sample 5 stores for tuning

# Initialize an empty data frame for predictions
all_preds <- tibble()
# Parallelize the loop over store-item combinations
results <- foreach(
  s = tune_combinations,
  .combine = 'list',
  .packages = c('prophet', 'dplyr', 'vroom', 'Metrics')
) %dopar% {
  
  # Filter data for the current store
  storeItemTrain <- item_demand_train %>% filter(store == s)
  storeItemTest <- item_demand_test %>% filter(store == s)
  
  # Initialize lists for predictions and SMAPE results
  store_preds <- list()
  smape_list <- list()
  
  # Loop through the items in the selected store
  for (i in 1:nItems) {
    # Filter data for the current store-item pair
    storeItemTrainSubset <- storeItemTrain %>% filter(item == i)
    storeItemTestSubset <- storeItemTest %>% filter(item == i)
    
    if (nrow(storeItemTrainSubset) > 0) {
      # Prepare data for Prophet
      prophet_data <- storeItemTrainSubset %>%
        select(ds = date, y = sales)
      
      # Fit the Prophet model
      prophet_model <- prophet()
      prophet_model <- add_country_holidays(prophet_model, country_name = 'US')
      prophet_model <- fit.prophet(prophet_model, prophet_data)
      
      # Make future data frame for prediction
      future_dates <- make_future_dataframe(prophet_model, periods = nrow(storeItemTestSubset), freq = "day")
      
      # Predict
      forecast <- predict(prophet_model, future_dates)
      
      # Check if the number of predictions matches the test data
      if (nrow(forecast) == nrow(storeItemTestSubset)) {
        # Format predictions to match Kaggle submission
        preds <- storeItemTestSubset %>%
          mutate(sales = forecast$yhat) %>%
          select(id, sales)
        
        # Append predictions to the list
        store_preds[[i]] <- preds
        
        # Calculate SMAPE for each iteration
        smape_value <- smape(storeItemTestSubset$sales, forecast$yhat)
        smape_list[[length(smape_list) + 1]] <- tibble(store = s, item = i, smape_value = smape_value)
      } else {
        warning("Mismatch in rows for store ", s, " item ", i)
      }
    }
  }
  
  # Combine predictions and SMAPE results for the current store
  list(
    predictions = do.call(rbind, store_preds),
    smape_results = do.call(rbind, smape_list)
  )
}

# Stop the cluster after use
stopCluster(cl)

# Extract and combine all results
all_preds <- bind_rows(lapply(results, `[[`, "predictions"))
smape_results <- bind_rows(lapply(results, `[[`, "smape_results"))

# Print the SMAPE results
print(smape_results)

# Optionally save SMAPE results to a file
# write.csv(smape_results, "smape_results.csv", row.names = FALSE)
