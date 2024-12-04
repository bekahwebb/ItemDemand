# Load required libraries
library(forecast) # For SMAPE calculation
library(dplyr)
library(vroom)
library(prophet)

# Load data
item_demand_train <- vroom::vroom('train.csv')  # Rows: 913000 Columns: 4
item_demand_test <- vroom::vroom('test.csv')    # Rows: 45000 Columns: 4

# Ensure 'date' is in the correct format
item_demand_train$date <- as.Date(item_demand_train$date)
item_demand_test$date <- as.Date(item_demand_test$date)

# Define the number of stores and sample stores
nStores <- max(item_demand_train$store)
sample_stores <- sample(1:nStores, 4)  # Randomly sample 4 stores for tuning

# Initialize an empty list to store results
results <- list()

# Sequential loop over sampled stores
for (s in sample_stores) {
  # Filter data for the current store
  store_train <- item_demand_train %>% filter(store == s)
  store_test <- item_demand_test %>% filter(store == s)
  
  # Initialize lists for storing predictions and SMAPE results
  store_predictions <- list()
  smape_list <- list()
  
  # Loop through each item in the store
  for (i in unique(store_train$item)) {
    # Filter data for the current store-item combination
    train_data <- store_train %>% filter(item == i)
    test_data <- store_test %>% filter(item == i)
    
    if (nrow(train_data) > 0 && nrow(test_data) > 0) {
      # Prepare data for Prophet
      prophet_data <- train_data %>%
        select(ds = date, y = sales)
      
      # Fit the Prophet model
      prophet_model <- prophet()
      prophet_model <- add_country_holidays(prophet_model, country_name = 'US')
      prophet_model <- fit.prophet(prophet_model, prophet_data)
      
      # Create future data frame for prediction
      future_dates <- make_future_dataframe(prophet_model, periods = nrow(test_data), freq = "day")
      
      # Make predictions
      forecast <- predict(prophet_model, future_dates)
      
      # Check if the number of predictions matches the test data
      if (nrow(forecast) == nrow(test_data)) {
        # Calculate SMAPE
        smape_value <- Metrics::smape(test_data$sales, forecast$yhat)
        
        # Store predictions and SMAPE
        store_predictions[[i]] <- test_data %>%
          mutate(predicted_sales = forecast$yhat) %>%
          select(id, predicted_sales)
        
        smape_list[[i]] <- tibble(
          store = s,
          item = i,
          smape_value = smape_value
        )
      } else {
        warning(paste("Prediction mismatch for store", s, "item", i))
      }
    }
  }
  
  # Combine predictions and SMAPE results for the current store
  results[[s]] <- list(
    predictions = bind_rows(store_predictions),
    smape_results = bind_rows(smape_list)
  )
}

# Combine all results
all_predictions <- bind_rows(lapply(results, `[[`, "predictions"))
all_smape_results <- bind_rows(lapply(results, `[[`, "smape_results"))

# Print the SMAPE results
print(all_smape_results)

# Optionally save SMAPE results to a file
# write.csv(all_smape_results, "smape_results.csv", row.names = FALSE)
#maybe the threshold for smote is under 20?