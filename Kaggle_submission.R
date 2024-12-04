library(tidyverse)
library(tidymodels)
library(vroom)
library(prophet)
library(modeltime)
library(timetk)

# loading data for kaggle submission
# Define file paths (use Kaggle's dataset paths)
# train_path <- "/kaggle/input/demand-forecasting-kernels-only/train.csv"
# test_path <- "/kaggle/input/demand-forecasting-kernels-only/test.csv"
# 
# # Load data
# train <- vroom(train_path)
# test <- vroom(test_path)

# Load data
item_demand_test <- vroom('test.csv') # Rows: 45000 Columns: 4  
item_demand_train <- vroom('train.csv')# Rows: 913000 Columns: 4 

# Ensure 'date' is in the correct format
item_demand_train$date <- as.Date(item_demand_train$date)
item_demand_test$date <- as.Date(item_demand_test$date)

nStores <- max(train$store)
nItems <- max(train$item)
# #run for 1 store to check
# s=9 
# i=27
for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- train %>%
      filter(store==s, item==i)
    storeItemTest <- test %>%
      filter(store==s, item==i)
    
    ## Fit storeItem models here
    cv_split <- time_series_split(storeItemTrain, assess="3 months", cumulative = TRUE)
    
    prophet_model <- prophet_reg() %>%
      set_engine(engine = "prophet") %>%
      fit(sales ~ date, data = training(cv_split))
    
    cv_results <- modeltime_calibrate(prophet_model,
                                      new_data = testing(cv_split))
    
    fullfit <- cv_results %>%
      modeltime_refit(data=storeItemTrain)
    
    preds <- fullfit %>% 
      modeltime_forecast(
        new_data = storeItemTest,
        actual_data = storeItemTrain
      ) %>% 
      filter(!is.na(.model_id)) %>%
      mutate(id=storeItemTest$id) %>%
      select(id, .value) %>%
      rename(sales=.value)
    
    
    ## Save storeItem predictions
    if(s==1 & i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
    
  }
}

vroom_write(all_preds, file="submission.csv", delim=",")