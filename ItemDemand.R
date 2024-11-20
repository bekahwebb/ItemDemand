library(tidyverse)
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


item_demand_test <- vroom('test.csv') # Rows: 45000 Columns: 4  
item_demand_train <- vroom('train.csv')# Rows: 913000 Columns: 4 
item_demand_train

## Filter down to just 1 store from 50 item for exploration and model building
#store 1
storeItem1 <- item_demand_train %>%
filter(store==7, item==27)

## Filter down to just 1 store from 50 item for exploration and model building
#store 2
storeItem2 <- item_demand_train %>%
  filter(store==9, item==48)


#EDA Time Series
one <- storeItem1 %>%
ggplot(mapping=aes(x=date, y=sales)) +
geom_line() +
geom_smooth(se=FALSE)


two <- storeItem2 %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE)


#Autocorrelation Function Plots
three <- storeItem1 %>%
pull(sales)%>%
forecast::ggAcf(., lag.max = 30)

four <- storeItem2 %>%
  pull(sales)%>%
  forecast::ggAcf(., lag.max = 30)

five <- storeItem1 %>%
pull(sales)%>%
forecast::ggAcf(., lag.max=2*365)

six <- storeItem2 %>%
  pull(sales)%>%
  forecast::ggAcf(., lag.max=2*365)

#create a 2x3 panel plot using patchwork
final_plot <- (one + three + five) / 
               (two + four + six) 


#save final plot
ggsave("2x3_panel_plot.png", final_plot, width = 12, height = 8)

# Display the final plot
print(final_plot)

11/18/24
#modeling time series data

#choose one store and one item
storeItem <- item_demand_train %>%
  filter(store==9, item==27)

#store recipe
store_recipe <- recipe(sales ~., storeItem) %>%
  step_date(date, features="dow") %>% #since we saw weekly trends
  step_date(date, features="month") %>%
  #step_date(date, features="doy") %>%
  step_date(date, features="decimal") %>%
  step_mutate(date_decimal= as.numeric(date_decimal))%>%
  step_mutate_at(date_dow, fn=factor)%>%
  step_mutate_at(date_month, fn=factor)%>%
  #step_mutate_at(date_doy, fn=factor)%>% 
  # step_range(date_doy, min=0, max=pi) %>%
  # step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>% 
  #step_holiday(role = "predictor") %>% 
  step_rm(c(store, item))

prepped_recipe <- prep(store_recipe)
baked = bake(prepped_recipe, new_data = storeItem)

my_time_mod <- rand_forest(mtry = tune (),
                           min_n = tune(),
                           trees = 500) %>%
  set_engine('ranger') %>%
  set_mode('regression')

wf <- workflow() %>%
  add_recipe(store_recipe) %>%
  add_model(my_time_mod) 
 

tuningGridForest <- grid_regular(min_n(),
                                 mtry(range = c(1,10)), levels = 2)

folds <- vfold_cv(storeItem, v = 2, repeats = 2)

CV_results <- wf %>%
  tune_grid(resample = folds,
            grid = tuningGridForest,
            metrics= metric_set(smape),
            control = control_grid(verbose = TRUE))

bestTune <- CV_results %>%
  show_best(n=1, metric = 'smape')
print(bestTune)
#print(CV_results)

# collect_metrics(CV_results) %>%
#   filter(bestTune) %>%
#   pull(mean)
# 
# finalwf <- wf %>%
#   finalize_workflow(bestTune) %>%
#   fit(data = item_demand_train)
# #find cross validation error 18.1  

#sarima 11/20/24 
storeItemTrain1 <- item_demand_train %>%
  filter(store==7, item==27)

storeItemTrain2 <- item_demand_train %>%
  filter(store==9, item==48)

storeItemTest1 <- item_demand_test %>%
  filter(store==7, item==27)

storeItemTest2 <- item_demand_test %>%
  filter(store==9, item==48)

## Create the CV split for time series
cv_split1 <- time_series_split(storeItemTrain1, assess="3 months", cumulative = TRUE)
cv_split2 <- time_series_split(storeItemTrain2, assess="3 months", cumulative = TRUE)

## Create a recipe for the linear model part
arima_recipe1 <- recipe(sales ~ ., data = training(cv_split1)) %>%
  step_date(date, features="dow") %>% #since we saw weekly trends
  step_date(date, features="month") %>%
  step_mutate_at(date_dow, fn=factor)%>%
  step_mutate_at(date_month, fn=factor)%>%
  step_date(date, features="decimal") %>%
  step_mutate(date_decimal= as.numeric(date_decimal))%>%
  step_zv(all_numeric_predictors()) %>% # Removes zero-variance columns
  step_normalize(all_numeric_predictors()) 

arima_recipe2 <- recipe(sales ~ ., data = training(cv_split2)) %>%
  step_date(date, features="dow") %>% #since we saw weekly trends
  step_date(date, features="month") %>%
  step_mutate_at(date_dow, fn=factor)%>%
  step_mutate_at(date_month, fn=factor)%>%
  step_date(date, features="decimal") %>%
  step_mutate(date_decimal= as.numeric(date_decimal))%>%
  step_zv(all_numeric_predictors()) %>% # Removes zero-variance columns
  step_normalize(all_numeric_predictors()) 
  
## Define the ARIMA Model
arima_model <- arima_reg( seasonal_period = 365,  # seasonality
                          non_seasonal_ar = 5, ## default max p to tune
                          non_seasonal_ma = 5, # default max q to tune
                          non_seasonal_differences = 2, # default max P to tune
                          seasonal_ar = 2, #default max Q to tune
                          seasonal_ma = 2, # default max d to tune
                          seasonal_differences = 2 #default max D to tune
) %>%
  set_engine("auto_arima")
## Merge into a single workflow and fit to the training data
arima_wf1 <- workflow() %>%
  add_recipe(arima_recipe1) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split1))

arima_wf2 <- workflow() %>%
  add_recipe(arima_recipe2) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split2))

## Calibrate (tune) the models (find p,d,q,P,D,Q)
cv_results1 <- modeltime_calibrate(arima_wf1,
                                  new_data = testing(cv_split1))

cv_results2 <- modeltime_calibrate(arima_wf2,
                                   new_data = testing(cv_split2))


## Visualize results
cv_plot1 <- cv_results1 %>% 
  modeltime_forecast(
  new_data = testing(cv_split1),
  actual_data = training(cv_split1)
) %>%
plot_modeltime_forecast(.interactive=FALSE)

cv_plot2 <- cv_results2 %>% 
  modeltime_forecast(
  new_data = testing(cv_split2),
  actual_data = training(cv_split2)
) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## Now that you have calibrated (tuned) refit to whole dataset
# ## Predict for all the observations in storeItemTest
## Now that you have calibrated (tuned) refit to whole dataset19
fullfit1 <- cv_results1 %>%
modeltime_refit(data=storeItemTrain1)
forecast1 <- fullfit1 %>%
  modeltime_forecast(
    new_data = storeItemTest1,
    actual_data = storeItemTrain1
    ) %>%
  plot_modeltime_forecast(.interactive=FALSE)

fullfit2 <- cv_results2 %>%
  modeltime_refit(data=storeItemTrain2)
forecast2 <- cv_results2 %>%
  modeltime_forecast(
    new_data = storeItemTest2,
    actual_data = storeItemTrain2
    ) %>%
  plot_modeltime_forecast(.interactive=FALSE)

# # top row is the cv results, bottom row is the forecast with the fullfit
cv_plot1 + cv_plot2 + forecast1 + forecast2 +
  plot_layout(ncol = 2)
