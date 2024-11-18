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