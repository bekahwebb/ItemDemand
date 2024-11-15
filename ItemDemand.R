library(tidyverse)
library(vroom)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(dplyr)
library(recipes)
library(tidymodels)
library(forecast)

item_demand_test <- vroom('test.csv') # Rows: 45000 Columns: 4  
item_demand_train <- vroom('train.csv')# Rows: 913000 Columns: 4 
#item_demand_train

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



