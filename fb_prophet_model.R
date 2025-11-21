library(readr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(tidyverse)
library(tidymodels)
library(prophet)

train_df <- read_csv("C:/Users/Jenna/OneDrive/Desktop/Statistics/Stat 348/WalmartForecasting/data/train.csv")
test_df <- read_csv("C:/Users/Jenna/OneDrive/Desktop/Statistics/Stat 348/WalmartForecasting/data/test.csv")
stores <- read_csv("C:/Users/Jenna/OneDrive/Desktop/Statistics/Stat 348/WalmartForecasting/data/stores.csv")
features <- read_csv("C:/Users/Jenna/OneDrive/Desktop/Statistics/Stat 348/WalmartForecasting/data/features.csv")

### Impute Missing Markdowns
features <- features %>%
  mutate(across(starts_with("MarkDown"), ~ replace_na(., 0))) %>%
  mutate(across(starts_with("MarkDown"), ~ pmax(., 0))) %>%
  mutate(
    MarkDown_Total = rowSums(across(starts_with("MarkDown")), na.rm = TRUE),
    MarkDown_Flag = if_else(MarkDown_Total > 0, 1, 0),
    MarkDown_Log   = log1p(MarkDown_Total)
  ) %>%
  select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5) %>%
  select(-IsHoliday)

# Define the specific dates as Date objects beforehand
super_bowl_dates <- as.Date(c("12-Feb-10", "11-Feb-11", "10-Feb-12", "08-Feb-13"), format = "%d-%b-%y")
labor_day_dates <- as.Date(c("10-Sep-10", "09-Sep-11", "07-Sep-12", "06-Sep-13"), format = "%d-%b-%y")
thanksgiving_dates <- as.Date(c("26-Nov-10", "25-Nov-11", "23-Nov-12", "29-Nov-13"), format = "%d-%b-%y")
christmas_dates <- as.Date(c("31-Dec-10", "30-Dec-11", "28-Dec-12", "27-Dec-13"), format = "%d-%b-%y")

## Impute Missing CPI and Unemployment
feature_recipe <- recipe(~., data=features) %>%
  step_mutate(DecDate = decimal_date(Date)) %>% #random forests helps with this
  step_impute_bag(CPI, Unemployment,
                  impute_with = imp_vars(DecDate, Store))%>%
  step_mutate(IsSuper = ifelse(Date %in% super_bowl_dates, 1, 0)) %>%
  step_mutate(IsLabor = ifelse(Date %in% labor_day_dates, 1, 0)) %>%
  step_mutate(IsThank = ifelse(Date %in% thanksgiving_dates, 1, 0)) %>%
  step_mutate(IsChrist = ifelse(Date %in% christmas_dates, 1, 0)) %>%
  step_rm(IsHoliday) %>% # take out store and dept later
  
imputed_features <- juice(prep(feature_recipe)) # juice is same as bad but uses data i flagged in recipe

# Now perform the left join using the modified data frame
joined_train <- left_join(train_df, imputed_features, by = c("Store", "Date"))
joined_Test <- left_join(test_df, imputed_features, by = c("Store", "Date"))

# train_clean <- joined_train %>%
#   filter(Store == 1, Dept == 1)

# # Define the specific dates as Date objects beforehand
# super_bowl_dates <- as.Date(c("12-Feb-10", "11-Feb-11", "10-Feb-12", "08-Feb-13"), format = "%d-%b-%y")
# labor_day_dates <- as.Date(c("10-Sep-10", "09-Sep-11", "07-Sep-12", "06-Sep-13"), format = "%d-%b-%y")
# thanksgiving_dates <- as.Date(c("26-Nov-10", "25-Nov-11", "23-Nov-12", "29-Nov-13"), format = "%d-%b-%y")
# christmas_dates <- as.Date(c("31-Dec-10", "30-Dec-11", "28-Dec-12", "27-Dec-13"), format = "%d-%b-%y")
# step_mutate(IsSuper = ifelse(Date %in% super_bowl_dates, 1, 0)) %>%
#   step_mutate(IsLabor = ifelse(Date %in% labor_day_dates, 1, 0)) %>%
#   step_mutate(IsThank = ifelse(Date %in% thanksgiving_dates, 1, 0)) %>%
#   step_mutate(IsChrist = ifelse(Date %in% christmas_dates, 1, 0)) %>%

store <- 28
dept <-  28

sd_train <- joined_train %>%
  filter(Store == store, Dept == dept) %>%
  rename(y=Weekly_Sales, ds=Date)
sd_test <- joined_Test %>%
  filter(Store==store, Dept == dept) %>%
  rename(ds=Date)

prophet_model <- prophet() %>%
  add_regressor("CPI") %>%
  add_regressor("Unemployment") %>%
  add_regressor("MarkDown_Total") %>%
  add_regressor("Fuel_Price") %>%
  add_regressor("IsSuper") %>%
  add_regressor("IsLabor") %>%
  add_regressor("IsChrist") %>%
  fit.prophet(df=sd_train)

## Predict Using Fitted prophet Model1
fitted_vals <- predict(prophet_model, df=sd_train) #For Plotting Fitted Values2
test_preds <- predict(prophet_model, df=sd_test) #Predictions are called "yhat"3

## Plot Fitted and Forecast on Same Plot5
ggplot() +
geom_line(data = sd_train, mapping = aes(x = ds, y = y, color = "Data")) +
geom_line(data = fitted_vals, mapping = aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
geom_line(data = test_preds, mapping = aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
scale_color_manual(values = c("Data" = "black", "Fitted" = "blue", "Forecast" = "red")) +
labs(color="")
