# feature engineer for if month = super bowl, labor day, tahnks giving , christmas month and holiday = true 
#then label as such holiday bc those will be weighted
# filter to only get same id's in test and train bc some are missing 

library(readr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(tidyverse)
library(tidymodels)

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

## Impute Missing CPI and Unemployment
feature_recipe <- recipe(~., data=features) %>%
  step_mutate(DecDate = decimal_date(Date)) %>% #random forests helps with this
  step_impute_bag(CPI, Unemployment,
                  impute_with = imp_vars(DecDate, Store))
imputed_features <- juice(prep(feature_recipe)) # juice is same as bad but uses data i flagged in recipe

# Now perform the left join using the modified data frame
train_joined <- left_join(train_df, imputed_features, by = c("Store", "Date"))
test <- left_join(test_df, imputed_features, by = c("Store", "Date"))

train_clean <- train_joined %>%
  filter(Store == 1, Dept == 1)

# Define the specific dates as Date objects beforehand
super_bowl_dates <- as.Date(c("12-Feb-10", "11-Feb-11", "10-Feb-12", "08-Feb-13"), format = "%d-%b-%y")
labor_day_dates <- as.Date(c("10-Sep-10", "09-Sep-11", "07-Sep-12", "06-Sep-13"), format = "%d-%b-%y")
thanksgiving_dates <- as.Date(c("26-Nov-10", "25-Nov-11", "23-Nov-12", "29-Nov-13"), format = "%d-%b-%y")
christmas_dates <- as.Date(c("31-Dec-10", "30-Dec-11", "28-Dec-12", "27-Dec-13"), format = "%d-%b-%y")

# The corrected recipe
recipe <- recipe(Weekly_Sales ~ ., data = train_clean) %>%
  step_mutate(IsSuper = ifelse(Date %in% super_bowl_dates, 1, 0)) %>%
  step_mutate(IsLabor = ifelse(Date %in% labor_day_dates, 1, 0)) %>%
  step_mutate(IsThank = ifelse(Date %in% thanksgiving_dates, 1, 0)) %>%
  step_mutate(IsChrist = ifelse(Date %in% christmas_dates, 1, 0)) %>%
  step_date(Date, features = "week") %>%
  step_date(Date, features = "quarter") %>%
  step_range(Date_week, min = 0, max = pi) %>%
  step_mutate(sinWEEK = sin(Date_week), cosWEEK = cos(Date_week)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_rm(Date, IsHoliday, Store, Dept) %>% # take out store and dept later
  step_normalize(all_numeric_predictors())
# step_zv(all_predictors()) # Remove zero-variance predictors

# prepared_recipe <- prep(recipe, training = train_clean)
# processed_train <- bake(prepared_recipe, new_data = train_clean)

preg_model <- rand_forest(mtry = tune(),
                          min_n = tune(),
                          trees=500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

preg_wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(preg_model)

prepped <- prep(recipe)
# num_predictors <- ncol(bake(prepped, new_data = NULL)) - 1

grid_of_tuning_params <- grid_regular(mtry(range = c(1,6)),
                                      min_n(),
                                      levels = 5)

folds <- vfold_cv(train_clean, v = 5, repeats = 1)

CV_results <- preg_wf %>%
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics = metric_set(rmse)) #change to mae

CV_results %>% show_best(n=1, metric = "rmse")

sd(train_clean$Weekly_Sales)
