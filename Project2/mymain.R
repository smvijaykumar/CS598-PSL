options(warn=-1,message=-1)
set.seed(8742)

# Load Libraries
suppressPackageStartupMessages({
library(lubridate)
library(tidyverse)
library(dplyr)
library(tidyr)
})

# My Predict function for Walmart Time Series Data
mypredict = function() {
  if (t>1){
    train <<- train %>% add_row(new_train)
  }
  
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  
  test_current <- test %>%
    filter(Date >= start_date & Date < end_date) %>%
    select(-IsHoliday)
  
  train$Weekly_Sales[train$Weekly_Sales < 0] = 0
  #train$Weekly_Sales[train$Weekly_Sales == 0] = 1
  
  
  # not all depts need prediction
  test_depts <- unique(test_current$Dept)
  test_pred <- NULL
  
  for(dept in test_depts){
    train_dept_data <- train %>% filter(Dept == dept)
    test_dept_data <- test_current %>% filter(Dept == dept)
    
    # no need to consider stores that do not need prediction
    # or do not have training samples
    train_stores <- unique(train_dept_data$Store)
    test_stores <- unique(test_dept_data$Store)
    test_stores <- intersect(train_stores, test_stores)
    
    for(store in test_stores){
      tmp_train <- train_dept_data %>%
        filter(Store == store) %>%
        mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
        mutate(Yr = year(Date))
      tmp_test <- test_dept_data %>%
        filter(Store == store) %>%
        mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
        mutate(Yr = year(Date))
      
      tmp_train$Wk = factor(tmp_train$Wk, levels = 1:52)
      tmp_test$Wk = factor(tmp_test$Wk, levels = 1:52)
      train_model_matrix <- model.matrix(~ Yr + Wk, tmp_train)
      test_model_matrix <- model.matrix(~ Yr + Wk, tmp_test)
      
      
      
      mycoef <- lm(sqrt(tmp_train$Weekly_Sales) ~ train_model_matrix)$coef
      mycoef[is.na(mycoef)] <- 0
      tmp_pred <- mycoef[1] + test_model_matrix %*% mycoef[-1]
      tmp_pred = tmp_pred^2
      
      tmp_test <- tmp_test %>%
        mutate(Weekly_Pred = tmp_pred[,1]) %>%
        select(-Wk, -Yr)
      test_pred <- test_pred %>% bind_rows(tmp_test)
    }
  }
  return(test_pred)
}

# Comment out before submission
# Initialize global variables
# t = 1
# train <- readr::read_csv('train_ini.csv')
# test <- readr::read_csv('test.csv')
# 
# # save weighted mean absolute error WMAE
# num_folds <- 10
# wae <- rep(0, num_folds)
# 
# for (t in 1:num_folds) {
#   # *** THIS IS YOUR PREDICTION FUNCTION ***
#   test_pred <- mypredict()
#   
#   # load fold file
#   fold_file <- paste0('fold_', t, '.csv')
#   new_train <- readr::read_csv(fold_file,
#                                col_types = cols())
#   
#   # extract predictions matching up to the current fold
#   scoring_tbl <- new_train %>%
#     left_join(test_pred, by = c('Date', 'Store', 'Dept'))
#   readr::write_csv(scoring_tbl, paste0('scoring_', t, '.csv'))
#   # compute WMAE
#   actuals <- scoring_tbl$Weekly_Sales
#   preds <- scoring_tbl$Weekly_Pred
#   preds[is.na(preds)] <- 0
#   weights <- if_else(scoring_tbl$IsHoliday, 5, 1)
#   wae[t] <- sum(weights * abs(actuals - preds)) / sum(weights)
# }
# 
# print(wae)
# mean(wae)
