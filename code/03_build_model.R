##########################################################
# 03_build_model.R
#
# Purpose: Train candidate predictive models.
#
# This script:
#     1. Creates a test/train split
#     2. Builds logistic regression baseline model
#     3. Trains several XGBoost and Random Forest models
#
# Inputs:
#   model_data
#
#
# Outputs:
#   model_xgb_v1
#   model_xgb_v2
#   model_xgb_v3
#   rf_model_v1
#   rf_model_v2
#   test (with prediction columns)
#
##########################################################

library(tidyverse)
library(xgboost)
library(ranger)
library(pROC)

##########################################################
# Create train/test split
##########################################################

train <- model_data |> 
  filter(
    season <= 2020
  )

test <- model_data |> 
  filter(
    season > 2020
  )

##########################################################
# Baseline logistic regression
##########################################################

model_glm <- glm(breakout ~ ppg + onIce_corsiPercentage + I_F_goals + I_F_points + I_F_xGoals + I_F_highDangerxGoals + shotpctg + offensivezoneshift_pctg + icetime + previous_best_points, data = train, family = "binomial")


test$pred_prob_glm <- predict(
  model_glm,
  newdata = test,
  type = "response"
)

##########################################################
# XGBoost Models
##########################################################

# Create a helper function to create the training/testing datasets and matrices for the XGBoost Models.
prepare_xgb_data <- function(features){
  train_df <- train |>
    select(breakout, all_of(features))
  test_df <- test |>
    select(breakout, all_of(features))
  list(
    train_matrix = data.matrix(train_df[,features]),
    test_matrix = data.matrix(test_df[,features]),
    train_y = train_df$breakout,
    test_y = test_df$breakout
  )
}

# Prepare features lists for XGBoost Models
features_v1 <- c(
  "ppg",
  "onIce_corsiPercentage",
  "I_F_goals",
  "I_F_xGoals",
  "I_F_highDangerxGoals",
  "shotpctg",
  "offensivezoneshift_pctg",
  "icetime",
  "previous_best_points"
)
features_v2 <- c(
  "ppg",
  "onIce_corsiPercentage",
  "I_F_goals",
  "I_F_xGoals",
  "I_F_highDangerxGoals",
  "shotpctg",
  "offensivezoneshift_pctg",
  "icetime",
  "previous_best_points",
  "experience",
  "change_points",
  "change_icetime",
  "change_xg",
  "change_shots",
  "change_corsi"
)
features_v3 <- c(
  "ppg",
  "onIce_corsiPercentage",
  "I_F_goals",
  "I_F_xGoals",
  "I_F_highDangerxGoals",
  "shotpctg",
  "offensivezoneshift_pctg",
  "icetime",
  "previous_best_points",
  "experience",
  "change_points",
  "change_icetime",
  "change_xg",
  "change_shots",
  "change_corsi",
  "points_growth",
  "xg_growth",
  "icetime_growth",
  "points_2yr",
  "xg_2yr",
  "icetime_2yr", 
  "ppg_change",
  "ppg_growth"
)

xgb_v1_data <- prepare_xgb_data(features_v1)
xgb_v2_data <- prepare_xgb_data(features_v2)
xgb_v3_data <- prepare_xgb_data(features_v3)

##########################################################
# XGBoost Version 1 Model
##########################################################

model_xgb_v1 <- xgboost(
  x = xgb_v1_data$train_matrix,
  y = xgb_v1_data$train_y,
  objective = "binary:logistic",
  eval_metric = "auc",
  nrounds = 200,
  max_depth = 4,
  learning_rate = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8
)
test$pred_prob_xgb_v1 <- predict(model_xgb_v1, xgb_v1_data$test_matrix)

##########################################################
# XGBoost Version 2 Model
##########################################################


model_xgb_v2 <- xgboost(
  x = xgb_v2_data$train_matrix,
  y = xgb_v2_data$train_y,
  objective = "binary:logistic",
  eval_metric = "auc",
  nrounds = 300,
  max_depth = 4,
  learning_rate = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8
)

test$pred_prob_xgb_v2 <- predict(model_xgb_v2, xgb_v2_data$test_matrix)

##########################################################
# XGBoost Version 3 Model
##########################################################

model_xgb_v3 <- xgboost(
  x = xgb_v3_data$train_matrix,
  y = xgb_v3_data$train_y,
  objective = "binary:logistic",
  eval_metric = "auc",
  nrounds = 300,
  max_depth = 4,
  learning_rate = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8
)
test$pred_prob_xgb_v3 <- predict(model_xgb_v3, xgb_v3_data$test_matrix)

##########################################################
# Random Forest Models
##########################################################

# Create a helper function for the rf models
prepare_rf_data <- function(features){
  train_df <- train |>
    select(
      breakout,
      all_of(features)
    )
  test_df <- test |>
    select(
      breakout,
      all_of(features)
    )
  train_df$breakout <- as.factor(train_df$breakout)
  test_df$breakout <- as.factor(test_df$breakout)
  list(
    train_df = train_df,
    test_df = test_df
  )
}

# Features list for random forest
features_rf <- c(
  "ppg",
  "onIce_corsiPercentage",
  "I_F_goals",
  "I_F_xGoals",
  "I_F_highDangerxGoals",
  "shotpctg",
  "offensivezoneshift_pctg",
  "icetime",
  "previous_best_points",
  "experience",
  "change_points",
  "change_icetime",
  "change_xg",
  "change_shots",
  "change_corsi"
)
rf_v1 <- prepare_rf_data(features_rf)
rf_v2 <- prepare_rf_data(features_v3) # Using the same features as XGBoost v3

##########################################################
# Random Forest Version One Model
##########################################################

rf_v1_model <- ranger(
  breakout ~ .,
  data = rf_v1$train_df,
  probability = TRUE,
  num.trees = 500,
  importance = "impurity"
)

rf_v1_pred <- predict(rf_v1_model, data = rf_v1$test_df)
test$pred_prob_rf_v1 <- rf_v1_pred$predictions[,2]

##########################################################
# Random Forest Version Two Model
##########################################################
rf_v2_model <- ranger(
  breakout ~ .,
  data = rf_v2$train_df,
  probability = TRUE,
  num.trees = 500,
  importance = "impurity"
)
rf_v2_pred <- predict(rf_v2_model, data = rf_v2$test_df)
test$pred_prob_rf_v2 <- rf_v2_pred$predictions[,2]

##########################################################
# End of Script
##########################################################