##########################################################
# 04_model_selection.R
#
# Purpose: Hyperparameterize, improve, and create final models for the project.
#
# This script:
#     1. Analyzes the AUC and Precision at K of the existing Models
#     2. Improves the models using hyperparameterization
#     3. Trains final two XGBoost Precision and XGBoost Coverage Models
#
# Inputs:
#   model_data
#   
#
# Outputs:
#   model_xgb_v4_precision
#   model_xgb_v4_coverage
#   test (including the two models predictions and a combined score)
#
##########################################################

library(tidyverse)
library(xgboost)
library(ranger)
library(pROC)

##########################################################
# Quick Evaluation of The Models
##########################################################

glmsum <- summary(model_glm)
glm_auc <- roc(test$breakout,test$pred_prob_glm)

roc_v1 <- roc(xgb_v1_data$test_y, test$pred_prob_xgb_v1)
imp_v1 <- xgb.importance(model = model_xgb_v1)


roc_v2 <- roc(xgb_v2_data$test_y, test$pred_prob_xgb_v2) # The best performing model by AUC; used for first hyperparameterization
imp_v2 <- xgb.importance(model = model_xgb_v2)

roc_v3 <- roc(xgb_v3_data$test_y, predictor = test$pred_prob_xgb_v3)
imp_v3 <- xgb.importance(model = model_xgb_v3)

roc_rf_v1 <- roc(test$breakout, test$pred_prob_rf_v1)

roc_rf_v2 <- roc(test$breakout, test$pred_prob_rf_v2)


##########################################################
# Hyperparameterization of XGBoost V2
##########################################################

# Helper function that produces Top-k precision
top_k <- function(predictions, truth, k){
  data.frame(
    breakout = truth,
    pred = predictions
  ) |>
    arrange(desc(pred)) |>
    slice_head(n = k) |>
    summarize(correct = sum(breakout)) |>
    pull(correct)
}
grid <- expand.grid(
  max_depth = c(3,4,5),
  learning_rate = c(0.03,0.05,0.1),
  nrounds = c(200,300,500)
)
# Initialize data-frame for results 
results <- data.frame()
for(i in 1:nrow(grid)) {
  model <- xgboost(
    x = xgb_v2_data$train_matrix,
    y = xgb_v2_data$train_y,
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = grid$max_depth[i],
    learning_rate = grid$learning_rate[i],
    nrounds = grid$nrounds[i],
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  pred <- predict(model, xgb_v2_data$test_matrix)
  auc <- roc(xgb_v2_data$test_y, pred)$auc
  results <- rbind(
    results,
    data.frame(
      max_depth = grid$max_depth[i],
      learning_rate = grid$learning_rate[i],
      nrounds = grid$nrounds[i],
      auc = as.numeric(auc)
    )
  )
}

# The best two models from the results data-frame by AUC are run independently for their P@K.
model1 <- xgboost(
  x = xgb_v2_data$train_matrix,
  y = xgb_v2_data$train_y,
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 3, # A parameter of interest
  learning_rate = 0.03, # A parameter of interest
  nrounds = 200, # A parameter of interest
  subsample = 0.8,
  colsample_bytree = 0.8
)

model2 <- xgboost(
  x = xgb_v2_data$train_matrix,
  y = xgb_v2_data$train_y,
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 4, # A parameter of interest
  learning_rate = 0.03, # A parameter of interest
  nrounds = 200, # A parameter of interest
  subsample = 0.8,
  colsample_bytree = 0.8
)
pred1 <- predict(model1, xgb_v2_data$test_matrix)
pred2 <- predict(model2, xgb_v2_data$test_matrix)

precision_results <- data.frame(model1 = c(top_k(pred1, xgb_v2_data$test_y, 10), # Model 1 is better than Model 2 here, with P@10 of 6. 
                                           top_k(pred1, xgb_v2_data$test_y, 30),
                                           top_k(pred1, xgb_v2_data$test_y, 50)),
                                model2 = c(top_k(pred2, xgb_v2_data$test_y, 10),
                                           top_k(pred2, xgb_v2_data$test_y, 30),
                                           top_k(pred2, xgb_v2_data$test_y, 50)))

##########################################################
# Building Final XGBoost v4 Precision Model
##########################################################

model_xgb_v4_precision <- xgboost(
  x = xgb_v2_data$train_matrix,
  y = xgb_v2_data$train_y,
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 3,
  learning_rate = 0.03,
  nrounds = 200,
  subsample = 0.8,
  colsample_bytree = 0.8
)

test$pred_prob_xgb_v4_precision <- predict(model_xgb_v4_precision, xgb_v2_data$test_matrix)
pred_prec <- predict(model_xgb_v4_precision, xgb_v2_data$test_matrix)

imp_v4 <- xgb.importance(model = model_xgb_v4_precision)
auc_v4 <- roc(xgb_v2_data$test_y, test$pred_prob_xgb_v4_precision)

##########################################################
# Hyperparamterization of XGBoost v3
##########################################################

# Same process as v2
grid <- expand.grid(
  max_depth = c(3,4,5),
  learning_rate = c(0.03,0.05,0.1),
  nrounds = c(200,300,500)
)

results <- data.frame()
for(i in 1:nrow(grid)) {
  model <- xgboost(
    x = xgb_v3_data$train_matrix,
    y = xgb_v3_data$train_y,
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = grid$max_depth[i],
    learning_rate = grid$learning_rate[i],
    nrounds = grid$nrounds[i],
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  pred <- predict(model, xgb_v3_data$test_matrix)
  auc <- roc(xgb_v3_data$test_y, pred)$auc
  results <- rbind(
    results,
    data.frame(
      max_depth = grid$max_depth[i],
      learning_rate = grid$learning_rate[i],
      nrounds = grid$nrounds[i],
      auc = as.numeric(auc)
    )
  )
}

model1 <- xgboost(
  x = xgb_v3_data$train_matrix,
  y = xgb_v3_data$train_y,
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 3, # A parameter of interest
  learning_rate = 0.03, # A parameter of interest
  nrounds = 300, # A parameter of interest
  subsample = 0.8,
  colsample_bytree = 0.8
)

model2 <- xgboost(
  x = xgb_v3_data$train_matrix,
  y = xgb_v3_data$train_y,
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 3, # A parameter of interest
  learning_rate = 0.03, # A parameter of interest
  nrounds = 200, # A parameter of interest
  subsample = 0.8,
  colsample_bytree = 0.8
)
pred1 <- predict(model1, xgb_v3_data$test_matrix)
pred2 <- predict(model2, xgb_v3_data$test_matrix)

coverage_results <- data.frame(model1 = c(top_k(pred1, xgb_v3_data$test_y, 10), 
                                           top_k(pred1, xgb_v3_data$test_y, 30),
                                           top_k(pred1, xgb_v3_data$test_y, 50)),
                                model2 = c(top_k(pred2, xgb_v3_data$test_y, 10),
                                           top_k(pred2, xgb_v3_data$test_y, 30),
                                           top_k(pred2, xgb_v3_data$test_y, 50))) # Models highly comparable, so model 1 was chosen with the higher AUC.

##########################################################
# Building Final XGBoost v4 Coverage Model
##########################################################
model_xgb_v4_coverage <- xgboost(
  x = xgb_v3_data$train_matrix,
  y = xgb_v3_data$train_y,
  objective = "binary:logistic",
  eval_metric = "auc",
  nrounds = 300,
  max_depth = 3,
  learning_rate = 0.03,
  subsample = 0.8,
  colsample_bytree = 0.8
)
test$pred_prob_xgb_v4_coverage <- predict(model_xgb_v4_coverage, xgb_v3_data$test_matrix)
pred_cov <- predict(model_xgb_v4_coverage, xgb_v3_data$test_matrix)
auc_v4_cov <- roc(xgb_v3_data$test_y, test$pred_prob_xgb_v4_coverage)

##########################################################
# Results Dataset with P@K of the Two Models
##########################################################

topk_results <- data.frame(precision = c(top_k(pred_prec, xgb_v2_data$test_y, 10), 
                                      top_k(pred_prec, xgb_v2_data$test_y, 30),
                                      top_k(pred_prec, xgb_v2_data$test_y, 50)),
                           coverage = c(top_k(pred_cov, xgb_v3_data$test_y, 10),
                                      top_k(pred_cov, xgb_v3_data$test_y, 30),
                                      top_k(pred_cov, xgb_v3_data$test_y, 50)))

##########################################################
# Adjusting the Test Dataset
##########################################################

# Updating Testing Dataset to Only Include the Two Final Models
test$combined_score <- (.6 * test$pred_prob_xgb_v4_precision) + (.4 * test$pred_prob_xgb_v4_coverage)
test <- test |> 
  select(
    breakout, season, pred_prob_xgb_v4_precision, pred_prob_xgb_v4_coverage, combined_score, everything(), 
    -pred_prob_glm, -pred_prob_xgb_v1, -pred_prob_xgb_v2, -pred_prob_xgb_v3, -pred_prob_rf_v1, -pred_prob_rf_v2
  )
##########################################################
# End of Script
##########################################################