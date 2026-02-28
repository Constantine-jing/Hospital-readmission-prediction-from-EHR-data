# ==============================================================================
# Step 5: Model Comparison — Elastic Net, Random Forest, XGBoost
# Stratified CV with AUC as primary metric
# ==============================================================================

library(data.table)
library(glmnet)       # Elastic Net
library(ranger)       # Random Forest (faster than randomForest)
library(xgboost)      # XGBoost
library(pROC)         # AUC
library(caret)        # createFolds for stratified CV
# install.packages(c("glmnet", "ranger", "xgboost", "pROC", "caret"))

processed_dir <- "G:/PHD_courses_start_star_star/Hospital-readmission-prediction-from-EHR-data/data/processed"
model_df <- fread(file.path(processed_dir, "model_ready_step4.csv"))

# ======================================================================
# 5.1  Prepare X and y
# ======================================================================
model_features <- c(
  "approx_age", "female", "married", "public_insurance",
  "race_Black", "race_Asian", "race_Hispanic", "race_Other",
  "los_days", "admit_hour", "weekend_admit", "emergency_admit",
  "n_diagnoses", "n_neuro_dx", "comorbidity_count",
  "has_diabetes", "has_hypertension", "has_heart_failure",
  "has_ckd", "has_copd", "has_depression",
  "has_epilepsy", "has_sleep_disorder", "has_parkinsons",
  "prior_admits_12m",
  "lab_albumin", "lab_creatinine", "lab_glucose", "lab_hemoglobin",
  "lab_platelet_count", "lab_potassium", "lab_sodium", "lab_white_blood_cells",
  "no_labs_flag",
  "n_medications", "on_anticoagulant", "on_antiepileptic",
  "on_antidepressant", "on_opioid", "on_insulin", "on_statin"
)

X <- as.matrix(model_df[, ..model_features])
y <- model_df$readmit_30d

cat("X:", nrow(X), "rows x", ncol(X), "features\n")
cat("y: 0 =", sum(y == 0), ", 1 =", sum(y == 1), "\n\n")

# ======================================================================
# 5.2  Stratified 5-fold CV
# ======================================================================
set.seed(42)
folds <- createFolds(y, k = 5, list = TRUE, returnTrain = FALSE)

# Storage for predictions
pred_enet <- pred_rf <- pred_xgb <- numeric(length(y))

cat("========== 5-FOLD STRATIFIED CV ==========\n\n")

for (i in seq_along(folds)) {
  test_idx <- folds[[i]]
  train_idx <- setdiff(seq_along(y), test_idx)
  
  X_train <- X[train_idx, ]
  y_train <- y[train_idx]
  X_test  <- X[test_idx, ]
  y_test  <- y[test_idx]
  
  cat("Fold", i, "- Train:", length(train_idx), "Test:", length(test_idx), "\n")
  
  # --- Elastic Net ---
  cv_enet <- cv.glmnet(X_train, y_train, family = "binomial", 
                        alpha = 0.5, nfolds = 5, type.measure = "auc")
  pred_enet[test_idx] <- predict(cv_enet, X_test, s = "lambda.1se", type = "response")[, 1]
  cat("  Elastic Net done (lambda.1se)\n")
  
  # --- Random Forest ---
  rf_data <- data.frame(X_train, y = as.factor(y_train))
  rf_model <- ranger(y ~ ., data = rf_data, num.trees = 500, probability = TRUE,
                     seed = 42, num.threads = 4)
  pred_rf[test_idx] <- predict(rf_model, data = data.frame(X_test))$predictions[, "1"]
  cat("  Random Forest done (500 trees)\n")
  
  # --- XGBoost ---
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest  <- xgb.DMatrix(data = X_test, label = y_test)
  
  # Calculate scale_pos_weight for class imbalance
  spw <- sum(y_train == 0) / sum(y_train == 1)
  
  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    scale_pos_weight = spw
  )
  
  xgb_model <- xgb.train(params, dtrain, nrounds = 300, 
                          watchlist = list(test = dtest),
                          early_stopping_rounds = 30, verbose = 0)
  pred_xgb[test_idx] <- predict(xgb_model, dtest)
  cat("  XGBoost done (", xgb_model$best_iteration, "rounds)\n\n")
}

# ======================================================================
# 5.3  Evaluate: AUC
# ======================================================================
auc_enet <- auc(roc(y, pred_enet, quiet = TRUE))
auc_rf   <- auc(roc(y, pred_rf, quiet = TRUE))
auc_xgb  <- auc(roc(y, pred_xgb, quiet = TRUE))

cat("\n========== OOF AUC RESULTS ==========\n")
cat("Elastic Net:    ", round(auc_enet, 4), "\n")
cat("Random Forest:  ", round(auc_rf, 4), "\n")
cat("XGBoost:        ", round(auc_xgb, 4), "\n")

# ======================================================================
# 5.4  Threshold tuning on best model
# ======================================================================
# Find optimal threshold that maximizes Youden's J (sensitivity + specificity - 1)
best_model_name <- c("Elastic Net", "Random Forest", "XGBoost")[which.max(c(auc_enet, auc_rf, auc_xgb))]
best_pred <- list(pred_enet, pred_rf, pred_xgb)[[which.max(c(auc_enet, auc_rf, auc_xgb))]]

roc_obj <- roc(y, best_pred, quiet = TRUE)
coords_best <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity", "ppv", "npv"))

cat("\n========== BEST MODEL:", best_model_name, "==========\n")
cat("Optimal threshold:", round(coords_best$threshold, 3), "\n")
cat("Sensitivity:      ", round(coords_best$sensitivity, 3), "\n")
cat("Specificity:      ", round(coords_best$specificity, 3), "\n")
cat("PPV:              ", round(coords_best$ppv, 3), "\n")
cat("NPV:              ", round(coords_best$npv, 3), "\n")

# ======================================================================
# 5.5  Per-fold AUC for confidence interval
# ======================================================================
cat("\n========== PER-FOLD AUC ==========\n")
for (i in seq_along(folds)) {
  idx <- folds[[i]]
  cat("Fold", i, "- EN:", round(auc(roc(y[idx], pred_enet[idx], quiet = TRUE)), 4),
      " RF:", round(auc(roc(y[idx], pred_rf[idx], quiet = TRUE)), 4),
      " XGB:", round(auc(roc(y[idx], pred_xgb[idx], quiet = TRUE)), 4), "\n")
}

# ======================================================================
# 5.6  Save predictions and models for Step 6 (SHAP)
# ======================================================================
results <- data.table(
  subject_id = model_df$subject_id,
  hadm_id    = model_df$hadm_id,
  y_true     = y,
  pred_enet  = pred_enet,
  pred_rf    = pred_rf,
  pred_xgb   = pred_xgb
)
fwrite(results, file.path(processed_dir, "cv_predictions_step5.csv"))

# Retrain best XGBoost on full data for SHAP (Step 6)
cat("\nRetraining XGBoost on full data for SHAP analysis...\n")
dtrain_full <- xgb.DMatrix(data = X, label = y)
spw_full <- sum(y == 0) / sum(y == 1)

params_full <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  scale_pos_weight = spw_full
)

# Use best_iteration from last fold as guide

best_nrounds <- 200
xgb_full <- xgb.train(params_full, dtrain_full, nrounds = best_nrounds, verbose = 0)
xgb.save(xgb_full, file.path(processed_dir, "xgb_full_model.xgb"))
cat("Full XGBoost model saved.\n")
cat("\nStep 5 complete. Ready for Step 6: SHAP interpretation.\n")
