# ==============================================================================
# Step 6: SHAP Interpretation
# ==============================================================================

library(data.table)
library(xgboost)
library(SHAPforxgboost)  # install.packages("SHAPforxgboost")
library(ggplot2)

processed_dir <- "G:/PHD_courses_start_star_star/Hospital-readmission-prediction-from-EHR-data/data/processed"
plot_dir <- "G:/PHD_courses_start_star_star/Hospital-readmission-prediction-from-EHR-data/plots"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# ======================================================================
# 6.1  Load model and data
# ======================================================================
model_df <- fread(file.path(processed_dir, "model_ready_step4.csv"))

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
xgb_full <- xgb.load(file.path(processed_dir, "xgb_full_model.xgb"))

# ======================================================================
# 6.2  Compute SHAP values (use sample for speed if needed)
# ======================================================================
cat("Computing SHAP values...\n")

# Use a random sample of 5000 for SHAP (full dataset is slow)
set.seed(42)
sample_idx <- sample(nrow(X), min(5000, nrow(X)))
X_sample <- X[sample_idx, ]

shap_values <- shap.values(xgb_model = xgb_full, X_train = X_sample)
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = X_sample)

cat("SHAP computation complete.\n")

# ======================================================================
# 6.3  Global feature importance (mean |SHAP|)
# ======================================================================
cat("\n========== TOP 15 FEATURES BY MEAN |SHAP| ==========\n")
importance <- shap_values$mean_shap_score
print(head(importance, 15))

# ======================================================================
# 6.4  SHAP Summary Plot (beeswarm)
# ======================================================================
p1 <- shap.plot.summary(shap_long)
ggsave(file.path(plot_dir, "shap_summary.png"), p1, width = 10, height = 8, dpi = 150)
cat("\nSHAP summary plot saved.\n")

# ======================================================================
# 6.5  SHAP Dependence Plots (top features)
# ======================================================================
top_features <- names(head(importance, 6))

for (feat in top_features) {
  p <- shap.plot.dependence(
    shap_long, x = feat, 
    color_feature = "auto",
    smooth = TRUE, size0 = 0.5
  )
  fname <- paste0("shap_dep_", gsub("[^a-zA-Z0-9]", "_", feat), ".png")
  ggsave(file.path(plot_dir, fname), p, width = 8, height = 5, dpi = 150)
}
cat("SHAP dependence plots saved for top 6 features.\n")

# ======================================================================
# 6.6  Individual patient explanation (example)
# ======================================================================
# Pick a high-risk patient and a low-risk patient
preds_sample <- predict(xgb_full, X_sample)
high_risk_idx <- which.max(preds_sample)
low_risk_idx  <- which.min(preds_sample)

cat("\n========== EXAMPLE PATIENTS ==========\n")
cat("High-risk patient (pred =", round(preds_sample[high_risk_idx], 3), "):\n")
cat("  Top SHAP drivers:\n")
high_shap <- shap_values$shap_score[high_risk_idx, ]
high_shap_sorted <- sort(unlist(high_shap), decreasing = TRUE)
print(head(high_shap_sorted, 5))

cat("\nLow-risk patient (pred =", round(preds_sample[low_risk_idx], 3), "):\n")
cat("  Top SHAP drivers (most negative):\n")
low_shap <- shap_values$shap_score[low_risk_idx, ]
low_shap_sorted <- sort(unlist(low_shap))
print(head(low_shap_sorted, 5))

# ======================================================================
# 6.7  Save SHAP data for Shiny dashboard (Step 7)
# ======================================================================
shap_for_shiny <- data.table(
  idx = sample_idx,
  subject_id = model_df$subject_id[sample_idx],
  hadm_id = model_df$hadm_id[sample_idx],
  pred = preds_sample,
  y_true = model_df$readmit_30d[sample_idx]
)
# Add SHAP values for each feature
shap_matrix <- as.data.table(shap_values$shap_score)
setnames(shap_matrix, paste0("shap_", names(shap_matrix)))
shap_for_shiny <- cbind(shap_for_shiny, shap_matrix)

# Add original feature values
features_sample <- as.data.table(X_sample)
shap_for_shiny <- cbind(shap_for_shiny, features_sample)

fwrite(shap_for_shiny, file.path(processed_dir, "shap_data_step6.csv"))
cat("\nSHAP data for Shiny saved.\n")

cat("\nStep 6 complete. Ready for Step 7: Shiny dashboard.\n")
