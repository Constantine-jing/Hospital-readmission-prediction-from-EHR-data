# ==============================================================================
# Step 4: Missing Data Handling (mice) & Modeling Preparation
# ==============================================================================

library(data.table)
library(mice)      # install.packages("mice") if needed

processed_dir <- "G:/PHD_courses_start_star_star/Hospital-readmission-prediction-from-EHR-data/data/processed"
df <- fread(file.path(processed_dir, "features_step3_full.csv"))

# ======================================================================
# 4.1  Inspect missing data pattern
# ======================================================================
cat("========== MISSING DATA SUMMARY ==========\n")
miss_cols <- colSums(is.na(df))
miss_cols <- miss_cols[miss_cols > 0]
cat("\nColumns with missing values:\n")
print(data.frame(
  n_missing = miss_cols,
  pct_missing = round(miss_cols / nrow(df) * 100, 1)
))

# ======================================================================
# 4.2  Prepare data for imputation
# ======================================================================
# Select only analysis features (drop IDs, keep outcome separate)
id_cols <- c("subject_id", "hadm_id")
outcome <- "readmit_30d"

# Columns to impute: only the lab features have missing values
lab_cols <- grep("^lab_", names(df), value = TRUE)

# All feature columns (excluding IDs and outcome)
feature_cols <- setdiff(names(df), c(id_cols, outcome))

# Convert categorical variables to factors for mice
df[, race_group := as.factor(race_group)]
df[, admission_type := as.factor(admission_type)]

# ======================================================================
# 4.3  Create indicator for "no labs taken"
# ======================================================================
# ~50% of admissions have no lab data at all — this is informative!
# Create a flag before imputation
df[, no_labs_flag := as.integer(is.na(lab_creatinine) & is.na(lab_hemoglobin) & 
                                  is.na(lab_glucose) & is.na(lab_sodium))]
cat("\nNo labs taken flag:\n")
print(table(df$no_labs_flag))

# Add to feature list
feature_cols <- c(feature_cols, "no_labs_flag")

# ======================================================================
# 4.4  Multiple imputation with mice
# ======================================================================
# Only impute lab columns; other features have no missing values
# Use a subset of predictors for speed (mice with 40 cols + 93K rows is slow)

cat("\nRunning mice imputation (this may take several minutes)...\n")
cat("Imputing", length(lab_cols), "lab columns\n")

# Prepare imputation data: labs + key predictors that help predict lab values
imp_predictors <- c("approx_age", "female", "los_days", "comorbidity_count",
                    "has_diabetes", "has_ckd", "has_heart_failure",
                    "n_diagnoses", "readmit_30d")
imp_data <- df[, c(lab_cols, imp_predictors), with = FALSE]

# Set up mice: only impute lab columns
meth <- rep("", ncol(imp_data))
names(meth) <- names(imp_data)
meth[lab_cols] <- "pmm"  # predictive mean matching — good for continuous data

# Predictor matrix: all columns predict each other
pred <- quickpred(imp_data, minpuc = 0.1)

# Run mice with m=5 imputations, maxit=5 iterations
imp <- mice(imp_data, m = 5, maxit = 5, method = meth, predictorMatrix = pred,
            seed = 42, printFlag = TRUE)

cat("\nImputation complete.\n")

# ======================================================================
# 4.5  Extract completed dataset (use first imputation)
# ======================================================================
imp_complete <- complete(imp, action = 1)

# Replace lab columns in original df
for (col in lab_cols) {
  df[[col]] <- imp_complete[[col]]
}

# Verify no missing values remain
cat("\nMissing values after imputation:\n")
print(colSums(is.na(df[, ..feature_cols])))

# ======================================================================
# 4.6  Final modeling dataframe
# ======================================================================
# One-hot encode categorical variables
df[, `:=`(
  race_Black    = as.integer(race_group == "Black"),
  race_Asian    = as.integer(race_group == "Asian"),
  race_Hispanic = as.integer(race_group == "Hispanic"),
  race_Other    = as.integer(race_group == "Other")
  # White is reference category
)]

# Admission type: simplify to emergency vs non-emergency
df[, emergency_admit := as.integer(admission_type %in% c("EW EMER.", "DIRECT EMER.", "URGENT"))]

# Final feature list (all numeric, ready for modeling)
model_features <- c(
  # Demographics
  "approx_age", "female", "married", "public_insurance",
  "race_Black", "race_Asian", "race_Hispanic", "race_Other",
  # Admission
  "los_days", "admit_hour", "weekend_admit", "emergency_admit",
  # Diagnoses
  "n_diagnoses", "n_neuro_dx", "comorbidity_count",
  "has_diabetes", "has_hypertension", "has_heart_failure",
  "has_ckd", "has_copd", "has_depression",
  "has_epilepsy", "has_sleep_disorder", "has_parkinsons",
  # Prior utilization
  "prior_admits_12m",
  # Labs
  "lab_albumin", "lab_creatinine", "lab_glucose", "lab_hemoglobin",
  "lab_platelet_count", "lab_potassium", "lab_sodium", "lab_white_blood_cells",
  "no_labs_flag",
  # Medications
  "n_medications", "on_anticoagulant", "on_antiepileptic",
  "on_antidepressant", "on_opioid", "on_insulin", "on_statin"
)

model_df <- df[, c("subject_id", "hadm_id", "readmit_30d", model_features), with = FALSE]

cat("\n========== MODELING DATAFRAME ==========\n")
cat("Rows:", nrow(model_df), "\n")
cat("Features:", length(model_features), "\n")
cat("Outcome distribution:\n")
print(table(model_df$readmit_30d))
cat("\nMissing values:", sum(is.na(model_df[, ..model_features])), "\n")

# ======================================================================
# 4.7  Save
# ======================================================================
fwrite(model_df, file.path(processed_dir, "model_ready_step4.csv"))
cat("\nSaved to model_ready_step4.csv\n")
cat("Ready for Step 5: Modeling\n")
