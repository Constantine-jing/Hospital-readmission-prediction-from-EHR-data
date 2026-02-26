# ==============================================================================
# Step 3a: Feature Engineering — Demographics & Diagnoses
# (does NOT require labevents or prescriptions)
# ==============================================================================

library(data.table)

raw_dir <- "C:/Users/Mengyan/Desktop/Hospital-readmission-prediction-from-EHR-data/data/raw"
processed_dir <- "C:/Users/Mengyan/Desktop/Hospital-readmission-prediction-from-EHR-data/data/processed"

cohort <- fread(file.path(processed_dir, "cohort_step2.csv"))
diagnoses <- fread(file.path(raw_dir, "diagnoses_icd.csv.gz"))

# ======================================================================
# 3a.1  Demographic features (already in cohort)
# ======================================================================
cohort[, `:=`(
  female       = as.integer(gender == "F"),
  los_days     = as.numeric(difftime(as.POSIXct(dischtime), as.POSIXct(admittime), units = "days")),
  admit_hour   = hour(as.POSIXct(admittime)),
  weekend_admit = as.integer(wday(as.IDate(admittime)) %in% c(1, 7))
)]

# Insurance as binary: Medicare/Medicaid vs Other
cohort[, public_insurance := as.integer(insurance %in% c("Medicare", "Medicaid"))]

# Marital status: simplify
cohort[, married := as.integer(marital_status == "MARRIED")]

# Race: simplify into broad categories
cohort[, race_group := fcase(
  grepl("WHITE", race),   "White",
  grepl("BLACK", race),   "Black",
  grepl("ASIAN", race),   "Asian",
  grepl("HISPANIC|LATINO", race), "Hispanic",
  default = "Other"
)]

cat("--- Demographic features ---\n")
cat("Age:       ", summary(cohort$approx_age), "\n")
cat("Female:    ", mean(cohort$female), "\n")
cat("LOS (days):", round(mean(cohort$los_days), 1), "\n")
cat("Insurance:\n"); print(table(cohort$public_insurance))
cat("Race:\n"); print(table(cohort$race_group))

# ======================================================================
# 3a.2  Diagnosis-based features
# ======================================================================
# Only ICD-10 diagnoses for admissions in our cohort
dx <- diagnoses[icd_version == 10 & hadm_id %in% cohort$hadm_id]

# (a) Number of diagnoses per admission
dx_count <- dx[, .(n_diagnoses = .N), by = hadm_id]

# (b) Number of neurological diagnoses (G codes)
neuro_count <- dx[grepl("^G", icd_code), .(n_neuro_dx = .N), by = hadm_id]

# (c) Key comorbidity flags (common in readmission literature)
dx[, `:=`(
  has_diabetes      = as.integer(grepl("^E1[0-4]", icd_code)),
  has_hypertension  = as.integer(grepl("^I1[0-6]", icd_code)),
  has_heart_failure = as.integer(grepl("^I50", icd_code)),
  has_ckd           = as.integer(grepl("^N18", icd_code)),
  has_copd          = as.integer(grepl("^J4[4-7]", icd_code)),
  has_depression    = as.integer(grepl("^F3[2-3]", icd_code)),
  has_epilepsy      = as.integer(grepl("^G40", icd_code)),
  has_sleep_disorder = as.integer(grepl("^G47", icd_code)),
  has_parkinsons    = as.integer(grepl("^G20", icd_code))
)]

# Aggregate to admission level (1 if any diagnosis in that category)
comorbidity <- dx[, .(
  has_diabetes       = max(has_diabetes),
  has_hypertension   = max(has_hypertension),
  has_heart_failure  = max(has_heart_failure),
  has_ckd            = max(has_ckd),
  has_copd           = max(has_copd),
  has_depression     = max(has_depression),
  has_epilepsy       = max(has_epilepsy),
  has_sleep_disorder = max(has_sleep_disorder),
  has_parkinsons     = max(has_parkinsons)
), by = hadm_id]

# (d) Charlson-style comorbidity count (simple version: count of above flags)
comorbidity[, comorbidity_count := has_diabetes + has_hypertension + has_heart_failure + 
              has_ckd + has_copd + has_depression]

cat("\n--- Comorbidity prevalence ---\n")
print(comorbidity[, lapply(.SD, mean), .SDcols = patterns("has_")])

# ======================================================================
# 3a.3  Prior utilization features
# ======================================================================
# Number of admissions in past 12 months for each admission
cohort[, admittime_posix := as.POSIXct(admittime)]
setorder(cohort, subject_id, admittime_posix)

# For each row, count prior admissions within 365 days
cohort[, prior_admits_12m := {
  n <- .N
  counts <- integer(n)
  at <- admittime_posix
  for(i in seq_len(n)) {
    counts[i] <- sum(at[1:(i)] >= at[i] - 365*24*3600 & at[1:(i)] < at[i])
  }
  counts
}, by = subject_id]

cat("\n--- Prior admissions in 12 months ---\n")
print(table(cohort$prior_admits_12m))

# ======================================================================
# 3a.4  Merge all features
# ======================================================================
features <- merge(cohort, dx_count, by = "hadm_id", all.x = TRUE)
features <- merge(features, neuro_count, by = "hadm_id", all.x = TRUE)
features <- merge(features, comorbidity, by = "hadm_id", all.x = TRUE)

# Fill NAs (admissions with no neuro dx count = 0, etc.)
setnafill(features, fill = 0, cols = c("n_neuro_dx"))

cat("\n--- Final feature set (3a) ---\n")
cat("Rows:", nrow(features), "\n")
cat("Columns:", ncol(features), "\n")

# ======================================================================
# 3a.5  Select analysis columns and save
# ======================================================================
analysis_cols <- c(
  # IDs
  "subject_id", "hadm_id",
  # Outcome
  "readmit_30d",
  # Demographics
  "approx_age", "female", "married", "public_insurance", "race_group",
  # Admission
  "los_days", "admit_hour", "weekend_admit", "admission_type",
  # Diagnoses
  "n_diagnoses", "n_neuro_dx", "comorbidity_count",
  "has_diabetes", "has_hypertension", "has_heart_failure",
  "has_ckd", "has_copd", "has_depression",
  "has_epilepsy", "has_sleep_disorder", "has_parkinsons",
  # Prior utilization
  "prior_admits_12m"
)

df <- features[, ..analysis_cols]
cat("\nAnalysis dataframe:", nrow(df), "rows x", ncol(df), "columns\n")
cat("\nMissing values:\n")
print(colSums(is.na(df)))

fwrite(df, file.path(processed_dir, "features_step3a.csv"))
cat("\nSaved to features_step3a.csv\n")
cat("Waiting for labevents and prescriptions to add more features in Step 3b.\n")
