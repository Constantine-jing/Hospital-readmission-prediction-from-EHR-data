# ==============================================================================
# Step 3b: Feature Engineering — Labs & Medications
# ==============================================================================

library(data.table)

raw_dir <- "C:/Users/Mengyan/Desktop/Hospital-readmission-prediction-from-EHR-data/data/raw"
processed_dir <- "C:/Users/Mengyan/Desktop/Hospital-readmission-prediction-from-EHR-data/data/processed"

df <- fread(file.path(processed_dir, "features_step3a.csv"))
cohort_ids <- unique(df$subject_id)
cohort_hadms <- unique(df$hadm_id)

# ======================================================================
# 3b.1  Lab features
# ======================================================================
cat("Reading labevents (this may take a minute)...\n")
labs_raw <- fread(file.path(raw_dir, "labevents.csv.gz"))
cat("  Total lab rows:", nrow(labs_raw), "\n")

# Filter to cohort patients only
labs <- labs_raw[subject_id %in% cohort_ids & hadm_id %in% cohort_hadms]
rm(labs_raw); gc()
cat("  After filtering to cohort:", nrow(labs), "\n")

# Read lab item descriptions
d_lab <- fread(file.path(raw_dir, "d_labitems.csv.gz"))

# Key labs commonly used in readmission studies (by itemid)
# We identify them by label first, then use itemid for speed
cat("\nFinding key lab itemids...\n")
key_labs <- d_lab[grepl("^(Creatinine|Hemoglobin|White Blood Cells|Platelet Count|Glucose|Sodium|Potassium|Albumin)$", 
                        label, ignore.case = FALSE)]
print(key_labs[, .(itemid, label)])

key_itemids <- key_labs$itemid
labs_key <- labs[itemid %in% key_itemids & !is.na(valuenum)]

# Merge lab labels
labs_key <- merge(labs_key, key_labs[, .(itemid, label)], by = "itemid")

cat("\nKey lab records:", nrow(labs_key), "\n")

# For each admission, get the LAST value of each key lab
labs_key[, charttime := as.POSIXct(charttime)]
setorder(labs_key, hadm_id, label, charttime)
labs_last <- labs_key[, .(last_value = last(valuenum)), by = .(hadm_id, label)]

# Pivot wide: one column per lab
labs_wide <- dcast(labs_last, hadm_id ~ label, value.var = "last_value")

# Clean column names
old_names <- names(labs_wide)[-1]
new_names <- paste0("lab_", tolower(gsub(" ", "_", old_names)))
setnames(labs_wide, old_names, new_names)

cat("\nLab features created:\n")
print(names(labs_wide))
cat("\nLab coverage (% non-missing per feature):\n")
print(round(colMeans(!is.na(labs_wide[, -1])) * 100, 1))

# ======================================================================
# 3b.2  Medication features
# ======================================================================
cat("\nReading prescriptions...\n")
rx_raw <- fread(file.path(raw_dir, "prescriptions.csv.gz"))
cat("  Total prescription rows:", nrow(rx_raw), "\n")

# Filter to cohort
rx <- rx_raw[subject_id %in% cohort_ids & hadm_id %in% cohort_hadms]
rm(rx_raw); gc()
cat("  After filtering to cohort:", nrow(rx), "\n")

# (a) Total number of unique medications per admission
med_count <- rx[, .(n_medications = uniqueN(drug)), by = hadm_id]

# (b) Flags for key medication classes (by drug name pattern)
rx[, `:=`(
  on_anticoagulant  = as.integer(grepl("warfarin|heparin|enoxaparin|apixaban|rivaroxaban", drug, ignore.case = TRUE)),
  on_antiepileptic  = as.integer(grepl("levetiracetam|valproic|phenytoin|carbamazepine|lamotrigine|lacosamide|topiramate", drug, ignore.case = TRUE)),
  on_antidepressant = as.integer(grepl("sertraline|fluoxetine|escitalopram|citalopram|venlafaxine|duloxetine|mirtazapine|trazodone", drug, ignore.case = TRUE)),
  on_opioid         = as.integer(grepl("morphine|oxycodone|hydromorphone|fentanyl|tramadol|hydrocodone", drug, ignore.case = TRUE)),
  on_insulin        = as.integer(grepl("insulin", drug, ignore.case = TRUE)),
  on_statin         = as.integer(grepl("atorvastatin|rosuvastatin|simvastatin|pravastatin|lovastatin", drug, ignore.case = TRUE))
)]

med_flags <- rx[, .(
  on_anticoagulant  = max(on_anticoagulant),
  on_antiepileptic  = max(on_antiepileptic),
  on_antidepressant = max(on_antidepressant),
  on_opioid         = max(on_opioid),
  on_insulin        = max(on_insulin),
  on_statin         = max(on_statin)
), by = hadm_id]

cat("\nMedication features:\n")
cat("  Median unique meds per admission:", median(med_count$n_medications), "\n")
cat("  Medication class prevalence:\n")
print(med_flags[, lapply(.SD, mean), .SDcols = patterns("on_")])

# ======================================================================
# 3b.3  Merge everything
# ======================================================================
df2 <- merge(df, labs_wide, by = "hadm_id", all.x = TRUE)
df2 <- merge(df2, med_count, by = "hadm_id", all.x = TRUE)
df2 <- merge(df2, med_flags, by = "hadm_id", all.x = TRUE)

# Patients with no prescriptions → 0 medications
setnafill(df2, fill = 0, cols = c("n_medications", 
                                   "on_anticoagulant", "on_antiepileptic", 
                                   "on_antidepressant", "on_opioid", 
                                   "on_insulin", "on_statin"))

cat("\n========== FINAL FEATURE SET ==========\n")
cat("Rows:", nrow(df2), "\n")
cat("Columns:", ncol(df2), "\n")
cat("\nMissing values:\n")
print(colSums(is.na(df2)))

# ======================================================================
# 3b.4  Save
# ======================================================================
fwrite(df2, file.path(processed_dir, "features_step3_full.csv"))
cat("\nSaved to features_step3_full.csv\n")
