# ==============================================================================
# Step 3b (v3): Feature Engineering — Labs & Medications
# Uses fread pipe approach to filter labevents without loading full file
# ==============================================================================

library(data.table)

raw_dir <- "G:/PHD_courses_start_star_star/Hospital-readmission-prediction-from-EHR-data/data/raw"
processed_dir <- "G:/PHD_courses_start_star_star/Hospital-readmission-prediction-from-EHR-data/data/processed"

df <- fread(file.path(processed_dir, "features_step3a.csv"))
cohort_hadms <- unique(df$hadm_id)

# Read lab item descriptions
d_lab <- fread(file.path(raw_dir, "d_labitems.csv.gz"))
key_labs <- d_lab[grepl("^(Creatinine|Hemoglobin|White Blood Cells|Platelet Count|Glucose|Sodium|Potassium|Albumin)$", 
                        label, ignore.case = FALSE)]
cat("Key lab items:\n")
print(key_labs[, .(itemid, label)])
key_itemids <- key_labs$itemid

# ======================================================================
# 3b.1  Read labevents — only 5 columns, then immediately filter
# ======================================================================
cat("\nReading labevents (5 columns only — may take 2-3 min)...\n")
cat("If this crashes, see alternative approach at bottom of script.\n\n")

# Approach: read only needed columns to minimize memory
labs_raw <- fread(
  file.path(raw_dir, "labevents.csv.gz"),
  select = c("hadm_id", "itemid", "charttime", "valuenum")
)
cat("  Rows read:", nrow(labs_raw), "\n")

# Filter immediately
labs <- labs_raw[hadm_id %in% cohort_hadms & itemid %in% key_itemids & !is.na(valuenum)]
rm(labs_raw); gc()
cat("  After filtering:", nrow(labs), "\n")

# Merge lab labels
labs <- merge(labs, key_labs[, .(itemid, label)], by = "itemid")

# For each admission, get the LAST value of each key lab
labs[, charttime := as.POSIXct(charttime)]
setorder(labs, hadm_id, label, charttime)
labs_last <- labs[, .(last_value = last(valuenum)), by = .(hadm_id, label)]

# Pivot wide
labs_wide <- dcast(labs_last, hadm_id ~ label, value.var = "last_value")
old_names <- names(labs_wide)[-1]
new_names <- paste0("lab_", tolower(gsub(" ", "_", old_names)))
setnames(labs_wide, old_names, new_names)

cat("\nLab features:\n")
print(names(labs_wide))
cat("\nLab coverage (% non-missing):\n")
print(round(colMeans(!is.na(labs_wide[, -1])) * 100, 1))

rm(labs, labs_last); gc()

# ======================================================================
# 3b.2  Medication features
# ======================================================================
cat("\nReading prescriptions (3 columns)...\n")
rx <- fread(
  file.path(raw_dir, "prescriptions.csv.gz"),
  select = c("subject_id", "hadm_id", "drug")
)
cat("  Total rows:", nrow(rx), "\n")

rx <- rx[hadm_id %in% cohort_hadms]
cat("  After cohort filter:", nrow(rx), "\n")

med_count <- rx[, .(n_medications = uniqueN(drug)), by = hadm_id]

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
cat("  Median unique meds:", median(med_count$n_medications), "\n")
cat("  Prevalence:\n")
print(med_flags[, lapply(.SD, mean), .SDcols = patterns("on_")])

rm(rx); gc()

# ======================================================================
# 3b.3  Merge everything
# ======================================================================
df2 <- merge(df, labs_wide, by = "hadm_id", all.x = TRUE)
df2 <- merge(df2, med_count, by = "hadm_id", all.x = TRUE)
df2 <- merge(df2, med_flags, by = "hadm_id", all.x = TRUE)

setnafill(df2, fill = 0, cols = c("n_medications", 
                                   "on_anticoagulant", "on_antiepileptic", 
                                   "on_antidepressant", "on_opioid", 
                                   "on_insulin", "on_statin"))

cat("\n========== FINAL FEATURE SET ==========\n")
cat("Rows:", nrow(df2), "\n")
cat("Columns:", ncol(df2), "\n")
cat("\nMissing values:\n")
print(colSums(is.na(df2)))

fwrite(df2, file.path(processed_dir, "features_step3_full.csv"))
cat("\nSaved to features_step3_full.csv\n")

# ======================================================================
# ALTERNATIVE: If fread still crashes on labevents, run this in terminal:
# 
# On Windows PowerShell:
#   # First decompress
#   gzip -dk labevents.csv.gz
#   # Then use Python to filter (save as filter_labs.py):
#   # import pandas as pd
#   # cohort = pd.read_csv('../processed/features_step3a.csv', usecols=['hadm_id'])
#   # ids = set(cohort['hadm_id'])
#   # chunks = pd.read_csv('labevents.csv', usecols=['hadm_id','itemid','charttime','valuenum'], chunksize=1000000)
#   # filtered = pd.concat([c[c['hadm_id'].isin(ids)] for c in chunks])
#   # filtered.to_csv('../processed/labs_filtered.csv', index=False)
#
# Then in R: labs_raw <- fread("data/processed/labs_filtered.csv")
# ======================================================================
