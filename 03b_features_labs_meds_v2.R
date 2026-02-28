# ==============================================================================
# Step 3b (revised): Feature Engineering — Labs & Medications
# Memory-efficient version: filter labevents in chunks
# ==============================================================================

library(data.table)

raw_dir <- "G:/PHD_courses_start_star_star/Hospital-readmission-prediction-from-EHR-data/data/raw"
processed_dir <- "G:/PHD_courses_start_star_star/Hospital-readmission-prediction-from-EHR-data/data/processed"

df <- fread(file.path(processed_dir, "features_step3a.csv"))
cohort_hadms <- unique(df$hadm_id)

# Read lab item descriptions to get key itemids first
d_lab <- fread(file.path(raw_dir, "d_labitems.csv.gz"))
key_labs <- d_lab[grepl("^(Creatinine|Hemoglobin|White Blood Cells|Platelet Count|Glucose|Sodium|Potassium|Albumin)$", 
                        label, ignore.case = FALSE)]
cat("Key lab items:\n")
print(key_labs[, .(itemid, label)])
key_itemids <- key_labs$itemid

# ======================================================================
# 3b.1  Read labevents in chunks (memory-safe)
# ======================================================================
cat("\nReading labevents in chunks...\n")

# Use R's connection to read in chunks
con <- gzfile(file.path(raw_dir, "labevents.csv.gz"), "r")
header <- readLines(con, n = 1)
col_names <- strsplit(header, ",")[[1]]
close(con)

cat("Columns in labevents:", paste(col_names, collapse = ", "), "\n")

# Find column indices we need
needed_cols <- c("subject_id", "hadm_id", "itemid", "charttime", "valuenum")
col_classes <- rep("NULL", length(col_names))  # skip all by default
for (col in needed_cols) {
  idx <- which(col_names == col)
  if (length(idx) > 0) col_classes[idx] <- NA  # NA = auto-detect (read this column)
}

# Read in chunks of 5 million rows
chunk_size <- 5000000
labs_filtered <- list()
skip_rows <- 1  # skip header
chunk_num <- 0

cat("Processing chunks (this may take a few minutes)...\n")

repeat {
  chunk <- tryCatch(
    fread(
      file.path(raw_dir, "labevents.csv.gz"),
      select = needed_cols,
      nrows = chunk_size,
      skip = skip_rows,
      header = FALSE,
      col.names = needed_cols
    ),
    error = function(e) NULL
  )
  
  if (is.null(chunk) || nrow(chunk) == 0) break
  
  chunk_num <- chunk_num + 1
  
  # Filter: only cohort admissions AND key lab items
  filtered <- chunk[hadm_id %in% cohort_hadms & itemid %in% key_itemids & !is.na(valuenum)]
  
  if (nrow(filtered) > 0) {
    labs_filtered[[chunk_num]] <- filtered
  }
  
  skip_rows <- skip_rows + chunk_size
  cat("  Chunk", chunk_num, "- read", nrow(chunk), "rows, kept", nrow(filtered), "\n")
  
  rm(chunk, filtered); gc(verbose = FALSE)
}

labs <- rbindlist(labs_filtered)
rm(labs_filtered); gc()
cat("\nTotal filtered lab records:", nrow(labs), "\n")

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

cat("\nLab features created:\n")
print(names(labs_wide))
cat("\nLab coverage (% non-missing):\n")
print(round(colMeans(!is.na(labs_wide[, -1])) * 100, 1))

rm(labs, labs_last); gc()

# ======================================================================
# 3b.2  Medication features (prescriptions is smaller, should fit)
# ======================================================================
cat("\nReading prescriptions (selected columns)...\n")
rx <- fread(
  file.path(raw_dir, "prescriptions.csv.gz"),
  select = c("subject_id", "hadm_id", "drug")
)
cat("  Total rows:", nrow(rx), "\n")

rx <- rx[hadm_id %in% cohort_hadms]
cat("  After cohort filter:", nrow(rx), "\n")

# (a) Unique medication count per admission
med_count <- rx[, .(n_medications = uniqueN(drug)), by = hadm_id]

# (b) Key medication class flags
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
cat("  Medication class prevalence:\n")
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
