# ==============================================================================
# Step 1: Read MIMIC-IV Data & Build Neurological Cohort
# ==============================================================================

library(data.table)

# --- Set paths ---
raw_dir <- "C:/Users/Mengyan/Desktop/Hospital-readmission-prediction-from-EHR-data/data/raw"

# --- 1.1 Read core tables ---
cat("Reading admissions...\n")
admissions <- fread(file.path(raw_dir, "admissions.csv.gz"))
cat("  Rows:", nrow(admissions), "\n")

cat("Reading patients...\n")
patients <- fread(file.path(raw_dir, "patients.csv.gz"))
cat("  Rows:", nrow(patients), "\n")

cat("Reading diagnoses_icd...\n")
diagnoses <- fread(file.path(raw_dir, "diagnoses_icd.csv.gz"))
cat("  Rows:", nrow(diagnoses), "\n")

cat("Reading ICD code descriptions...\n")
d_icd <- fread(file.path(raw_dir, "d_icd_diagnoses.csv.gz"))

# --- 1.2 Explore the data briefly ---
cat("\n--- admissions columns ---\n")
print(names(admissions))

cat("\n--- patients columns ---\n")
print(names(patients))

cat("\n--- diagnoses columns ---\n")
print(names(diagnoses))

cat("\n--- Sample ICD codes ---\n")
print(head(diagnoses, 10))

# --- 1.3 Identify neurological patients ---
# ICD-10 Chapter VI: Diseases of the Nervous System = G00-G99
# icd_version == 10 ensures we only use ICD-10 codes
neuro_dx <- diagnoses[icd_version == 10 & grepl("^G", icd_code)]
cat("\nNeurological diagnosis records (ICD-10 G00-G99):", nrow(neuro_dx), "\n")
cat("Unique patients with neuro dx:", uniqueN(neuro_dx$subject_id), "\n")
cat("Unique admissions with neuro dx:", uniqueN(neuro_dx$hadm_id), "\n")

# --- 1.4 Build base cohort ---
# Keep admissions that have at least one neurological diagnosis
neuro_hadm_ids <- unique(neuro_dx$hadm_id)

cohort <- admissions[hadm_id %in% neuro_hadm_ids]

# Merge with patient demographics
cohort <- merge(cohort, patients, by = "subject_id", all.x = TRUE)

cat("\nCohort before filters:", nrow(cohort), "admissions\n")

# --- 1.5 Apply inclusion/exclusion criteria ---
# (a) Adults only (age >= 18)
#     MIMIC-IV: anchor_age is age at anchor_year; we approximate age at admission
cohort[, admit_year := year(as.IDate(admittime))]
cohort[, approx_age := anchor_age + (admit_year - anchor_year)]
cohort <- cohort[approx_age >= 18]
cat("After age >= 18:", nrow(cohort), "\n")

# (b) Remove in-hospital deaths (can't have readmission if died)
cohort <- cohort[hospital_expire_flag == 0]
cat("After removing in-hospital deaths:", nrow(cohort), "\n")

# (c) Remove newborns / elective admissions if needed
#     Keep emergency, urgent, and observation admissions
cat("\nAdmission types:\n")
print(table(cohort$admission_type))

# --- 1.6 Summary ---
cat("\n========== COHORT SUMMARY ==========\n")
cat("Total admissions:", nrow(cohort), "\n")
cat("Unique patients:", uniqueN(cohort$subject_id), "\n")
cat("Age range:", range(cohort$approx_age), "\n")
cat("Gender:\n")
print(table(cohort$gender))
cat("\nTop 10 neurological diagnoses in cohort:\n")
top_dx <- merge(neuro_dx[hadm_id %in% cohort$hadm_id], d_icd, 
                by.x = c("icd_code", "icd_version"), 
                by.y = c("icd_code", "icd_version"), all.x = TRUE)
print(head(top_dx[, .N, by = .(icd_code, long_title)][order(-N)], 10))

# --- Save cohort for Step 2 ---
out_dir <- "C:/Users/Mengyan/Desktop/Hospital-readmission-prediction-from-EHR-data/data/processed"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
fwrite(cohort, file.path(out_dir, "cohort_step1.csv"))
cat("\nCohort saved to", file.path(out_dir, "cohort_step1.csv"), "\n")
