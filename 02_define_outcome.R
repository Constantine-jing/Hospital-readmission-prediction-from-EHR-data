# ==============================================================================
# Step 2: Define 30-Day Readmission Outcome
# ==============================================================================

library(data.table)

# --- Read cohort from Step 1 ---
processed_dir <- "G:/PHD_courses_start_star_star/Hospital-readmission-prediction-from-EHR-data/data/processed"
cohort <- fread(file.path(processed_dir, "cohort_step1.csv"))

# --- 2.1 Convert datetime columns ---
cohort[, admittime := as.POSIXct(admittime)]
cohort[, dischtime := as.POSIXct(dischtime)]

# --- 2.2 Sort by patient and admission time ---
setorder(cohort, subject_id, admittime)

# --- 2.3 For each admission, find the NEXT admission for the same patient ---
cohort[, next_admittime := shift(admittime, type = "lead"), by = subject_id]

# --- 2.4 Calculate days to next admission ---
cohort[, days_to_next := as.numeric(difftime(next_admittime, dischtime, units = "days"))]

# --- 2.5 Define 30-day readmission (binary outcome) ---
cohort[, readmit_30d := as.integer(!is.na(days_to_next) & days_to_next >= 0 & days_to_next <= 30)]

# --- 2.6 Remove last admission per patient (no follow-up possible) ---
# Actually, we keep them — they just have readmit_30d = 0 if no next admission
# This is the standard approach

# --- 2.7 Summary ---
cat("========== READMISSION OUTCOME ==========\n")
cat("Total admissions:", nrow(cohort), "\n")
print(table(cohort$readmit_30d))
cat("\nReadmission rate:", round(mean(cohort$readmit_30d) * 100, 1), "%\n")

cat("\nDays to next admission (among readmitted):\n")
print(summary(cohort[readmit_30d == 1, days_to_next]))

cat("\nDays to next admission (among all with a next admission):\n")
print(summary(cohort[!is.na(days_to_next) & days_to_next >= 0, days_to_next]))

# --- 2.8 Check class balance ---
cat("\nClass balance:\n")
cat("  No readmission (0):", sum(cohort$readmit_30d == 0), 
    "(", round(mean(cohort$readmit_30d == 0) * 100, 1), "%)\n")
cat("  Readmission (1):   ", sum(cohort$readmit_30d == 1), 
    "(", round(mean(cohort$readmit_30d == 1) * 100, 1), "%)\n")

# --- Save ---
fwrite(cohort, file.path(processed_dir, "cohort_step2.csv"))
cat("\nCohort with outcome saved to cohort_step2.csv\n")
