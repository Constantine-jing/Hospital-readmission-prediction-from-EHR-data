"""
generate_synthetic_cohort.py
Generate a realistic synthetic MIMIC-IV neurological cohort for pipeline testing.

Clinical distributions are calibrated to published MIMIC-IV summary statistics
and neurological readmission literature. This is NOT real patient data.

Output: data/raw/cohort.csv (matches the schema of 01_cohort_extraction.sql)
"""

import numpy as np
import pandas as pd
from datetime import datetime, timedelta

np.random.seed(2025)

N = 2400  # Target cohort size (realistic for neuro subset of MIMIC-IV)

print(f"Generating {N} synthetic neurological admissions...")

# =============================================================================
# 1. DEMOGRAPHICS
# =============================================================================

subject_id = np.arange(100001, 100001 + N)

# Age: skewed older for neurological patients (mean ~67, SD ~15)
age_at_admit = np.clip(
    np.random.normal(67, 15, N).astype(int), 18, 99
)

# Gender: ~53% male in MIMIC-IV neuro population
gender = np.random.choice(["M", "F"], N, p=[0.53, 0.47])

# Race distribution (approximate MIMIC-IV)
race = np.random.choice(
    ["WHITE", "BLACK/AFRICAN AMERICAN", "HISPANIC/LATINO",
     "ASIAN", "OTHER", "UNKNOWN"],
    N, p=[0.58, 0.16, 0.08, 0.05, 0.08, 0.05]
)

# Insurance
insurance = np.random.choice(
    ["Medicare", "Medicaid", "Other"],
    N, p=[0.55, 0.18, 0.27]
)

# Marital status
marital_status = np.random.choice(
    ["MARRIED", "SINGLE", "DIVORCED", "WIDOWED", "UNKNOWN"],
    N, p=[0.38, 0.25, 0.10, 0.17, 0.10]
)

# =============================================================================
# 2. ADMISSION CHARACTERISTICS
# =============================================================================

# Length of stay: log-normal (median ~5d, heavy right tail)
los_days = np.clip(np.random.lognormal(1.6, 0.7, N), 1.0, 60.0).round(1)

# Admission type (all non-elective per inclusion criteria)
admission_type = np.random.choice(
    ["URGENT", "EMERGENCY", "OBSERVATION"],
    N, p=[0.35, 0.55, 0.10]
)

# Generate hadm_id
hadm_id = np.arange(200001, 200001 + N)

# =============================================================================
# 3. UTILIZATION FEATURES
# =============================================================================

# Prior admissions in past 12 months: zero-inflated
prior_admits_12m = np.where(
    np.random.random(N) < 0.45,  # 45% have zero prior admits
    0,
    np.clip(np.random.poisson(1.8, N), 1, 8)
)

# ICU stay: ~25% of neurological admissions
had_icu_stay = np.random.binomial(1, 0.25, N)
icu_los_days = np.where(
    had_icu_stay == 1,
    np.clip(np.random.lognormal(0.8, 0.6, N), 0.5, 30.0).round(1),
    0.0
)

# =============================================================================
# 4. DIAGNOSIS FEATURES (ICD-10 flags)
# =============================================================================

# Number of diagnoses: Poisson-like
num_diagnoses = np.clip(np.random.poisson(5.5, N), 1, 20)

# Neurological diagnoses (not mutually exclusive)
dx_cerebral_infarction = np.random.binomial(1, 0.22, N)  # I63
dx_ich                 = np.random.binomial(1, 0.09, N)  # I61
dx_epilepsy            = np.random.binomial(1, 0.18, N)  # G40
dx_parkinson           = np.random.binomial(1, 0.08, N)  # G20
dx_alzheimer           = np.random.binomial(1, 0.07, N)  # G30
dx_ms                  = np.random.binomial(1, 0.04, N)  # G35

# Comorbidity flags
dx_diabetes = np.random.binomial(1, 0.28, N)
dx_chf      = np.random.binomial(1, 0.18, N)
dx_copd     = np.random.binomial(1, 0.14, N)
dx_ckd      = np.random.binomial(1, 0.16, N)

# =============================================================================
# 5. LABORATORY VALUES (last before discharge)
# =============================================================================
# Clinical ranges calibrated to MIMIC-IV published distributions

# Creatinine: higher in CKD patients
creatinine_base = np.random.lognormal(0.1, 0.35, N)
creatinine = np.where(dx_ckd == 1, creatinine_base * 1.8, creatinine_base)
creatinine = np.clip(creatinine, 0.3, 8.0).round(2)

# Hemoglobin: lower in females, CKD
hgb_base = np.where(gender == "F",
                     np.random.normal(12.2, 1.8, N),
                     np.random.normal(13.8, 2.0, N))
hemoglobin = np.where(dx_ckd == 1, hgb_base - 1.5, hgb_base)
hemoglobin = np.clip(hemoglobin, 5.0, 19.0).round(1)

# WBC
wbc = np.clip(np.random.lognormal(2.1, 0.35, N), 1.5, 35.0).round(1)

# Glucose: higher in diabetics
glucose_base = np.random.lognormal(4.8, 0.25, N)
glucose = np.where(dx_diabetes == 1, glucose_base * 1.3, glucose_base)
glucose = np.clip(glucose, 50, 500).round(0).astype(int)

# Sodium
sodium = np.clip(np.random.normal(139, 3.5, N), 118, 158).round(0).astype(int)

# Potassium
potassium = np.clip(np.random.normal(4.1, 0.5, N), 2.5, 6.5).round(1)

# BUN: correlated with creatinine
bun = np.clip(creatinine * np.random.normal(12, 3, N) + 5, 5, 80).round(0).astype(int)

# Platelet
platelet = np.clip(np.random.normal(220, 75, N), 30, 600).round(0).astype(int)

# =============================================================================
# 6. MEDICATION FEATURES
# =============================================================================

# Number of medications
num_medications = np.clip(np.random.poisson(8, N), 1, 25)

# Medication flags
on_anticoagulant = np.random.binomial(1, 0.32, N)
on_statin        = np.random.binomial(1, 0.38, N)
on_insulin       = np.where(dx_diabetes == 1,
                            np.random.binomial(1, 0.55, N),
                            np.random.binomial(1, 0.03, N))
on_antiepileptic = np.where(dx_epilepsy == 1,
                            np.random.binomial(1, 0.85, N),
                            np.random.binomial(1, 0.12, N))

# =============================================================================
# 7. GENERATE READMISSION OUTCOME (realistic logistic model)
# =============================================================================
# Based on published risk factors for 30-day neurological readmission

print("Generating readmission outcome via logistic model...")

# Standardize continuous features for logistic model
def z(x):
    return (x - np.mean(x)) / np.std(x)

logit = (
    -1.10                              # intercept (targets ~27% readmission rate)
    + 0.15  * z(age_at_admit)          # older → higher risk
    + 0.45  * z(prior_admits_12m)      # strongest predictor
    + 0.08  * z(los_days)             # longer stay → higher risk
    + 0.30  * z(creatinine)           # renal dysfunction
    - 0.18  * z(hemoglobin)           # anemia → higher risk (negative direction)
    + 0.12  * z(wbc)                  # inflammation marker
    + 0.08  * z(glucose)              # metabolic instability
    - 0.06  * z(sodium)               # hyponatremia risk
    + 0.10  * z(num_diagnoses)        # complexity
    + 0.25  * dx_cerebral_infarction  # stroke risk
    + 0.35  * dx_ich                  # hemorrhagic stroke — highest risk
    + 0.15  * dx_chf                  # heart failure comorbidity
    + 0.20  * dx_ckd                  # renal comorbidity
    + 0.10  * dx_diabetes             # metabolic comorbidity
    + 0.12  * dx_copd                 # pulmonary comorbidity
    + 0.18  * had_icu_stay            # severity marker
    + 0.05  * z(num_medications)      # polypharmacy
    # Interaction effects
    + 0.08  * z(prior_admits_12m) * dx_ckd           # repeat admits + CKD
    + 0.06  * z(age_at_admit) * z(creatinine)        # age × renal
    + np.random.normal(0, 0.3, N)                     # noise
)

prob_readmit = 1 / (1 + np.exp(-logit))
readmit_30d = np.random.binomial(1, prob_readmit, N)

readmit_rate = readmit_30d.mean() * 100
print(f"  Readmission rate: {readmit_rate:.1f}% ({readmit_30d.sum()}/{N})")

# =============================================================================
# 8. INTRODUCE REALISTIC MISSING DATA PATTERNS
# =============================================================================
# MIMIC-IV lab missingness is ~3-15% depending on test

print("Introducing missing data patterns...")

def add_missing(arr, pct, name):
    """Introduce MCAR/MAR missingness."""
    n_miss = int(len(arr) * pct / 100)
    idx = np.random.choice(len(arr), n_miss, replace=False)
    result = arr.astype(float).copy()
    result[idx] = np.nan
    print(f"  {name}: {n_miss} missing ({pct:.1f}%)")
    return result

creatinine_m  = add_missing(creatinine, 4.5, "creatinine")
hemoglobin_m  = add_missing(hemoglobin, 7.2, "hemoglobin")
wbc_m         = add_missing(wbc, 5.8, "wbc")
glucose_m     = add_missing(glucose.astype(float), 12.5, "glucose")
sodium_m      = add_missing(sodium.astype(float), 4.0, "sodium")
potassium_m   = add_missing(potassium, 6.1, "potassium")
bun_m         = add_missing(bun.astype(float), 8.3, "bun")
platelet_m    = add_missing(platelet.astype(float), 3.5, "platelet")

# A few missing in medications/marital
num_meds_m    = add_missing(num_medications.astype(float), 2.5, "num_medications")

# MAR pattern: glucose more likely missing if no diabetes
mar_mask = (dx_diabetes == 0) & (np.random.random(N) < 0.05)
glucose_m[mar_mask] = np.nan
print(f"  glucose (additional MAR): {mar_mask.sum()} extra missing for non-diabetics")

# =============================================================================
# 9. ASSEMBLE AND EXPORT
# =============================================================================

df = pd.DataFrame({
    "subject_id":              subject_id,
    "hadm_id":                 hadm_id,
    "readmit_30d":             readmit_30d,
    "age_at_admit":            age_at_admit,
    "gender":                  gender,
    "race":                    race,
    "insurance":               insurance,
    "marital_status":          marital_status,
    "los_days":                los_days,
    "admission_type":          admission_type,
    "prior_admits_12m":        prior_admits_12m,
    "had_icu_stay":            had_icu_stay,
    "icu_los_days":            icu_los_days,
    "num_diagnoses":           num_diagnoses,
    "dx_cerebral_infarction":  dx_cerebral_infarction,
    "dx_ich":                  dx_ich,
    "dx_epilepsy":             dx_epilepsy,
    "dx_parkinson":            dx_parkinson,
    "dx_alzheimer":            dx_alzheimer,
    "dx_ms":                   dx_ms,
    "dx_diabetes":             dx_diabetes,
    "dx_chf":                  dx_chf,
    "dx_copd":                 dx_copd,
    "dx_ckd":                  dx_ckd,
    "creatinine":              creatinine_m,
    "hemoglobin":              hemoglobin_m,
    "wbc":                     wbc_m,
    "glucose":                 glucose_m,
    "sodium":                  sodium_m,
    "potassium":               potassium_m,
    "bun":                     bun_m,
    "platelet":                platelet_m,
    "num_medications":         num_meds_m,
    "on_anticoagulant":        on_anticoagulant,
    "on_statin":               on_statin,
    "on_insulin":              on_insulin,
    "on_antiepileptic":        on_antiepileptic,
})

# Save
df.to_csv("cohort.csv", index=False)
print(f"\nExported: cohort.csv")
print(f"  Shape: {df.shape[0]} rows × {df.shape[1]} columns")
print(f"  Readmission rate: {df['readmit_30d'].mean()*100:.1f}%")
print(f"  Mean age: {df['age_at_admit'].mean():.1f}")
print(f"  Female: {(df['gender']=='F').mean()*100:.1f}%")
print(f"  Mean LOS: {df['los_days'].mean():.1f} days")
print(f"  ICU rate: {df['had_icu_stay'].mean()*100:.1f}%")

# Missing data summary
print(f"\nMissing data summary:")
miss = df.isnull().sum()
miss = miss[miss > 0]
for col, n in miss.items():
    print(f"  {col}: {n} ({n/len(df)*100:.1f}%)")

# Quick sanity checks
print(f"\nLab value ranges:")
for lab in ["creatinine", "hemoglobin", "wbc", "glucose", "sodium", "potassium", "bun", "platelet"]:
    print(f"  {lab}: {df[lab].min():.1f} – {df[lab].max():.1f} (mean {df[lab].mean():.1f})")
