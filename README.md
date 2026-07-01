# Predicting 30-Day Hospital Readmission in Neurological Patients (MIMIC-IV)

## Table of Contents
- [What Is This Project?](#what-is-this-project)
- [Why Does This Matter?](#why-does-this-matter)
- [Data Source](#data-source)
- [Variable Dictionary](#variable-dictionary)
- [Missing Data](#missing-data)
- [Pipeline Overview](#pipeline-overview)
- [Models Used](#models-used)
- [Results & Outputs](#results--outputs)
- [File Structure](#file-structure)
- [How to Run](#how-to-run)
- [R Dependencies](#r-dependencies)
- [Key Terms Glossary](#key-terms-glossary)

---

## What Is This Project?

When a patient is discharged from a hospital, sometimes they end up coming back within 30 days — this is called a **readmission**. We are building a machine learning model to predict: **"Will this patient come back to the hospital within 30 days after discharge?"**

We focus specifically on **neurological patients** — people admitted for conditions like stroke, epilepsy, Parkinson's disease, and Alzheimer's. These patients tend to have high readmission rates because their conditions are complex, chronic, and often come with multiple other health problems.

If we can identify high-risk patients **before they leave**, hospitals can provide extra follow-up care, better discharge planning, and medication counseling to prevent unnecessary readmissions.

---

## Why Does This Matter?

- **For patients:** Readmissions mean they are sick again, often sicker than before
- **For hospitals:** Medicare penalizes hospitals with high readmission rates (Hospital Readmissions Reduction Program, HRRP)
- **For the healthcare system:** Unplanned readmissions cost an estimated $26 billion per year in the US
- **For your portfolio:** This is a classic healthcare data science problem that employers in health tech, pharma, insurance, and hospital systems actively hire for

---

## Data Source

### What is MIMIC-IV?

**MIMIC-IV** stands for **Medical Information Mart for Intensive Care, version IV**. It is a freely available, de-identified clinical database from:

- **MIT** (Massachusetts Institute of Technology)
- **Beth Israel Deaconess Medical Center** (a hospital in Boston)

It contains real hospital records from over 500,000 admissions. Researchers worldwide use it to study hospital outcomes. Access requires completing CITI ethics training and a PhysioNet data use agreement.

### Is this EHR data or RWE?

**Both.**

- **EHR (Electronic Health Records):** This is the *source type* — the data comes from the hospital's electronic medical record system. Every lab result, diagnosis code, and medication order was entered by clinicians during real patient care.
- **RWE (Real-World Evidence):** This is the *purpose* — we are generating evidence from real clinical practice (as opposed to a randomized controlled trial).

EHR is one of the most common sources of RWE. Others include insurance claims, patient registries, and wearable device data.

### Current Status

- **Real MIMIC-IV data:** The pipeline now runs directly on the real MIMIC-IV tables (`admissions`, `patients`, `diagnoses_icd`, `labevents`, `prescriptions`, plus the `d_icd_diagnoses` / `d_labitems` dictionaries) stored as compressed CSVs in `data/raw/`. The neurological cohort is built from scratch by the R scripts — there is no longer a pre-built `cohort.csv`.
- **Legacy synthetic generator:** `data/raw/generate_synthetic_cohort.py` (which produced the old 2,400-row synthetic `cohort.csv`) is kept for reference only. It is **not** part of the current pipeline.
- **Pipeline status:** Steps 1–7 have all been run end-to-end. The cohort, engineered features, imputed modeling table, cross-validated predictions for all three models, a saved XGBoost model, SHAP outputs, and the Shiny dashboard all exist. See [Results & Outputs](#results--outputs) for exactly what has and has not been recorded.

---

## Variable Dictionary

The modeling dataset (`data/processed/model_ready_step4.csv`) has **93,570 admissions × 44 columns** — 2 identifiers, 1 outcome, and **41 engineered modeling features**. Columns are produced by the feature-engineering scripts (Steps 1–4), so the names below are the *engineered* names, not raw MIMIC-IV column names. Variables are grouped by category.

> Cohort definition: an admission is included if it has at least one ICD-10 nervous-system diagnosis (chapter G00–G99), the patient is ≥18 at admission, and the patient survived the stay (`hospital_expire_flag == 0`). See `01_build_cohort.R`.

### Identifiers (2 columns) — Not used in modeling

| Column | Meaning |
|--------|---------|
| `subject_id` | Unique patient ID (like a medical record number) |
| `hadm_id` | Unique hospital admission ID (one patient can have multiple visits) |

### Outcome — The thing we predict (1 column)

| Column | Meaning |
|--------|---------|
| `readmit_30d` | **0** = did NOT come back within 30 days. **1** = the same patient had a later admission whose start was 0–30 days after this discharge. This is our **target variable**. In the current cohort, **14,852 of 93,570 admissions (≈15.9%)** are positive (the rest, 78,718, are negative). |

### Demographics — Who is the patient? (8 columns)

All demographics are pre-encoded for modeling (binary / one-hot) in Step 3a–4.

| Column | Meaning |
|--------|---------|
| `approx_age` | Age in years at admission (`anchor_age` adjusted to the admission year) |
| `female` | 1 = female, 0 = male (from `gender`) |
| `married` | 1 = marital status is MARRIED, else 0 — proxy for social support |
| `public_insurance` | 1 = Medicare or Medicaid, 0 = other/private |
| `race_Black`, `race_Asian`, `race_Hispanic`, `race_Other` | One-hot indicators for a simplified race grouping. **White is the reference category** (all four = 0) |

### Admission Info — What was the hospital stay like? (4 columns)

| Column | Meaning |
|--------|---------|
| `los_days` | **Length of stay** — days between admission and discharge. Longer stays often mean sicker patients |
| `admit_hour` | Hour of day (0–23) the admission started |
| `weekend_admit` | 1 = admitted on Saturday or Sunday |
| `emergency_admit` | 1 = `admission_type` is EW EMER., DIRECT EMER., or URGENT |

### Utilization — How much healthcare have they used? (1 column)

| Column | Meaning |
|--------|---------|
| `prior_admits_12m` | Count of the patient's prior admissions in the 365 days before this one. **Usually a top predictor** — frequent visitors keep coming back |

### Diagnoses — What's wrong with them? (12 columns)

All diagnosis flags are derived from the admission's ICD-10 codes (`diagnoses_icd`, `icd_version == 10`).

**Counts / summary:**

| Column | Meaning |
|--------|---------|
| `n_diagnoses` | Total ICD-10 diagnosis codes on the admission. More = more complex patient |
| `n_neuro_dx` | Number of neurological (G-chapter) diagnosis codes |
| `comorbidity_count` | Sum of six comorbidity flags below (diabetes, hypertension, heart failure, CKD, COPD, depression); range 0–6 |

**Comorbidity flags (1 = present on this admission):**

| Column | Meaning | ICD-10 prefix |
|--------|---------|---------------|
| `has_diabetes` | Diabetes mellitus | E10–E14 |
| `has_hypertension` | Hypertensive disease | I10–I16 |
| `has_heart_failure` | Heart failure | I50 |
| `has_ckd` | Chronic kidney disease | N18 |
| `has_copd` | Chronic lower respiratory disease (incl. COPD) | J44–J47 |
| `has_depression` | Depressive disorder | F32–F33 |

**Neurological condition flags (1 = present on this admission):**

| Column | Meaning | ICD-10 prefix |
|--------|---------|---------------|
| `has_epilepsy` | Epilepsy / seizure disorder | G40 |
| `has_sleep_disorder` | Sleep disorders | G47 |
| `has_parkinsons` | Parkinson's disease | G20 |

### Lab Values — Blood test results before discharge (9 columns)

These are the **last** recorded value of each key lab on the admission (from `labevents`). They have **missing values** because not every patient gets every test ordered — see [Missing Data](#missing-data).

| Column | What it measures | Normal Range | High means | Low means |
|--------|-----------------|-------------|------------|-----------|
| `lab_creatinine` | Kidney function (mg/dL) | 0.7–1.3 | Kidneys struggling | Normal |
| `lab_hemoglobin` | Oxygen-carrying protein (g/dL) | 12–17 | Normal | Anemia, weakness |
| `lab_white_blood_cells` | White blood cells (K/µL) | 4.5–11 | Infection/inflammation | Immune suppression |
| `lab_glucose` | Blood sugar (mg/dL) | 70–100 | Uncontrolled diabetes | Hypoglycemia risk |
| `lab_sodium` | Electrolyte (mEq/L) | 136–145 | Dehydration | Brain swelling risk |
| `lab_potassium` | Electrolyte (mEq/L) | 3.5–5.0 | Heart rhythm risk | Heart rhythm risk |
| `lab_albumin` | Nutrition / liver protein (g/dL) | 3.5–5.0 | Normal | Malnutrition, inflammation |
| `lab_platelet_count` | Clotting cells (K/µL) | 150–400 | Inflammation | Bleeding risk |
| `no_labs_flag` | 1 = the admission has **no** recorded labs at all (informative missingness; see below) |  |  |  |

### Medications — What drugs are they on? (7 columns)

Medication flags are derived by drug-name matching in `prescriptions`.

| Column | Meaning |
|--------|---------|
| `n_medications` | Number of distinct drugs prescribed. More = **polypharmacy** = higher complication risk |
| `on_anticoagulant` | 1 = blood thinners (warfarin, heparin, enoxaparin, apixaban, rivaroxaban). Common after stroke |
| `on_antiepileptic` | 1 = seizure medication (levetiracetam, valproic acid, phenytoin, etc.) |
| `on_antidepressant` | 1 = antidepressant (sertraline, fluoxetine, escitalopram, etc.) |
| `on_opioid` | 1 = opioid analgesic (morphine, oxycodone, fentanyl, etc.) |
| `on_insulin` | 1 = insulin for diabetes |
| `on_statin` | 1 = cholesterol medication. Protective for heart/stroke patients |

---

## Missing Data

In real hospitals, not every patient gets every lab test, so the **lab columns are the only ones with missing values** in the modeling table — the demographic, admission, diagnosis, utilization, and medication features are all complete (missing medication flags are filled with 0). Two things are worth noting:

- **Informative missingness:** A large share of admissions have **no recorded labs at all**. `04_imputation_prep.R` captures this with a dedicated `no_labs_flag` *before* imputation, so the model can use "labs were never drawn" as a signal. (The script comment estimates this at roughly half of admissions; the exact figure is printed to the console at run time but is not saved to a file.)
- **Per-lab missingness percentages are not stored.** Step 4 prints a missing-data summary to the console but does not write it to a results file, so exact per-column percentages are not recorded here. *(The earlier figures in this section were from the retired synthetic cohort and no longer apply to the real MIMIC-IV data.)*

**How we handle it:** Multiple Imputation by Chained Equations (**MICE**), using **predictive mean matching (PMM)** on the lab columns with `m = 5` imputations and `maxit = 5`. The current pipeline then proceeds with the **first** completed imputation for modeling. MICE is the gold standard for handling missing clinical data.

---

## Pipeline Overview

| Step | Script | What it does |
|------|--------|-------------|
| 1 | `01_build_cohort.R` | Reads the raw MIMIC-IV tables and builds the adult neurological cohort (ICD-10 G00–G99, survived stay) → `cohort_step1.csv` |
| 2 | `02_define_outcome.R` | Computes the 30-day readmission label per admission → `cohort_step2.csv` |
| 3a | `03a_features_demo_dx.R` | Demographic, admission, diagnosis, comorbidity, and prior-utilization features → `features_step3a.csv` |
| 3b | `03b_features_labs_meds_v3.R` | Adds last-value lab features and medication features → `features_step3_full.csv` |
| 4 | `04_imputation_prep.R` | `no_labs_flag`, MICE/PMM imputation of labs, one-hot encoding → `model_ready_step4.csv` |
| 5 | `05_modeling.R` | Trains Elastic Net, Random Forest, XGBoost with stratified 5-fold CV; saves out-of-fold predictions and a full XGBoost model |
| 6 | `06_shap.R` | SHAP values from the saved XGBoost model: global summary + dependence plots, plus per-patient SHAP data |
| 7 | `07_shiny_app.R` | Interactive R Shiny dashboard for population, individual-patient, and subgroup risk profiling |

`03b_features_labs_meds_v3.R` is the version used in the pipeline; `03b_features_labs_meds.R` and `..._v2.R` are earlier iterations kept for reference. `data/raw/generate_synthetic_cohort.py` is the retired synthetic-data generator and is not part of this flow.

---

## Models Used

| Model | Type | Why |
|-------|------|-----|
| **Elastic Net** | Regularized logistic regression | Interpretable baseline — gives coefficients clinicians can understand |
| **Random Forest** | Ensemble of decision trees | Robust, handles interactions, good middle ground |
| **XGBoost** | Gradient boosted trees | Best performance, paired with SHAP for interpretability |

**Evaluation metrics:**

| Metric | What it measures | Good value |
|--------|-----------------|------------|
| **AUC** | Can the model tell readmitted vs. not apart? | Closer to 1.0 |
| **Brier Score** | Are predicted probabilities accurate? | Closer to 0 |
| **Sensitivity** | Of actual readmissions, what % did we catch? | Higher is better |
| **Specificity** | Of non-readmissions, what % did we correctly clear? | Higher is better |

> **What the code currently reports:** `05_modeling.R` computes **AUC** (overall and per-fold) for all three models and, for the best model, picks the Youden-optimal threshold and reports **sensitivity, specificity, PPV, and NPV**. XGBoost uses `scale_pos_weight` to handle the class imbalance. **Brier score is not yet computed** in the script despite being listed above. All of these values are printed to the console only — they are **not** written to a metrics/comparison file (see below).

---

## Results & Outputs

All steps have been run on the real MIMIC-IV data. Here is what is actually saved on disk versus what is only printed at run time.

**Saved artifacts (present in the repo):**

| File | What it is |
|------|-----------|
| `data/processed/cohort_step1.csv` … `model_ready_step4.csv` | Cohort, outcome, and engineered feature tables for each step (93,570 admissions) |
| `data/processed/cv_predictions_step5.csv` | Out-of-fold CV predictions for **all three models** (`pred_enet`, `pred_rf`, `pred_xgb`) plus the true label, one row per admission |
| `data/processed/xgb_full_model.xgb` | XGBoost model retrained on the full data, used for SHAP |
| `data/processed/shap_data_step6.csv` | Per-patient SHAP values + feature values for a 5,000-admission sample (feeds the Shiny app) |
| `plots/shap_summary.png` | Global SHAP beeswarm summary plot |
| `plots/shap_dep_*.png` | SHAP dependence plots for the top 6 features: `prior_admits_12m`, `lab_hemoglobin`, `n_medications`, `approx_age`, `on_statin`, `los_days` |
| `output/tables/model_metrics.csv` | Out-of-fold metrics for all three models (produced by `08_metrics_summary.R` from `cv_predictions_step5.csv`) |
| `output/figures/roc_curves.png` | Combined out-of-fold ROC curves for the three models |
| `output/tables/xgb_calibration_metrics.csv` + `output/figures/xgb_calibration.png` | XGBoost raw vs Platt vs isotonic calibration comparison and reliability plot (from `09_calibration.R`) |

**Model performance (out-of-fold, from `cv_predictions_step5.csv`):**

Computed by `08_metrics_summary.R` and saved to `output/tables/model_metrics.csv`. The threshold is each model's Youden-optimal cutoff (maximizing sensitivity + specificity − 1).

| Model | AUC | Brier | Threshold | Sensitivity | Specificity | PPV | NPV |
|-------|-----|-------|-----------|-------------|-------------|-----|-----|
| Elastic Net | 0.6808 | 0.1241 | 0.1567 | 0.5405 | 0.7278 | 0.2726 | 0.8936 |
| Random Forest | 0.6856 | 0.1232 | 0.1909 | 0.5510 | 0.7177 | 0.2691 | 0.8944 |
| XGBoost | 0.6944 | 0.2113 | 0.4712 | 0.6347 | 0.6505 | 0.2552 | 0.9042 |

- AUCs of ~0.68–0.69 are consistent with published benchmarks for structured-data 30-day readmission models, which typically land in the ~0.65–0.75 range.
- **XGBoost calibration:** its raw probabilities are miscalibrated (Brier 0.2113, Youden threshold ~0.47) because `05_modeling.R` trains with `scale_pos_weight`. Post-hoc out-of-fold calibration (`09_calibration.R`) fixes this without changing AUC — Platt scaling cuts the Brier score from 0.2113 to 0.1227 (isotonic 0.1228), with AUC essentially unchanged (0.6944).

---

## File Structure

```
Hospital-readmission-prediction-from-EHR-data/
├── README.md                          ← You are here
├── 01_build_cohort.R                  ← Read MIMIC-IV, build neuro cohort
├── 02_define_outcome.R                ← 30-day readmission label
├── 03a_features_demo_dx.R             ← Demographics + diagnosis features
├── 03b_features_labs_meds_v3.R        ← Lab + medication features (active version)
├── 03b_features_labs_meds.R / _v2.R   ← Earlier iterations (reference only)
├── 04_imputation_prep.R               ← MICE imputation + encoding → model-ready
├── 05_modeling.R                      ← Elastic Net / RF / XGBoost, 5-fold CV
├── 06_shap.R                          ← SHAP interpretation
├── 07_shiny_app.R                     ← R Shiny dashboard
├── data/
│   ├── raw/                           ← Real MIMIC-IV tables (*.csv.gz)
│   │   ├── admissions.csv.gz, patients.csv.gz, diagnoses_icd.csv.gz
│   │   ├── labevents.csv.gz, prescriptions.csv.gz
│   │   ├── d_icd_diagnoses.csv.gz, d_labitems.csv.gz
│   │   └── generate_synthetic_cohort.py   ← Retired synthetic generator
│   └── processed/                     ← Step outputs (cohort, features, model-ready,
│                                         CV predictions, SHAP data, saved XGBoost model)
└── plots/                             ← SHAP summary + dependence plots
```

> Note: the large raw MIMIC-IV files (`labevents.csv.gz` ≈ 2.6 GB, `prescriptions.csv.gz` ≈ 0.6 GB) are credentialed PhysioNet data and should not be redistributed. There is currently no separate `output/tables/` or `output/models/` directory — saved tables and the model object live under `data/processed/`, and figures live under `plots/`.

---

## How to Run

### Run the pipeline (real MIMIC-IV data)

1. Place the MIMIC-IV `hosp` tables as gzipped CSVs in `data/raw/` (`admissions`, `patients`, `diagnoses_icd`, `labevents`, `prescriptions`, `d_icd_diagnoses`, `d_labitems`). These require credentialed PhysioNet access.
2. Open the project (`Hospital-readmission-prediction-from-EHR-data.Rproj`) in RStudio. The scripts use absolute `G:/...` paths — adjust the `project_root` / `processed_dir` paths at the top of each script if your location differs.
3. Run the scripts in order:
   - `01_build_cohort.R`
   - `02_define_outcome.R`
   - `03a_features_demo_dx.R`
   - `03b_features_labs_meds_v3.R`
   - `04_imputation_prep.R`
   - `05_modeling.R`
   - `06_shap.R`
4. Launch the dashboard: `shiny::runApp("07_shiny_app.R")` (needs `shap_data_step6.csv` from Step 6).

> The `labevents` step reads a very large file; `03b_features_labs_meds_v3.R` reads only the needed columns and filters early to keep memory manageable, with a Python chunked-filtering fallback documented in the script's comments.

---

## R Dependencies

```r
install.packages(c(
  "tidyverse", "data.table", "janitor",
  "mice", "VIM", "naniar",
  "glmnet", "ranger", "xgboost",
  "caret", "pROC", "CalibrationCurves",
  "SHAPforxgboost", "shapviz",
  "ggplot2", "patchwork", "gt", "gtsummary",
  "shiny", "DT", "plotly"   # Step 7 Shiny dashboard
))
```

The scripts that have actually been run rely on: `data.table`, `mice`, `glmnet`, `ranger`, `xgboost`, `pROC`, `caret` (modeling); `SHAPforxgboost`, `ggplot2` (SHAP); and `shiny`, `data.table`, `ggplot2`, `DT`, `plotly` (dashboard).

---

## Key Terms Glossary

| Term | Meaning |
|------|---------|
| **Readmission** | Patient returns to hospital within a set time after discharge |
| **EHR** | Electronic Health Records — digital records from hospital systems |
| **RWE** | Real-World Evidence — evidence from real clinical data, not clinical trials |
| **MIMIC-IV** | Free research database of hospital records from MIT/Beth Israel |
| **ICD-10** | International Classification of Diseases — standardized diagnosis codes |
| **Comorbidity** | Additional disease beyond the primary condition |
| **Charlson Index** | Weighted comorbidity score predicting mortality risk |
| **LOS** | Length of Stay — days hospitalized |
| **ICU** | Intensive Care Unit — for critically ill patients |
| **MICE** | Multiple Imputation by Chained Equations — gold standard for missing data |
| **MAR** | Missing At Random — missingness depends on other observed variables |
| **AUC** | Area Under ROC Curve — model discrimination (0.5 = random, 1.0 = perfect) |
| **Brier Score** | Calibration metric — how close predictions are to reality (lower = better) |
| **SHAP** | SHapley Additive exPlanations — explains individual predictions |
| **Cross-validation** | Testing model on held-out data folds to prevent overfitting |
| **Elastic Net** | Logistic regression with penalty — auto-selects important variables |
| **Random Forest** | Many decision trees voting together |
| **XGBoost** | Extreme Gradient Boosting — powerful tree model, often best performer |
| **Polypharmacy** | Taking many medications — higher risk of interactions and errors |
| **PMM** | Predictive Mean Matching — imputation method for continuous variables |
| **CMS** | Centers for Medicare & Medicaid Services — US federal health agency |
| **HRRP** | Hospital Readmissions Reduction Program — CMS penalty program |

---

*Author: Mengyan Jing · PhD Statistics, University of Missouri*
*Status: Full pipeline run on real MIMIC-IV data — cohort, features, 3-model CV, saved XGBoost model, SHAP outputs, and Shiny dashboard all complete. Headline performance metrics are computed but not yet persisted to a results file (see [Results & Outputs](#results--outputs)).*
