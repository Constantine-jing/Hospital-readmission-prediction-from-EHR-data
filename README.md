# Predicting 30-Day Hospital Readmission in Neurological Patients (MIMIC-IV)

## Table of Contents
- [What Is This Project?](#what-is-this-project)
- [Why Does This Matter?](#why-does-this-matter)
- [Data Source](#data-source)
- [Variable Dictionary](#variable-dictionary)
- [Missing Data](#missing-data)
- [Pipeline Overview](#pipeline-overview)
- [Models Used](#models-used)
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

- **Demo phase:** Using synthetic data (`cohort.csv`) that mimics the MIMIC-IV structure with realistic clinical distributions
- **Production phase (planned):** Will swap in real MIMIC-IV data once credentialed access is approved via PhysioNet

---

## Variable Dictionary

Our dataset has **2,400 patients × 37 columns**. Here is every variable grouped by category.

### Identifiers (2 columns) — Not used in modeling

| Column | Meaning |
|--------|---------|
| `subject_id` | Unique patient ID (like a medical record number) |
| `hadm_id` | Unique hospital admission ID (one patient can have multiple visits) |

### Outcome — The thing we predict (1 column)

| Column | Meaning |
|--------|---------|
| `readmit_30d` | **0** = did NOT come back within 30 days. **1** = came back within 30 days. This is our **target variable**. |

### Demographics — Who is the patient? (5 columns)

| Column | Meaning |
|--------|---------|
| `age_at_admit` | Age in years at admission |
| `gender` | M = male, F = female |
| `race` | WHITE, BLACK/AFRICAN AMERICAN, HISPANIC/LATINO, ASIAN, OTHER, UNKNOWN |
| `insurance` | Medicare (age 65+ or disabled), Medicaid (low income), Other (private) |
| `marital_status` | MARRIED, SINGLE, DIVORCED, WIDOWED, UNKNOWN — social support affects recovery |

### Admission Info — What was the hospital stay like? (2 columns)

| Column | Meaning |
|--------|---------|
| `los_days` | **Length of stay** — days in hospital. Longer stays often mean sicker patients |
| `admission_type` | EMERGENCY (came through ER), URGENT (needed quick admission), OBSERVATION (short monitoring) |

### Utilization — How much healthcare have they used? (3 columns)

| Column | Meaning |
|--------|---------|
| `prior_admits_12m` | Hospital admissions in past 12 months. **Usually the #1 predictor** — frequent visitors keep coming back |
| `had_icu_stay` | 0 = no, 1 = yes. Intensive Care Unit = very sick |
| `icu_los_days` | Days in ICU (0 if no ICU stay) |

### Diagnoses — What's wrong with them? (11 columns)

**Neurological conditions (our target population):**

| Column | Meaning | ICD-10 Code |
|--------|---------|-------------|
| `dx_cerebral_infarction` | Ischemic stroke — blood clot blocks brain artery | I63 |
| `dx_ich` | Intracerebral hemorrhage — bleeding in the brain (more dangerous) | I61 |
| `dx_epilepsy` | Seizure disorder | G40 |
| `dx_parkinson` | Progressive movement disorder | G20 |
| `dx_alzheimer` | Progressive dementia | G30 |
| `dx_ms` | Multiple sclerosis — immune system attacks nerve coverings | G35 |

**Comorbidities (other diseases they also have):**

| Column | Meaning | Why it matters |
|--------|---------|----------------|
| `dx_diabetes` | Diabetes | Affects healing, infection risk, organ function |
| `dx_chf` | Congestive heart failure | Heart can't pump well, fluid buildup |
| `dx_copd` | Chronic obstructive pulmonary disease | Chronic lung disease |
| `dx_ckd` | Chronic kidney disease | Kidneys failing, affects drug dosing |

**General:**

| Column | Meaning |
|--------|---------|
| `num_diagnoses` | Total diagnosis codes on record. More = more complex patient |

### Lab Values — Blood test results before discharge (8 columns)

These are the **last** blood test results before discharge. They have **missing values** because not every patient gets every test ordered.

| Column | What it measures | Normal Range | High means | Low means |
|--------|-----------------|-------------|------------|-----------|
| `creatinine` | Kidney function (mg/dL) | 0.7–1.3 | Kidneys struggling | Normal |
| `hemoglobin` | Oxygen-carrying protein (g/dL) | 12–17 | Normal | Anemia, weakness |
| `wbc` | White blood cells (K/µL) | 4.5–11 | Infection/inflammation | Immune suppression |
| `glucose` | Blood sugar (mg/dL) | 70–100 | Uncontrolled diabetes | Hypoglycemia risk |
| `sodium` | Electrolyte (mEq/L) | 136–145 | Dehydration | Brain swelling risk |
| `potassium` | Electrolyte (mEq/L) | 3.5–5.0 | Heart rhythm risk | Heart rhythm risk |
| `bun` | Blood urea nitrogen (mg/dL) | 7–20 | Kidney problems | Normal |
| `platelet` | Clotting cells (K/µL) | 150–400 | Inflammation | Bleeding risk |

### Medications — What drugs are they on? (5 columns)

| Column | Meaning |
|--------|---------|
| `num_medications` | Total drugs prescribed. More = **polypharmacy** = higher complication risk |
| `on_anticoagulant` | 1 = blood thinners (warfarin, heparin). Common after stroke |
| `on_statin` | 1 = cholesterol medication. Protective for heart/stroke patients |
| `on_insulin` | 1 = insulin for diabetes |
| `on_antiepileptic` | 1 = seizure medication |

---

## Missing Data

In real hospitals, not every patient gets every lab test. Our data reflects this:

| Variable | % Missing | Why |
|----------|-----------|-----|
| `glucose` | ~16% | Non-diabetic patients less likely to have glucose ordered (MAR pattern) |
| `bun` | ~8% | Not always part of standard panel |
| `hemoglobin` | ~7% | May not be ordered if no bleeding concern |
| `potassium` | ~6% | Sometimes not in basic panel |
| `wbc` | ~6% | May not be ordered for non-infectious presentations |
| `creatinine` | ~5% | Usually ordered but sometimes missing |
| `sodium` | ~4% | Usually ordered but sometimes missing |
| `platelet` | ~4% | Part of CBC, sometimes not ordered |
| `num_medications` | ~3% | Occasional documentation gaps |

**How we handle it:** Multiple Imputation by Chained Equations (**MICE**) — creates 5 plausible versions of the dataset with missing values filled in using information from other variables. This is the gold standard for handling missing clinical data.

---

## Pipeline Overview

| Step | Script | What it does |
|------|--------|-------------|
| 0 | `generate_synthetic_cohort.py` | Creates synthetic demo data (cohort.csv) |
| 1 | `01_cohort_extraction.sql` | **(Real data only)** Extracts cohort from MIMIC-IV database |
| 2 | `02_feature_engineering.R` | Loads CSV, cleans, EDA, Table 1, MICE imputation |
| 3 | `03_modeling.R` | Trains 3 models, stratified 5-fold CV, compares performance |
| 4 | `04_shap_interpretation.R` | SHAP values, global + individual explanation plots |

**For demo:** Start at Step 2 with `cohort.csv`. Step 1 is only for real MIMIC-IV data.

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

---

## File Structure

```
readmission_project/
├── README.md                          ← You are here
├── generate_synthetic_cohort.py       ← Creates demo data
├── 01_cohort_extraction.sql           ← SQL for real MIMIC-IV (future)
├── 02_feature_engineering.R           ← Data cleaning, EDA, imputation
├── 03_modeling.R                      ← Model training & comparison
├── 04_shap_interpretation.R           ← SHAP explanations
├── data/
│   ├── raw/
│   │   └── cohort.csv                 ← Input data
│   └── processed/                     ← Output of Step 2
├── output/
│   ├── figures/                       ← All plots
│   ├── tables/                        ← Table 1, comparisons
│   └── models/                        ← Saved model objects
└── app/
    └── app.R                          ← R Shiny dashboard (future)
```

---

## How to Run

### Demo (synthetic data)
1. Create folder structure above
2. Place `cohort.csv` in `data/raw/`
3. Open project folder in VSCode
4. Run `02_feature_engineering.R`
5. Run `03_modeling.R`
6. Run `04_shap_interpretation.R`

### Real data (after MIMIC-IV access approved)
1. Run `01_cohort_extraction.sql` against MIMIC-IV BigQuery
2. Export result → replace `data/raw/cohort.csv`
3. Run Steps 2–4 (everything else stays the same)

---

## R Dependencies

```r
install.packages(c(
  "tidyverse", "data.table", "janitor",
  "mice", "VIM", "naniar",
  "glmnet", "ranger", "xgboost",
  "caret", "pROC", "CalibrationCurves",
  "SHAPforxgboost", "shapviz",
  "ggplot2", "patchwork", "gt", "gtsummary"
))
```

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
*Status: Demo phase with synthetic data — real MIMIC-IV analysis pending credentialed access*
