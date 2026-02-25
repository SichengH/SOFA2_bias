# Evaluation of SOFA-2 Score Performance Across Demographic Subgroups

An external validation study evaluating the fairness of the SOFA-2 (Sequential Organ Failure Assessment 2) score for ICU mortality prediction across demographic subgroups using MIMIC-IV.

## Overview

The SOFA-2 score was recently validated across >3 million ICU admissions from 9 countries but was not evaluated for performance differences across demographic subgroups. This repository contains the complete analytic pipeline (SQL scoring queries and R analysis scripts) used to assess SOFA-2 discrimination and calibration by age, sex, race/ethnicity, primary language, and insurance status.

## Repository Structure

```
├── SOFA2_sql/
│   └── mimiciv-SOFA2/                # SOFA-2 scoring implementation
│       ├── SOFA2.sql                 # SOFA-2 organ scores + 24h rolling max
│       ├── SOFA2_component.sql       # Component assembly (all raw inputs per hour)
│       ├── SOFA.sql                  # Original SOFA score (for comparison)
│       ├── bg_spo2.sql               # Blood gas pivot with SpO2/FiO2 fallback
│       ├── ECMO_hourly.sql           # ECMO flags (VV/VA) per ICU hour
│       ├── ECMO_settings.sql         # ECMO settings pivot (audit only)
│       ├── mechanical_support_hourly.sql  # IABP/Impella/VAD detection
│       ├── vaso_hourly.sql           # 7-agent vasopressor rates (≥60 min filter)
│       └── delirium-drug.sql         # Delirium drug administration (IV + oral)
│
├── SOFA2_bias_analysis/
│   ├── Data_Processing.R             # Cohort assembly + missingness flags
│   └── Downstream_Analysis.R         # Fairness analysis (Tables, Figures)
```

## Data Requirements

This project uses **MIMIC-IV version 3.1**, a freely accessible electronic health record dataset hosted on [PhysioNet](https://physionet.org/content/mimiciv/3.1/).

## Pipeline

### Step 1: Build Derived Tables

Run the SQL scripts in `mimiciv-derived-public/` on Google BigQuery to create upstream derived tables (vitals, labs, ventilation status, vasopressors, etc.). These are standard MIMIC-IV derived concepts.

### Step 2: Build SOFA-2 Components

Run the scripts in `mimiciv-SOFA2/` in the following order:

1. **`bg_spo2.sql`** — Blood gas pivot with paired SpO2/FiO2 ratios
2. **`ECMO_hourly.sql`** — ECMO flags (VV-ECMO → respiratory; VA-ECMO → respiratory + cardiovascular)
3. **`mechanical_support_hourly.sql`** — Mechanical circulatory support detection
4. **`vaso_hourly.sql`** — Hourly vasopressor rates (7 agents, ≥60 min duration filter)
5. **`delirium-drug.sql`** — Delirium drug administration (IV + oral)
6. **`SOFA2_component.sql`** — Assembles all raw inputs into one table per (stay_id, hour)
7. **`SOFA2.sql`** — Computes organ scores and 24-hour rolling maximums

### Step 3: Extract Cohort

Run **`Data_Processing.R`** which:
- Downloads SOFA-2 scores, demographics, and ICU stay data from BigQuery
- Identifies first ICU stay per patient
- Computes 24-hour max SOFA-2 scores
- Generates variable-level and component-level missingness flags
- Outputs `mimic_first_icu_final.csv`

### Step 4: Fairness Analysis

Run **`Downstream_Analysis.R`** which:
- Applies plausible physiologic range filters
- Excludes stays < 6 hours and those missing SOFA-2 or mortality outcome
- Creates demographic subgroup variables
- Produces all study outputs (see below)

## Outputs

| Output | Description |
|---|---|
| **Table 1** | Baseline characteristics (age, sex, race, language, insurance, SOFA-2, mortality) |
| **Table 2** | AUROC, ΔAUROC, calibration intercept/slope by subgroup |
| **eTable 2A** | Component-level missingness (first 24h) |
| **eTable 2B** | Variable-level missingness (first 24h) |
| **eTable 3** | Organ subscores by subgroup (mean/SD and median/IQR) |
| **Figure 2** | Mortality bar charts by SOFA-2 score stratified by demographics |
| **eFigures 2–6** | Calibration plots by subgroup |

## Software

- **SQL**: Google BigQuery (Standard SQL)
- **R** ≥ 4.5.0 

## Citation

If you use this code, please cite:

...

## License

The analytic code in this repository is publicly available. MIMIC-IV data access is governed by the PhysioNet data use agreement.
