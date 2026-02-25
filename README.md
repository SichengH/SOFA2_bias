# Evaluation of SOFA-2 Score Performance Across Demographic Subgroups

An external validation study evaluating the fairness of the SOFA-2 (Sequential Organ Failure Assessment 2) score for ICU mortality prediction across demographic subgroups using MIMIC-IV.

## Overview

The SOFA-2 score was recently validated across >3 million ICU admissions from 9 countries but was not evaluated for performance differences across demographic subgroups. This repository contains the complete analytic pipeline — SQL scoring queries and R analysis scripts — used to assess SOFA-2 discrimination and calibration by age, sex, race/ethnicity, primary language, and insurance status.

**Key findings** (n = 64,220 ICU admissions from MIMIC-IV v3.1):
- Overall AUROC of 0.776 with excellent calibration
- Discrimination declined significantly with age (AUROC 0.864 for ages 18–44 vs. 0.723 for ages ≥75; ΔAUROC −0.142)
- Significantly lower discrimination among non-English speakers (ΔAUROC −0.039)
- Patients with unknown race/ethnicity (14.3% of cohort) had nearly double the overall mortality rate (15.8% vs. 8.4%)

## Repository Structure

```
├── SOFA2_sql/
│   ├── mimiciv-SOFA2/                # SOFA-2 scoring implementation
│   │   ├── SOFA2.sql                 # SOFA-2 organ scores + 24h rolling max
│   │   ├── SOFA2_component.sql       # Component assembly (all raw inputs per hour)
│   │   ├── SOFA.sql                  # Original SOFA score (for comparison)
│   │   ├── bg_spo2.sql              # Blood gas pivot with SpO2/FiO2 fallback
│   │   ├── ECMO_hourly.sql          # ECMO flags (VV/VA) per ICU hour
│   │   ├── ECMO_settings.sql        # ECMO settings pivot (audit only)
│   │   ├── mechanical_support_hourly.sql  # IABP/Impella/VAD detection
│   │   ├── vaso_hourly.sql          # 7-agent vasopressor rates (≥60 min filter)
│   │   └── delirium-drug.sql        # Delirium drug administration (IV + oral)
│   │
│   └── mimiciv-derived-public/       # Upstream derived tables (from MIMIC-IV)
│       ├── Blood-gas.sql             # ABG pivot table
│       ├── Chemistry.sql             # Chemistry labs
│       ├── Complete-blood-count.sql  # CBC including platelets
│       ├── Enzyme.sql                # Liver enzymes / bilirubin
│       ├── GCS.sql                   # Glasgow Coma Scale
│       ├── Vitalsigns.sql            # Vital signs (HR, BP, SpO2, temp)
│       ├── Urine_output.sql          # Urine output events
│       ├── Urine_output_rate.sql     # UO rate (mL/kg/hr) at 6/12/24h windows
│       ├── RRT.sql                   # Renal replacement therapy
│       ├── ventilation.sql           # Ventilation status classification
│       ├── Ventilator_settings.sql   # Ventilator settings pivot
│       ├── Oxygen_delivery.sql       # O2 delivery devices
│       ├── Vasopressors(combined).sql # Individual vasopressor extractions
│       ├── Vasoactive_agent.sql      # Combined vasoactive agent table
│       ├── Age.sql                   # Patient age at admission
│       ├── Height.sql                # Patient height
│       ├── Weight_duration.sql       # Weight with start/stop times
│       ├── APS-III.sql               # Acute Physiology Score III
│       ├── CCI.sql                   # Charlson Comorbidity Index
│       ├── Antibiotics.sql           # Antibiotic prescriptions
│       ├── Suspicion_of_infection.sql # Sepsis-3 infection criteria
│       ├── Sepsis3.sql               # Sepsis-3 onset definition
│       ├── Blood-differential.sql    # WBC differential
│       ├── Cardiac-markers.sql       # Troponin, CK-MB, NT-proBNP
│       ├── Coagulation.sql           # Coagulation labs
│       ├── Surgical_status.sql       # Surgical vs. non-surgical admission
│       ├── ICU_hourly.sql            # One row per ICU hour
│       ├── ICU_daily.sql             # One row per ICU day
│       ├── ICUstay_detail.sql        # ICU stay details
│       └── ICUstay_times.sql         # ICU stay time boundaries
│
├── SOFA2_bias_analysis/
│   ├── Data_Processing.R             # Cohort assembly + missingness flags
│   └── Downstream_Analysis.R         # Fairness analysis (Tables 1–2, Figures)
│
└── README.md
```

## Data Requirements

This project uses **MIMIC-IV version 3.1**, a freely accessible electronic health record dataset hosted on [PhysioNet](https://physionet.org/content/mimiciv/3.1/). Access requires:

1. Completion of CITI human-subjects training
2. A signed data use agreement via PhysioNet
3. A Google BigQuery project for running SQL queries

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
- Applies plausible physiologic range filters (eTable 1)
- Excludes stays < 6 hours and those missing SOFA-2 or mortality outcome
- Creates demographic subgroup variables
- Produces all study outputs (see below)

## SOFA-2 Implementation Details

Key differences from the original SOFA score implemented in this codebase:

| Component | SOFA-2 Modification |
|---|---|
| **Respiratory** | Revised P/F thresholds (≤75/150/225/300); SpO2/FiO2 fallback when PaO2 unavailable (SpO2 < 98% only); ventilatory support includes NIV, HFNC, CPAP; any ECMO → score 4 |
| **Cardiovascular** | 7 vasopressors (adds milrinone, vasopressin, phenylephrine); NE+EPI dose tiers with combination rules; mechanical support (IABP/Impella/VAD/VA-ECMO) → score 4; revised MAP tiers (<40/50/60/70) |
| **Neurological** | GCS motor fallback for intubated patients; delirium drug administration → minimum score 1 |
| **Renal** | RRT (any) → score 4; RRT criteria check (Cr > 3.5 + K⁺ ≥ 6 or acidosis); urine output at 6/12/24h windows |
| **Coagulation** | Revised platelet thresholds (≤50/80/100/150) |
| **Hepatic** | Bilirubin score 2 threshold raised from ≥2.0 to >3.0 mg/dL |

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
- **R** ≥ 4.5.0 with packages: `data.table`, `tidyverse`, `bigrquery`, `DBI`, `pROC`, `gt`, `ggplot2`, `patchwork`, `writexl`, `comorbidity`, `table1`

## Citation

If you use this code, please cite:

> Ellen J. Evaluation of SOFA-2 Score Performance Across Demographic Subgroups: An External Validation Study Using MIMIC-IV. 2025.

And the SOFA-2 development study:

> Ranzani OT, Singer M, Salluh JIF, et al. Development and Validation of the Sequential Organ Failure Assessment (SOFA)-2 Score. *JAMA*. 2025. doi:10.1001/jama.2025.20516

## License

The analytic code in this repository is publicly available. MIMIC-IV data access is governed by the PhysioNet data use agreement.
