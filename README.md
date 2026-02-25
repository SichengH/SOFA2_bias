# Evaluation of SOFA-2 Score Performance Across Demographic Subgroups

An external validation study assessing potential performance disparities
of the **Sequential Organ Failure Assessment 2 (SOFA-2)** score for ICU
mortality prediction using MIMIC-IV v3.1.

## Overview

The SOFA-2 score was recently validated across more than 3 million ICU
admissions internationally. However, subgroup-specific performance
differences were not reported.

This repository contains the complete analytic pipeline used to
externally evaluate:

-   **Discrimination** (AUROC)
-   **Calibration** (intercept and slope)

across demographic subgroups defined by:

-   Age\
-   Sex\
-   Race/ethnicity\
-   Primary language\
-   Insurance status

The objective of this study is to assess whether SOFA-2 exhibits
systematic performance differences across demographic groups when
applied to a large, diverse ICU cohort.

## Repository Structure

    ├── SOFA2_sql/
    │   └── mimiciv-SOFA2/
    │       ├── SOFA2.sql
    │       ├── SOFA2_component.sql
    │       ├── SOFA.sql
    │       ├── bg_spo2.sql
    │       ├── ECMO_hourly.sql
    │       ├── ECMO_settings.sql
    │       ├── mechanical_support_hourly.sql
    │       ├── vaso_hourly.sql
    │       └── delirium-drug.sql
    │
    ├── SOFA2_bias_analysis/
    │   ├── Data_Processing.R
    │   └── Downstream_Analysis.R

### Directory Description

#### `SOFA2_sql/mimiciv-SOFA2/`

Implements the full SOFA-2 scoring pipeline in BigQuery (Standard SQL):

-   **bg_spo2.sql** -- Blood gas pivot table with paired SpO₂/FiO₂
    fallback logic\
-   **ECMO_hourly.sql** -- Hourly VV- and VA-ECMO flags\
-   **mechanical_support_hourly.sql** -- Mechanical circulatory support
    detection (IABP, Impella, VAD)\
-   **vaso_hourly.sql** -- Hourly vasopressor dosing for 7 agents
    (≥60-minute filter)\
-   **delirium-drug.sql** -- IV and oral delirium drug administration\
-   **SOFA2_component.sql** -- Assembly of all hourly component inputs\
-   **SOFA2.sql** -- Organ subscore computation and 24-hour rolling
    maximum\
-   **SOFA.sql** -- Original SOFA score implementation (for comparison)

#### `SOFA2_bias_analysis/`

Implements cohort construction and fairness evaluation in R:

-   **Data_Processing.R** -- Cohort assembly and missingness flag
    generation\
-   **Downstream_Analysis.R** -- Discrimination and calibration analyses

## Data Requirements

This project uses **MIMIC-IV version 3.1**, hosted on PhysioNet.

The dataset must be loaded into **Google BigQuery** prior to running the
SQL scripts.


## Analytic Pipeline

### Step 1 --- Build SOFA-2 Components (BigQuery)

Run the SQL scripts in the following order:

1.  `bg_spo2.sql`\
2.  `ECMO_hourly.sql`\
3.  `mechanical_support_hourly.sql`\
4.  `vaso_hourly.sql`\
5.  `delirium-drug.sql`\
6.  `SOFA2_component.sql`\
7.  `SOFA2.sql`

This produces hourly organ subscores and 24-hour maximum SOFA-2 scores
for each ICU stay.

### Step 2 --- Cohort Construction (R)

Run `Data_Processing.R`, which:

-   Queries SOFA-2 scores, ICU stays, and demographics from BigQuery\
-   Restricts to the first ICU stay per patient\
-   Computes first-24-hour maximum SOFA-2 scores\
-   Generates component-level and variable-level missingness flags\
-   Outputs: `mimic_first_icu_final.csv`

### Step 3 --- Fairness Analysis (R)

Run `Downstream_Analysis.R`, which:

-   Applies physiologic plausibility filters\
-   Excludes stays \< 6 hours\
-   Excludes missing SOFA-2 or mortality outcome\
-   Constructs subgroup variables\
-   Computes discrimination and calibration metrics\
-   Generates all tables and figures below

## Outputs

The downstream analysis generates:

-   **Table 1:** Baseline characteristics by subgroup\
-   **Table 2:** AUROC, ΔAUROC, calibration intercept and slope by
    subgroup\
-   **eTable 2A:** Component-level missingness (first 24 hours)\
-   **eTable 2B:** Variable-level missingness (first 24 hours)\
-   **eTable 3:** Organ subscore distributions by subgroup\
-   **Figure 2:** Mortality by SOFA-2 score stratified by demographics\
-   **eFigures 2--6:** Calibration plots by subgroup

## Reproducibility

To run the full pipeline:

1.  Obtain approved access to MIMIC-IV v3.1\
2.  Load MIMIC-IV into Google BigQuery\
3.  Update dataset paths in all SQL files\
4.  Configure Google Cloud authentication for R

## Software

-   **SQL:** Google BigQuery (Standard SQL)\
-   **R:** ≥ 4.5.0

## Citation

If you use this code, please cite:

...

## License

The analytic code in this repository is publicly available. MIMIC-IV data access is governed by the PhysioNet data use agreement.
