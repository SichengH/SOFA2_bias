# ============================================================
# SOFA-2 FAIRNESS ANALYSIS (MIMIC-IV)
# ============================================================
# 
# OUTPUTS:
#   - Table 1: Baseline characteristics
#   - Table 2: Discrimination (AUROC) and Calibration by subgroup
#   - Supplementary Table 1A: Organ subscores by subgroup (mean, SD)
#   - Supplementary Table 1B: Organ subscores by subgroup (median, IQR)
#   - Figure 2A-E: Bar charts of mortality by SOFA score and demographics
#   - Supplementary calibration plots
#
# SUBGROUPS ANALYZED:
#   - Age: 18–44, 45–64, 65–74, ≥75
#   - Sex: Male, Female
#   - Race/Ethnicity: White, Black, Hispanic, Asian, Other, Unknown
#   - Language: English, Non-English, Unknown
#   - Insurance: Private, Medicare, Medicaid, Other
#
# DATA HANDLING:
#   - Values outside plausible physiological ranges are excluded
#   - Patients missing SOFA-2 total score or ICU mortality outcome are excluded
#

# ============================================================
# SECTION 1: SETUP - Load packages and data
# ============================================================

# Install and load required packages
pkgs <- c("dplyr", "tidyr", "stringr", "purrr", "tibble", "gt", "pROC", "ggplot2")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, require, character.only = TRUE))

# [D-04] Filename corrected: Data_Processing.R writes mimic_first_icu.csv
sofa2 <- read.csv(
  "/Users/jellen/downloads/mimic_first_icu.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "NaN")
)

# Verify required columns exist
required_cols <- c(
  "age", "gender", "race", "language", "insurance", "icu_death_flag", "los",
  "cns_24hours", "cardiovascular_24hours", "respiration_24hours",
  "liver_24hours", "renal_24hours", "coagulation_24hours",
  "gcs_min", "meanbp_min",
  "rate_norepinephrine", "rate_epinephrine", "rate_dopamine", "rate_dobutamine",
  "rate_milrinone", "rate_vasopressin", "rate_phenylephrine",
  "pao2fio2ratio_novent", "pao2fio2ratio_vent",
  "bilirubin_max", "creatinine_max", "uomlkghr_24hr", "platelet_min",
  "sofa_24hours"
)
missing_cols <- setdiff(required_cols, names(sofa2))
if (length(missing_cols) > 0) stop("Missing required columns: ", paste(missing_cols, collapse = ", "))


# ============================================================
# SECTION 2: HELPER FUNCTIONS
# ============================================================

# Format count and percentage: "n (x.x%)"
fmt_n_pct <- function(n, N) {
  sprintf("%s (%.1f%%)", n, 100 * n / N)
}

# Format mean and standard deviation: "mean (SD)"
fmt_mean_sd <- function(x, digits = 1) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"), mean(x), sd(x))
}

# Format median and interquartile range: "median (Q1–Q3)"
fmt_median_iqr <- function(x, digits = 0) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  q <- quantile(x, probs = c(0.25, 0.5, 0.75), names = FALSE)
  sprintf(paste0("%.", digits, "f (%.", digits, "f–%.", digits, "f)"), q[2], q[1], q[3])
}

# Safe logit transformation (avoids infinity for p near 0 or 1)
safe_logit <- function(p, eps = 1e-6) {
  qlogis(pmin(pmax(p, eps), 1 - eps))
}

# Brier reliability component (measures calibration)
# Lower values indicate better calibration
brier_reliability <- function(y, p, bins = 10) {
  df <- tibble(y = y, p = p) %>% filter(!is.na(y), !is.na(p))
  if (nrow(df) == 0) return(NA_real_)
  brks <- unique(quantile(df$p, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE))
  if (length(brks) < 3) return(0)
  df <- df %>% mutate(bin = cut(p, breaks = brks, include.lowest = TRUE))
  summ <- df %>% group_by(bin) %>% summarise(n = n(), pbar = mean(p), ybar = mean(y), .groups = "drop")
  N <- sum(summ$n)
  sum((summ$n / N) * (summ$pbar - summ$ybar)^2)
}

# Bootstrap confidence interval for difference between two groups
boot_diff <- function(df_ref, df_cmp, metric_fun, B = 1000, seed = 1) {
  set.seed(seed)
  d0 <- metric_fun(df_cmp) - metric_fun(df_ref)
  diffs <- replicate(B, {
    r_ref <- df_ref[sample.int(nrow(df_ref), replace = TRUE), , drop = FALSE]
    r_cmp <- df_cmp[sample.int(nrow(df_cmp), replace = TRUE), , drop = FALSE]
    metric_fun(r_cmp) - metric_fun(r_ref)
  })
  ci <- quantile(diffs, probs = c(0.025, 0.975), na.rm = TRUE)
  list(diff = d0, lo = ci[[1]], hi = ci[[2]])
}

# Calculate AUROC with 95% confidence interval
auc_ci <- function(df, outcome_col, score_col) {
  df <- df %>% filter(!is.na(.data[[outcome_col]]), !is.na(.data[[score_col]]))
  if (nrow(df) < 10 || length(unique(df[[outcome_col]])) < 2) {
    return(list(auc = NA_real_, lo = NA_real_, hi = NA_real_))
  }
  r <- pROC::roc(response = df[[outcome_col]], predictor = df[[score_col]], quiet = TRUE, direction = "<")
  ci <- as.numeric(pROC::ci.auc(r))
  list(auc = as.numeric(pROC::auc(r)), lo = ci[1], hi = ci[3])
}

# Calculate calibration intercept and slope for a subgroup
get_calibration_for_subgroup <- function(d, min_n = 50) {
  if (nrow(d) < min_n || length(unique(d$icu_mort)) < 2) {
    return(list(int = NA_real_, int_ci = NA_character_, slope = NA_real_, slope_ci = NA_character_))
  }
  
  # Calibration intercept (offset model with slope fixed at 1)
  m_int <- tryCatch(glm(icu_mort ~ 1 + offset(lp_hat), data = d, family = binomial()), error = function(e) NULL)
  if (is.null(m_int)) {
    return(list(int = NA_real_, int_ci = NA_character_, slope = NA_real_, slope_ci = NA_character_))
  }
  int <- coef(m_int)[1]
  int_se <- sqrt(vcov(m_int)[1, 1])
  int_ci <- sprintf("%.2f, %.2f", int - 1.96 * int_se, int + 1.96 * int_se)
  
  # Calibration slope
  m_slope <- tryCatch(glm(icu_mort ~ lp_hat, data = d, family = binomial()), error = function(e) NULL)
  if (is.null(m_slope)) {
    return(list(int = int, int_ci = int_ci, slope = NA_real_, slope_ci = NA_character_))
  }
  b1 <- coef(m_slope)[2]
  b1_se <- sqrt(diag(vcov(m_slope)))[2]
  slope_ci <- sprintf("%.2f, %.2f", b1 - 1.96 * b1_se, b1 + 1.96 * b1_se)
  
  list(int = int, int_ci = int_ci, slope = b1, slope_ci = slope_ci)
}

# Wilson score confidence interval for proportions
wilson_ci <- function(deaths, n, z = 1.96) {
  p <- deaths / n
  denom <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  margin <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denom
  tibble(p = p, lo = center - margin, hi = center + margin)
}


# ============================================================
# SECTION 3: DATA CLEANING - Apply plausible value ranges
# ============================================================
# 
# Exclude patients with physiologically implausible values
# Ranges from SOFA-2 validation study (Supplementary Table 1):
#   - GCS: 3–15
#   - GCS Motor: 1–6
#   - Mean arterial pressure: ≥15, ≤199 mmHg
#   - Platelets: ≥1, ≤999 x10^3/µL
#   - Bilirubin: ≥0.1, ≤46.78 mg/dL
#   - Creatinine: ≥0.10, ≤11.23 mg/dL
#   - Urine output: ≥0, ≤3 ml/kg/h
#   - PaO2/FiO2 ratio: ≥45, <500 mmHg
#   - FiO2: ≥21%, ≤100%
#   - White blood cell: ≥0, ≤99 x10^3/µL
#   - Lymphocytes: ≥0, ≤99 x10^3/µL

# Helper: check if value is within range or missing (missing values pass)
in_range_or_na <- function(x, lo, hi) is.na(x) | (x >= lo & x <= hi)

# [D-01] Range filtering now writes back to sofa2 (was dead code writing to sofa2_t)
# Step 1: Convert impossible zero values to NA
sofa2 <- sofa2 %>%
  mutate(
    gcs_min = ifelse(!is.na(gcs_min) & gcs_min <= 0, NA, gcs_min),
    meanbp_min = ifelse(!is.na(meanbp_min) & meanbp_min <= 0, NA, meanbp_min),
    platelet_min = ifelse(!is.na(platelet_min) & platelet_min <= 0, NA, platelet_min),
    bilirubin_max = ifelse(!is.na(bilirubin_max) & bilirubin_max <= 0, NA, bilirubin_max),
    creatinine_max = ifelse(!is.na(creatinine_max) & creatinine_max <= 0, NA, creatinine_max),
    pao2fio2ratio_novent = ifelse(!is.na(pao2fio2ratio_novent) & pao2fio2ratio_novent <= 0, NA, pao2fio2ratio_novent),
    pao2fio2ratio_vent = ifelse(!is.na(pao2fio2ratio_vent) & pao2fio2ratio_vent <= 0, NA, pao2fio2ratio_vent)
  ) %>%
  # Step 2: Exclude rows with out-of-range values
  filter(
    in_range_or_na(gcs_min, 3, 15),
    in_range_or_na(meanbp_min, 15, 199),
    in_range_or_na(platelet_min, 1, 999),
    in_range_or_na(bilirubin_max, 0.1, 46.78),
    in_range_or_na(creatinine_max, 0.10, 11.23),
    in_range_or_na(uomlkghr_24hr, 0, 3),
    in_range_or_na(pao2fio2ratio_novent, 45, 500),
    in_range_or_na(pao2fio2ratio_vent, 45, 500)
  )

# Step 3: Exclude patients with LOS < 6h
sofa2 <- sofa2 %>%
  dplyr::filter(los >= 0.25)


# ============================================================
# SECTION 4: CREATE ANALYSIS VARIABLES
# ============================================================

sofa2 <- sofa2 %>%
  mutate(
    # ----- Outcome -----
    icu_mort = as.integer(icu_death_flag == 1),
    
    # ----- Sex -----
    sex = case_when(
      gender %in% c("M", "Male", "MALE") ~ "Male",
      gender %in% c("F", "Female", "FEMALE") ~ "Female"
    ),
    
    # ----- Age groups -----
    age_group = case_when(
      is.na(age) ~ "Unknown",
      age >= 18 & age <= 44 ~ "18–44",
      age >= 45 & age <= 64 ~ "45–64",
      age >= 65 & age <= 74 ~ "65–74",
      age >= 75 ~ "≥75"
    ),
    
    # ----- Race/Ethnicity -----
    # Portuguese -> White; AI/AN, NH/PI, Multiple -> Other
    race_group = {
      r <- toupper(trimws(race))
      case_when(
        is.na(r) | r == "" ~ "Unknown",
        str_detect(r, "UNABLE TO OBTAIN|UNKNOWN|PATIENT DECLINED") ~ "Unknown",
        str_detect(r, "PORTUGUESE") ~ "White",
        str_detect(r, "^WHITE") ~ "White",
        str_detect(r, "BLACK") ~ "Black",
        str_detect(r, "HISPAN|SOUTH AMERICAN|LATIN") ~ "Hispanic",
        str_detect(r, "ASIAN") ~ "Asian",
        TRUE ~ "Other"
      )
    },
    
    # ----- Primary language -----
    language_group = case_when(
      is.na(language) | trimws(language) == "" ~ "Unknown",
      toupper(trimws(language)) == "ENGLISH" ~ "English",
      TRUE ~ "Non-English"
    ),
    
    # ----- Insurance status -----
    insurance_group = {
      ins <- toupper(trimws(insurance))
      case_when(
        is.na(ins) | ins == "" ~ "Other",
        str_detect(ins, "MEDICAID") ~ "Medicaid",
        str_detect(ins, "MEDICARE") ~ "Medicare",
        str_detect(ins, "PRIVATE") ~ "Private",
        str_detect(ins, "NO CHARGE") ~ "Other",
        TRUE ~ "Other"
      )
    },
    
    # ----- Rename organ subscore columns for clarity -----
    neuro = cns_24hours,
    cardio = cardiovascular_24hours,
    resp = respiration_24hours,
    hepatic = liver_24hours,
    renal = renal_24hours,
    coag = coagulation_24hours
  )


# ============================================================
# SECTION 5: CREATE ANALYTIC COHORT AND MISSING DATA/LOS ANALYSIS
# ============================================================
# 
# Exclude patients missing:
#   - Total SOFA-2 score (sofa_24hours)
#   - ICU mortality outcome (icu_mort)

sofa2 <- sofa2 %>% filter(!is.na(sofa_24hours), 
                          !is.na(icu_death_flag)
                          )
nrow(sofa2)

# Store total sample size
N_total <- nrow(sofa2)
cat("Analytic cohort size:", N_total, "patients\n")


# Missing Data Table
table(sofa2$missing_GCS_24h) / nrow(sofa2)
table(sofa2$missing_bilirubin_24h) / nrow(sofa2)
table(sofa2$missing_MAP_24h) / nrow(sofa2)
table(sofa2$missing_platelet_24h) / nrow(sofa2)
table(sofa2$missing_creatinine) / nrow(sofa2)
# [D-02] Changed from complete_case_24h (never created) to complete_sofa2
#        which is produced by Data_Processing.R [R-07]
table(sofa2$complete_sofa2) / nrow(sofa2)

missing_summary <- function(x, data) {
  n_missing <- sum(x, na.rm = TRUE)
  pct_missing <- n_missing / nrow(data) * 100
  sprintf("%d (%.1f%%)", n_missing, pct_missing)
}

missing_table <- data.frame(
  Variable = c(
    "GCS (Neurological)",
    "Bilirubin (Hepatic)",
    "Creatinine (Renal)",
    "Mean Arterial Pressure (Cardiovascular)",
    "Platelets (Coagulation)",
    "Complete SOFA-2 Case (24h)"
  ),
  `MIMIC-IV` = c(
    missing_summary(sofa2$missing_GCS_24h, sofa2),
    missing_summary(sofa2$missing_bilirubin_24h, sofa2),
    missing_summary(sofa2$missing_creatinine, sofa2),
    missing_summary(sofa2$missing_MAP_24h, sofa2),
    missing_summary(sofa2$missing_platelet_24h, sofa2),
    # [D-02] complete_sofa2: 1 = all 6 components non-NULL in at least 1 hour
    missing_summary(sofa2$complete_sofa2, sofa2)
  ),
  check.names = FALSE
)
missing_table

# LOS Analysis
demographic_vars <- c("sex", "race_group", "language_group", "insurance_group", "age_group")

sofa2_los <- sofa2 %>%
  group_by(insurance_group) %>% # can choose with demographic variable you want above
  summarize(
    avg_los = mean(los, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Histogram View of SOFA2 Scores
ggplot(sofa2, aes(x = sofa_24hours)) +
  geom_histogram(
    binwidth = 1,          # 1-point SOFA bins (adjust if you prefer)
    closed   = "right",
    color    = "#08306B",  # dark blue outline
    fill     = "#6BAED6",  # mid blue fill
  ) +
  labs(
    x = "Maximum First Day SOFA-2 Score",
    y = "Number of ICU Stays"
  ) +
  theme_minimal(base_size = 12)

# ============================================================
# SECTION 6: TABLE 1 - Baseline Characteristics
# ============================================================

table1_df <- bind_rows(
  # Age
  tibble(Characteristic = "Age categories (years)", Value = ""),
  sofa2 %>% count(age_group) %>%
    mutate(Characteristic = paste0("    ", age_group), Value = fmt_n_pct(n, N_total)) %>%
    select(Characteristic, Value) %>%
    arrange(factor(Characteristic, levels = paste0("    ", c("18–44", "45–64", "65–74", "≥75")))),
  
  # Sex
  tibble(Characteristic = "Sex", Value = ""),
  sofa2 %>% count(sex) %>%
    mutate(Characteristic = paste0("    ", sex), Value = fmt_n_pct(n, N_total)) %>%
    select(Characteristic, Value) %>%
    arrange(factor(Characteristic, levels = paste0("    ", c("Male", "Female")))),
  
  # Race/Ethnicity
  tibble(Characteristic = "Race/Ethnicity", Value = ""),
  sofa2 %>% count(race_group) %>%
    mutate(Characteristic = paste0("    ", race_group), Value = fmt_n_pct(n, N_total)) %>%
    select(Characteristic, Value) %>%
    arrange(factor(Characteristic, levels = paste0("    ", c("White", "Black", "Hispanic", "Asian", "Other", "Unknown")))),
  
  # Primary Language
  tibble(Characteristic = "Primary Language", Value = ""),
  sofa2 %>% count(language_group) %>%
    mutate(Characteristic = paste0("    ", language_group), Value = fmt_n_pct(n, N_total)) %>%
    select(Characteristic, Value) %>%
    arrange(factor(Characteristic, levels = paste0("    ", c("English", "Non-English", "Unknown")))),
  
  # Insurance
  tibble(Characteristic = "Insurance Status", Value = ""),
  sofa2 %>% count(insurance_group) %>%
    mutate(Characteristic = paste0("    ", insurance_group), Value = fmt_n_pct(n, N_total)) %>%
    select(Characteristic, Value) %>%
    arrange(factor(Characteristic, levels = paste0("    ", c("Medicare", "Private", "Medicaid", "Other")))),
  
  # SOFA-2 score and mortality
  tibble(Characteristic = "SOFA-2 Score (Day 1), median (IQR)", Value = fmt_median_iqr(sofa2$sofa_24hours, digits = 0)),
  tibble(Characteristic = "ICU Mortality", Value = fmt_n_pct(sum(sofa2$icu_mort == 1), N_total))
)

# Create formatted table
table1_gt <- table1_df %>%
  gt() %>%
  tab_header(title = "Table 1. Baseline Characteristics of the Study Population") %>%
  cols_label(Value = paste0("MIMIC-IV Patients (N = ", N_total, ")")) %>%
  sub_missing(everything(), missing_text = "")

table1_gt


# ============================================================
# SECTION 7: TABLE 2 - Discrimination and Calibration by Subgroup
# ============================================================

make_table2_df <- function(df, score_col = "sofa_24hours", B = 1000, seed = 1) {
  
  # Fit overall logistic model for predicted probabilities
  cal_model <- glm(reformulate(score_col, "icu_mort"), data = df, family = binomial())
  df <- df %>% mutate(
    p_hat = predict(cal_model, type = "response"),
    lp_hat = safe_logit(p_hat)
  )
  
  # Define metric functions for bootstrapping
  metric_auc <- function(d) auc_ci(d, "icu_mort", score_col)$auc
  metric_br <- function(d) brier_reliability(d$icu_mort, d$p_hat, bins = 10)
  
  # Function to create one block of results (e.g., all age groups)
  make_block <- function(group_var, levels, ref_level, block_title) {
    # Header row
    rows <- list(tibble(
      Subgroup = block_title, N = NA_integer_,
      `ICU Mortality, n (%)` = NA_character_,
      `AUROC (95% CI)` = NA_character_,
      `Δ AUROC (95% CI)` = NA_character_,
      `Brier Reliability` = NA_character_,
      `Δ Brier (95% CI)` = NA_character_,
      `Calibration Intercept (95% CI)` = NA_character_,
      `Calibration Slope (95% CI)` = NA_character_
    ))
    
    # Data rows for each level
    for (lv in levels) {
      d_lv <- df %>% filter(.data[[group_var]] == lv)
      N <- nrow(d_lv)
      mort <- sum(d_lv$icu_mort == 1, na.rm = TRUE)
      
      # AUROC
      a <- auc_ci(d_lv, "icu_mort", score_col)
      auc_str <- if (is.na(a$auc)) NA_character_ else sprintf("%.3f (%.3f–%.3f)", a$auc, a$lo, a$hi)
      
      # Brier reliability
      br <- metric_br(d_lv)
      br_str <- if (is.na(br)) NA_character_ else sprintf("%.4f", br)
      
      # Calibration metrics
      cal <- get_calibration_for_subgroup(d_lv)
      
      # Delta vs reference
      if (lv == ref_level) {
        dA <- "Ref"
        dB <- "Ref"
      } else {
        d_ref <- df %>% filter(.data[[group_var]] == ref_level)
        if (nrow(d_ref) < 50 || N < 50) {
          dA <- NA_character_
          dB <- NA_character_
        } else {
          d_auc <- boot_diff(d_ref, d_lv, metric_auc, B = B, seed = seed)
          d_br <- boot_diff(d_ref, d_lv, metric_br, B = B, seed = seed)
          dA <- sprintf("%.3f (%.3f–%.3f)", d_auc$diff, d_auc$lo, d_auc$hi)
          dB <- sprintf("%.4f (%.4f–%.4f)", d_br$diff, d_br$lo, d_br$hi)
        }
      }
      
      rows <- append(rows, list(tibble(
        Subgroup = paste0("    ", lv),
        N = N,
        `ICU Mortality, n (%)` = if (N == 0) NA_character_ else fmt_n_pct(mort, N),
        `AUROC (95% CI)` = auc_str,
        `Δ AUROC (95% CI)` = dA,
        `Brier Reliability` = br_str,
        `Δ Brier (95% CI)` = dB,
        `Calibration Intercept (95% CI)` = if (is.na(cal$int)) NA_character_ else sprintf("%.2f (%s)", cal$int, cal$int_ci),
        `Calibration Slope (95% CI)` = if (is.na(cal$slope)) NA_character_ else sprintf("%.2f (%s)", cal$slope, cal$slope_ci)
      )))
    }
    bind_rows(rows)
  }
  
  # Overall row
  overall_auc <- auc_ci(df, "icu_mort", score_col)
  overall_br <- brier_reliability(df$icu_mort, df$p_hat, bins = 10)
  overall_cal <- get_calibration_for_subgroup(df)
  
  # Combine all blocks
  bind_rows(
    tibble(
      Subgroup = "Overall",
      N = nrow(df),
      `ICU Mortality, n (%)` = fmt_n_pct(sum(df$icu_mort == 1), nrow(df)),
      `AUROC (95% CI)` = sprintf("%.3f (%.3f–%.3f)", overall_auc$auc, overall_auc$lo, overall_auc$hi),
      `Δ AUROC (95% CI)` = "–",
      `Brier Reliability` = sprintf("%.4f", overall_br),
      `Δ Brier (95% CI)` = "–",
      `Calibration Intercept (95% CI)` = if (is.na(overall_cal$int)) NA_character_ else sprintf("%.2f (%s)", overall_cal$int, overall_cal$int_ci),
      `Calibration Slope (95% CI)` = if (is.na(overall_cal$slope)) NA_character_ else sprintf("%.2f (%s)", overall_cal$slope, overall_cal$slope_ci)
    ),
    make_block("age_group", c("18–44", "45–64", "65–74", "≥75"), "18–44", "Age"),
    make_block("sex", c("Male", "Female"), "Male", "Sex"),
    make_block("race_group", c("White", "Black", "Hispanic", "Asian", "Other", "Unknown"), "White", "Race/Ethnicity"),
    make_block("language_group", c("English", "Non-English", "Unknown"), "English", "Primary Language"),
    make_block("insurance_group", c("Private", "Medicare", "Medicaid", "Other"), "Private", "Insurance Status")
  )
}

# Generate Table 2
table2_df <- make_table2_df(sofa2, score_col = "sofa_24hours", B = 1000, seed = 1)

table2_gt <- table2_df %>%
  gt() %>%
  tab_header(
    title = "Table 2. Discrimination and Calibration of SOFA-2 Score by Subgroup",
    subtitle = "Predicted risk from overall logistic model: ICU mortality ~ SOFA-2"
  ) %>%
  sub_missing(everything(), missing_text = "") %>%
  cols_align(align = "left", columns = Subgroup)

table2_gt


# ============================================================
# SECTION 8: SUPPLEMENTARY TABLE 1A - Organ Subscores (Mean, SD)
# [D-03] NEW SECTION — was missing from original, causing supp1a_gt /
#        supp1a_df to be undefined when referenced in Sections 12 & 13.
# ============================================================

# Helper function for one row of mean/SD values
summ_row_mean_sd <- function(df, label, digits = 1) {
  tibble(
    Subgroup = label,
    N = nrow(df),
    Neuro = fmt_mean_sd(df$neuro, digits),
    Cardio = fmt_mean_sd(df$cardio, digits),
    Respiratory = fmt_mean_sd(df$resp, digits),
    Hepatic = fmt_mean_sd(df$hepatic, digits),
    Renal = fmt_mean_sd(df$renal, digits),
    Coag = fmt_mean_sd(df$coag, digits),
    `Mean SOFA-2` = fmt_mean_sd(df$sofa_24hours, digits)
  )
}

# Build the table
supp1a_df <- bind_rows(
  summ_row_mean_sd(sofa2, "Overall"),
  
  tibble(Subgroup = "Sex", N = NA, Neuro = NA, Cardio = NA, Respiratory = NA, Hepatic = NA, Renal = NA, Coag = NA, `Mean SOFA-2` = NA),
  summ_row_mean_sd(filter(sofa2, sex == "Male"), "    Male"),
  summ_row_mean_sd(filter(sofa2, sex == "Female"), "    Female"),
  
  tibble(Subgroup = "Age", N = NA, Neuro = NA, Cardio = NA, Respiratory = NA, Hepatic = NA, Renal = NA, Coag = NA, `Mean SOFA-2` = NA),
  summ_row_mean_sd(filter(sofa2, age_group == "18–44"), "    18–44"),
  summ_row_mean_sd(filter(sofa2, age_group == "45–64"), "    45–64"),
  summ_row_mean_sd(filter(sofa2, age_group == "65–74"), "    65–74"),
  summ_row_mean_sd(filter(sofa2, age_group == "≥75"), "    ≥75"),
  
  tibble(Subgroup = "Race/Ethnicity", N = NA, Neuro = NA, Cardio = NA, Respiratory = NA, Hepatic = NA, Renal = NA, Coag = NA, `Mean SOFA-2` = NA),
  summ_row_mean_sd(filter(sofa2, race_group == "White"), "    White"),
  summ_row_mean_sd(filter(sofa2, race_group == "Black"), "    Black"),
  summ_row_mean_sd(filter(sofa2, race_group == "Hispanic"), "    Hispanic"),
  summ_row_mean_sd(filter(sofa2, race_group == "Asian"), "    Asian"),
  summ_row_mean_sd(filter(sofa2, race_group == "Other"), "    Other"),
  summ_row_mean_sd(filter(sofa2, race_group == "Unknown"), "    Unknown"),
  
  tibble(Subgroup = "Primary Language", N = NA, Neuro = NA, Cardio = NA, Respiratory = NA, Hepatic = NA, Renal = NA, Coag = NA, `Mean SOFA-2` = NA),
  summ_row_mean_sd(filter(sofa2, language_group == "English"), "    English"),
  summ_row_mean_sd(filter(sofa2, language_group == "Non-English"), "    Non-English"),
  summ_row_mean_sd(filter(sofa2, language_group == "Unknown"), "    Unknown"),
  
  tibble(Subgroup = "Insurance Status", N = NA, Neuro = NA, Cardio = NA, Respiratory = NA, Hepatic = NA, Renal = NA, Coag = NA, `Mean SOFA-2` = NA),
  summ_row_mean_sd(filter(sofa2, insurance_group == "Private"), "    Private"),
  summ_row_mean_sd(filter(sofa2, insurance_group == "Medicare"), "    Medicare"),
  summ_row_mean_sd(filter(sofa2, insurance_group == "Medicaid"), "    Medicaid"),
  summ_row_mean_sd(filter(sofa2, insurance_group == "Other"), "    Other")
)

supp1a_gt <- supp1a_df %>%
  gt() %>%
  tab_header(
    title = "Supplementary Table 1A. SOFA-2 Organ-Specific Subscores by Demographic Subgroup",
    subtitle = "Values are mean (SD)"
  ) %>%
  sub_missing(everything(), missing_text = "")

supp1a_gt


# ============================================================
# SECTION 9: SUPPLEMENTARY TABLE 1B - Organ Subscores (Median, IQR)
# ============================================================

# Helper function for one row of median/IQR values
summ_row_median_iqr <- function(df, label, digits = 0) {
  tibble(
    Subgroup = label,
    N = nrow(df),
    Neuro = fmt_median_iqr(df$neuro, digits),
    Cardio = fmt_median_iqr(df$cardio, digits),
    Respiratory = fmt_median_iqr(df$resp, digits),
    Hepatic = fmt_median_iqr(df$hepatic, digits),
    Renal = fmt_median_iqr(df$renal, digits),
    Coag = fmt_median_iqr(df$coag, digits),
    `Median SOFA-2` = fmt_median_iqr(df$sofa_24hours, digits)
  )
}

# Build the table
supp1b_df <- bind_rows(
  summ_row_median_iqr(sofa2, "Overall"),
  
  tibble(Subgroup = "Sex", N = NA, Neuro = NA, Cardio = NA, Respiratory = NA, Hepatic = NA, Renal = NA, Coag = NA, `Median SOFA-2` = NA),
  summ_row_median_iqr(filter(sofa2, sex == "Male"), "    Male"),
  summ_row_median_iqr(filter(sofa2, sex == "Female"), "    Female"),
  
  tibble(Subgroup = "Age", N = NA, Neuro = NA, Cardio = NA, Respiratory = NA, Hepatic = NA, Renal = NA, Coag = NA, `Median SOFA-2` = NA),
  summ_row_median_iqr(filter(sofa2, age_group == "18–44"), "    18–44"),
  summ_row_median_iqr(filter(sofa2, age_group == "45–64"), "    45–64"),
  summ_row_median_iqr(filter(sofa2, age_group == "65–74"), "    65–74"),
  summ_row_median_iqr(filter(sofa2, age_group == "≥75"), "    ≥75"),
  
  tibble(Subgroup = "Race/Ethnicity", N = NA, Neuro = NA, Cardio = NA, Respiratory = NA, Hepatic = NA, Renal = NA, Coag = NA, `Median SOFA-2` = NA),
  summ_row_median_iqr(filter(sofa2, race_group == "White"), "    White"),
  summ_row_median_iqr(filter(sofa2, race_group == "Black"), "    Black"),
  summ_row_median_iqr(filter(sofa2, race_group == "Hispanic"), "    Hispanic"),
  summ_row_median_iqr(filter(sofa2, race_group == "Asian"), "    Asian"),
  summ_row_median_iqr(filter(sofa2, race_group == "Other"), "    Other"),
  summ_row_median_iqr(filter(sofa2, race_group == "Unknown"), "    Unknown"),
  
  tibble(Subgroup = "Primary Language", N = NA, Neuro = NA, Cardio = NA, Respiratory = NA, Hepatic = NA, Renal = NA, Coag = NA, `Median SOFA-2` = NA),
  summ_row_median_iqr(filter(sofa2, language_group == "English"), "    English"),
  summ_row_median_iqr(filter(sofa2, language_group == "Non-English"), "    Non-English"),
  summ_row_median_iqr(filter(sofa2, language_group == "Unknown"), "    Unknown"),
  
  tibble(Subgroup = "Insurance Status", N = NA, Neuro = NA, Cardio = NA, Respiratory = NA, Hepatic = NA, Renal = NA, Coag = NA, `Median SOFA-2` = NA),
  summ_row_median_iqr(filter(sofa2, insurance_group == "Private"), "    Private"),
  summ_row_median_iqr(filter(sofa2, insurance_group == "Medicare"), "    Medicare"),
  summ_row_median_iqr(filter(sofa2, insurance_group == "Medicaid"), "    Medicaid"),
  summ_row_median_iqr(filter(sofa2, insurance_group == "Other"), "    Other")
)

supp1b_gt <- supp1b_df %>%
  gt() %>%
  tab_header(
    title = "Supplementary Table 1B. SOFA-2 Organ-Specific Subscores by Demographic Subgroup",
    subtitle = "Values are median (IQR)"
  ) %>%
  sub_missing(everything(), missing_text = "")

supp1b_gt


# ============================================================
# SECTION 10: FIGURE 2 - Mortality Bar Charts by SOFA Score
# ============================================================

# ----- Helper: Compute mortality by individual SOFA score -----
compute_mortality_by_score <- function(df, group_var, levels, score_col = "sofa_24hours") {
  df %>%
    filter(.data[[group_var]] %in% levels) %>%
    mutate(
      # Bin scores: 0, 1, 2, ..., 14, 15+
      sofa_bin = case_when(
        .data[[score_col]] >= 15 ~ "15+",
        TRUE ~ as.character(floor(.data[[score_col]]))
      ),
      sofa_bin = factor(sofa_bin, levels = c(as.character(0:14), "15+")),
      group = factor(.data[[group_var]], levels = levels)
    ) %>%
    filter(!is.na(group), !is.na(sofa_bin)) %>%
    group_by(group, sofa_bin, .drop = FALSE) %>%
    summarise(
      n = n(),
      deaths = sum(icu_mort == 1, na.rm = TRUE),
      mortality_pct = 100 * mean(icu_mort, na.rm = TRUE),
      .groups = "drop"
    )
}

# ----- Helper: Create bar chart -----
plot_mortality_barchart <- function(mort_data, title, group_var_name, color_palette, min_n = 10) {
  mort_data %>%
    filter(n >= min_n) %>%
    ggplot(aes(x = sofa_bin, y = mortality_pct, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = color_palette) +
    scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 20)) +
    labs(x = "SOFA-2 Score", y = "Mortality, %", title = title, fill = group_var_name) +
    theme_minimal() +
    theme(legend.position = "bottom", axis.text.x = element_text(size = 9), panel.grid.minor = element_blank())
}

# ----- Define subgroup orderings and colors (blue palette, light to dark) -----

# Age: youngest (lightest) to oldest (darkest)
age_levels <- c("18–44", "45–64", "65–74", "≥75")
age_colors <- c("18–44" = "#C6DBEF", "45–64" = "#6BAED6", "65–74" = "#2171B5", "≥75" = "#08306B")

# Sex: Male (dark), Female (light)
sex_levels <- c("Male", "Female")
sex_colors <- c("Male" = "#2166AC", "Female" = "#92C5DE")

# Race: White (lightest) to Unknown (darkest)
race_levels <- c("White", "Asian", "Hispanic", "Black", "Other", "Unknown")
race_colors <- c("White" = "#C6DBEF", "Asian" = "#9ECAE1", "Hispanic" = "#6BAED6", 
                 "Black" = "#4292C6", "Other" = "#2171B5", "Unknown" = "#084594")

# Language: English (lightest) to Unknown (darkest)
language_levels <- c("English", "Non-English", "Unknown")
language_colors <- c("English" = "#C6DBEF", "Non-English" = "#6BAED6", "Unknown" = "#08519C")

# Insurance: Private (lightest) to Other (darkest)
insurance_levels <- c("Private", "Medicare", "Medicaid", "Other")
insurance_colors <- c("Private" = "#C6DBEF", "Medicare" = "#6BAED6", "Medicaid" = "#2171B5", "Other" = "#08306B")

# ----- Compute mortality data for each demographic -----
mort_age <- compute_mortality_by_score(sofa2, "age_group", age_levels)
mort_sex <- compute_mortality_by_score(sofa2, "sex", sex_levels)
mort_race <- compute_mortality_by_score(sofa2, "race_group", race_levels)
mort_lang <- compute_mortality_by_score(sofa2, "language_group", language_levels)
mort_ins <- compute_mortality_by_score(sofa2, "insurance_group", insurance_levels)

# ----- Create figures -----
fig2a_age <- plot_mortality_barchart(mort_age, "Mortality rate by SOFA-2 score and age", "Age Group", age_colors)
fig2b_sex <- plot_mortality_barchart(mort_sex, "Mortality rate by SOFA-2 score and sex", "Sex", sex_colors)
fig2c_race <- plot_mortality_barchart(mort_race, "", "Race/Ethnicity", race_colors)
fig2d_language <- plot_mortality_barchart(mort_lang, "", "Language", language_colors)
fig2e_insurance <- plot_mortality_barchart(mort_ins, "", "Insurance", insurance_colors)


# ============================================================
# SECTION 11: SUPPLEMENTARY CALIBRATION PLOTS
# ============================================================

# ----- Helper: Bin data by predicted risk and calculate observed mortality -----
calibration_bins <- function(df, group_var, levels, score_col = "sofa_24hours", n_bins = 10, min_n = 20) {
  # Fit overall model
  m <- glm(reformulate(score_col, "icu_mort"), data = df, family = binomial())
  df <- df %>% mutate(p_hat = predict(m, type = "response"))
  
  # Process each subgroup
  purrr::map_dfr(levels, function(lv) {
    d <- df %>% filter(.data[[group_var]] == lv)
    if (nrow(d) == 0) return(NULL)
    
    brks <- unique(quantile(d$p_hat, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE))
    if (length(brks) < 3) return(NULL)
    
    cal <- d %>%
      mutate(bin = cut(p_hat, breaks = brks, include.lowest = TRUE)) %>%
      group_by(bin) %>%
      summarise(n = n(), deaths = sum(icu_mort == 1), p_mean = mean(p_hat), y_mean = mean(icu_mort), .groups = "drop") %>%
      filter(n >= min_n)
    
    if (nrow(cal) == 0) return(NULL)
    
    ci <- wilson_ci(cal$deaths, cal$n)
    bind_cols(cal, ci) %>% mutate(level = lv)
  })
}

# ----- Helper: Plot calibration curve -----
plot_calibration <- function(cal_df, title) {
  ggplot(cal_df, aes(x = p_mean, y = y_mean, color = level, group = level)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(0, 0.5)) +
    labs(x = "Mean predicted risk", y = "Observed ICU mortality", title = title, color = NULL) +
    theme_minimal()
}

# ----- Build calibration plots -----
cal_age <- calibration_bins(sofa2, "age_group", c("18–44", "45–64", "65–74", "≥75"))
cal_sex <- calibration_bins(sofa2, "sex", c("Male", "Female"))
cal_race <- calibration_bins(sofa2, "race_group", c("White", "Black", "Hispanic", "Asian", "Other", "Unknown"))
cal_lang <- calibration_bins(sofa2, "language_group", c("English", "Non-English", "Unknown"))
cal_ins <- calibration_bins(sofa2, "insurance_group", c("Private", "Medicare", "Medicaid", "Other"))

p_cal_age <- plot_calibration(cal_age, "Calibration by Age")
p_cal_sex <- plot_calibration(cal_sex, "Calibration by Sex")
p_cal_race <- plot_calibration(cal_race, "Calibration by Race/Ethnicity")
p_cal_lang <- plot_calibration(cal_lang, "Calibration by Language")
p_cal_ins <- plot_calibration(cal_ins, "Calibration by Insurance")


# ============================================================
# SECTION 12: DISPLAY ALL OUTPUTS
# ============================================================

cat("\n========== TABLE 1 ==========\n")
table1_gt

cat("\n========== TABLE 2 ==========\n")
table2_gt

cat("\n========== SUPPLEMENTARY TABLE 1A (Mean, SD) ==========\n")
supp1a_gt

cat("\n========== SUPPLEMENTARY TABLE 1B (Median, IQR) ==========\n")
supp1b_gt

cat("\n========== FIGURE 2: MORTALITY BAR CHARTS ==========\n")
print(fig2a_age)
print(fig2b_sex)
print(fig2c_race)
print(fig2d_language)
print(fig2e_insurance)

cat("\n========== SUPPLEMENTARY: CALIBRATION PLOTS ==========\n")
print(p_cal_age)
print(p_cal_sex)
print(p_cal_race)
print(p_cal_lang)
print(p_cal_ins)


# ============================================================
# SECTION 13: EXPORT TABLES AS TAB-SEPARATED TEXT
# ============================================================

df_to_tsv <- function(df) {
  out <- df %>% mutate(across(everything(), ~ ifelse(is.na(.x), "", as.character(.x))))
  paste(apply(out, 1, function(row) paste(row, collapse = "\t")), collapse = "\n") |>
    (\(body) paste(paste(names(out), collapse = "\t"), body, sep = "\n"))()
}

cat("\n\n========== TABLE 1 (TSV) ==========\n")
cat(df_to_tsv(table1_df))

cat("\n\n========== TABLE 2 (TSV) ==========\n")
cat(df_to_tsv(table2_df))

cat("\n\n========== SUPPLEMENTARY TABLE 1A (TSV) ==========\n")
cat(df_to_tsv(supp1a_df))

cat("\n\n========== SUPPLEMENTARY TABLE 1B (TSV) ==========\n")
cat(df_to_tsv(supp1b_df))

cat("\n\n========== FIGURE 2A DATA - AGE (TSV) ==========\n")
cat(df_to_tsv(mort_age))

cat("\n\n========== FIGURE 2B DATA - SEX (TSV) ==========\n")
cat(df_to_tsv(mort_sex))

cat("\n\n========== FIGURE 2C DATA - RACE (TSV) ==========\n")
cat(df_to_tsv(mort_race))

cat("\n\n========== FIGURE 2D DATA - LANGUAGE (TSV) ==========\n")
cat(df_to_tsv(mort_lang))

cat("\n\n========== FIGURE 2E DATA - INSURANCE (TSV) ==========\n")
cat(df_to_tsv(mort_ins))
