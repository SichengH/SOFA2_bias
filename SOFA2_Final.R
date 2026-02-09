# ============================================================
# SOFA-2 FAIRNESS ANALYSIS (MIMIC-IV)
# ============================================================
#
# OUTPUTS:
#   - Table 1: Baseline characteristics with SOFA-2, LOS, and Mortality by subgroup
#   - Table 2: Discrimination (AUROC) and Calibration by subgroup
#   - Supplementary Table 1B: Organ subscores by subgroup (median, IQR)
#   - Figure 2A-B: Bar charts of mortality by SOFA score (Age, Sex) with stats
#   - Supplementary Figures 7-9: Bar charts (Race, Language, Insurance) with stats
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
# ============================================================


# ============================================================
# SECTION 1: SETUP - Load packages and data
# ============================================================

# Install and load required packages
pkgs <- c("dplyr", "tidyr", "stringr", "purrr", "tibble", "gt", "pROC", "ggplot2", "scales")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, require, character.only = TRUE))

# Load data
# UPDATE THIS PATH TO YOUR DATA LOCATION
sofa2 <- read.csv("/Users/jellen/downloads/mimic_first_icu (2).csv",
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

# Format p-value for display
format_pval <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) return("P<.001")
  if (p < 0.01) return(sprintf("P=%.3f", p))
  if (p < 0.05) return(sprintf("P=%.2f", p))
  return(sprintf("P=%.2f", p))
}


# ============================================================
# SECTION 3: DATA CLEANING - Apply plausible value ranges
# ============================================================
# 
# Exclude patients with physiologically implausible values
# Ranges from SOFA-2 validation study (eTable 2):
#   - GCS: 3–15
#   - Mean arterial pressure: 15–199 mmHg
#   - Platelets: 1–999 x10^3/µL
#   - Bilirubin: 0.1–46.78 mg/dL
#   - Creatinine: 0.10–11.23 mg/dL
#   - Urine output: 0–3 ml/kg/h
#   - PaO2/FiO2 ratio: 45–500

# Helper: check if value is within range or missing (missing values pass)
in_range_or_na <- function(x, lo, hi) is.na(x) | (x >= lo & x <= hi)

# Convert impossible zero values to NA, then filter by plausible ranges
sofa2 <- sofa2 %>%
  mutate(
    # Zero is impossible for these variables - convert to NA
    gcs_min = ifelse(!is.na(gcs_min) & gcs_min <= 0, NA, gcs_min),
    meanbp_min = ifelse(!is.na(meanbp_min) & meanbp_min <= 0, NA, meanbp_min),
    platelet_min = ifelse(!is.na(platelet_min) & platelet_min <= 0, NA, platelet_min),
    bilirubin_max = ifelse(!is.na(bilirubin_max) & bilirubin_max <= 0, NA, bilirubin_max),
    creatinine_max = ifelse(!is.na(creatinine_max) & creatinine_max <= 0, NA, creatinine_max),
    pao2fio2ratio_novent = ifelse(!is.na(pao2fio2ratio_novent) & pao2fio2ratio_novent <= 0, NA, pao2fio2ratio_novent),
    pao2fio2ratio_vent = ifelse(!is.na(pao2fio2ratio_vent) & pao2fio2ratio_vent <= 0, NA, pao2fio2ratio_vent)
  ) %>%
  # Exclude rows with out-of-range values
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
# SECTION 5: CREATE ANALYTIC COHORT
# ============================================================

sofa2 <- sofa2 %>% filter(!is.na(sofa_24hours), 
                          !is.na(icu_death_flag))
nrow(sofa2)

# Store total sample size
N_total <- nrow(sofa2)
cat("Analytic cohort size:", N_total, "patients\n")


# Missing Data Table
missing_summary <- function(x, data) {
  n_missing <- sum(x, na.rm = TRUE)
  pct_missing <- n_missing / nrow(data) * 100
  sprintf("%d (%.1f%%)", n_missing, pct_missing)
}

# Check if missing indicator columns exist before creating table
if (all(c("missing_GCS_24h", "missing_bilirubin_24h", "missing_MAP_24h", 
          "missing_platelet_24h", "missing_creatinine", "complete_case_24h") %in% names(sofa2))) {
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
      missing_summary(sofa2$complete_case_24h, sofa2)
    ),
    check.names = FALSE
  )
  print(missing_table)
}

# Histogram View of SOFA2 Scores
ggplot(sofa2, aes(x = sofa_24hours)) +
  geom_histogram(
    binwidth = 1,
    closed   = "right",
    color    = "#08306B",
    fill     = "#6BAED6",
  ) +
  labs(
    x = "Maximum First Day SOFA-2 Score",
    y = "Number of ICU Stays"
  ) +
  theme_minimal(base_size = 12)


# ============================================================
# SECTION 6: TABLE 1 - Baseline Characteristics (MODIFIED)
# ============================================================
# 
# NEW: Table 1 now shows N (%), Median SOFA-2 (IQR), Median LOS (IQR), 
# and ICU Mortality n(%) for each demographic subgroup level

# Helper function to compute summary statistics for a subgroup
compute_subgroup_stats <- function(df, label, indent = TRUE) {
  n <- nrow(df)
  deaths <- sum(df$icu_mort == 1, na.rm = TRUE)
  
  tibble(
    Characteristic = if(indent) paste0("    ", label) else label,
    `N (%)` = fmt_n_pct(n, N_total),
    `Median SOFA-2 (IQR)` = fmt_median_iqr(df$sofa_24hours, digits = 0),
    `Median LOS, days (IQR)` = fmt_median_iqr(df$los, digits = 1),
    `ICU Mortality, n (%)` = fmt_n_pct(deaths, n)
  )
}

# Build Table 1 with all columns
table1_expanded <- bind_rows(
  # Overall
  tibble(
    Characteristic = "Overall",
    `N (%)` = as.character(N_total),
    `Median SOFA-2 (IQR)` = fmt_median_iqr(sofa2$sofa_24hours, digits = 0),
    `Median LOS, days (IQR)` = fmt_median_iqr(sofa2$los, digits = 1),
    `ICU Mortality, n (%)` = fmt_n_pct(sum(sofa2$icu_mort == 1), N_total)
  ),
  
  # Age header
  tibble(Characteristic = "Age categories (years)", `N (%)` = "", 
         `Median SOFA-2 (IQR)` = "", `Median LOS, days (IQR)` = "", `ICU Mortality, n (%)` = ""),
  compute_subgroup_stats(filter(sofa2, age_group == "18–44"), "18–44"),
  compute_subgroup_stats(filter(sofa2, age_group == "45–64"), "45–64"),
  compute_subgroup_stats(filter(sofa2, age_group == "65–74"), "65–74"),
  compute_subgroup_stats(filter(sofa2, age_group == "≥75"), "≥75"),
  
  # Sex header
  tibble(Characteristic = "Sex", `N (%)` = "", 
         `Median SOFA-2 (IQR)` = "", `Median LOS, days (IQR)` = "", `ICU Mortality, n (%)` = ""),
  compute_subgroup_stats(filter(sofa2, sex == "Male"), "Male"),
  compute_subgroup_stats(filter(sofa2, sex == "Female"), "Female"),
  
  # Race/Ethnicity header
  tibble(Characteristic = "Race/Ethnicity", `N (%)` = "", 
         `Median SOFA-2 (IQR)` = "", `Median LOS, days (IQR)` = "", `ICU Mortality, n (%)` = ""),
  compute_subgroup_stats(filter(sofa2, race_group == "White"), "White"),
  compute_subgroup_stats(filter(sofa2, race_group == "Black"), "Black"),
  compute_subgroup_stats(filter(sofa2, race_group == "Hispanic"), "Hispanic"),
  compute_subgroup_stats(filter(sofa2, race_group == "Asian"), "Asian"),
  compute_subgroup_stats(filter(sofa2, race_group == "Other"), "Other"),
  compute_subgroup_stats(filter(sofa2, race_group == "Unknown"), "Unknown"),
  
  # Primary Language header
  tibble(Characteristic = "Primary Language", `N (%)` = "", 
         `Median SOFA-2 (IQR)` = "", `Median LOS, days (IQR)` = "", `ICU Mortality, n (%)` = ""),
  compute_subgroup_stats(filter(sofa2, language_group == "English"), "English"),
  compute_subgroup_stats(filter(sofa2, language_group == "Non-English"), "Non-English"),
  compute_subgroup_stats(filter(sofa2, language_group == "Unknown"), "Unknown"),
  
  # Insurance header
  tibble(Characteristic = "Insurance Status", `N (%)` = "", 
         `Median SOFA-2 (IQR)` = "", `Median LOS, days (IQR)` = "", `ICU Mortality, n (%)` = ""),
  compute_subgroup_stats(filter(sofa2, insurance_group == "Medicare"), "Medicare"),
  compute_subgroup_stats(filter(sofa2, insurance_group == "Private"), "Private"),
  compute_subgroup_stats(filter(sofa2, insurance_group == "Medicaid"), "Medicaid"),
  compute_subgroup_stats(filter(sofa2, insurance_group == "Other"), "Other")
)

# Create formatted gt table
table1_gt <- table1_expanded %>%
  gt() %>%
  tab_header(title = "Table 1. Baseline Characteristics of the Study Population") %>%
  sub_missing(everything(), missing_text = "") %>%
  cols_align(align = "left", columns = Characteristic) %>%
  cols_align(align = "center", columns = -Characteristic)

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
# SECTION 8: SUPPLEMENTARY TABLE - Organ Subscores (Median, IQR)
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
    title = "Supplementary Table 3. SOFA-2 Organ-Specific Subscores by Demographic Subgroup",
    subtitle = "Values are median (IQR)"
  ) %>%
  sub_missing(everything(), missing_text = "")

supp1b_gt


# ============================================================
# SECTION 9: FIGURE 2 - Mortality Bar Charts with Error Bars and Stats
# ============================================================
# 
# MODIFIED: Now includes Wilson 95% CI error bars and chi-square
# statistical testing with p-value annotations

# ----- Helper: Compute mortality by individual SOFA score with Wilson CI -----
compute_mortality_by_score_with_ci <- function(df, group_var, levels, score_col = "sofa_24hours") {
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
      .groups = "drop"
    ) %>%
    # Add Wilson CI
    mutate(
      mortality_pct = 100 * deaths / n,
      # Wilson CI (in percentage)
      ci_data = pmap(list(deaths, n), function(d, n_obs) {
        if (n_obs < 1) return(tibble(lo = NA_real_, hi = NA_real_))
        wilson_ci(d, n_obs) %>% mutate(lo = lo * 100, hi = hi * 100)
      })
    ) %>%
    unnest(ci_data, names_sep = "_") %>%
    rename(ci_lo = ci_data_lo, ci_hi = ci_data_hi) %>%
    select(-ci_data_p)
}

# ----- Helper: Compute chi-square p-values comparing groups at each SOFA score -----
# This compares each group to the reference group (first level) at each SOFA bin
compute_pairwise_chisq <- function(df, group_var, levels, ref_level, score_col = "sofa_24hours", min_n = 10) {
  
  sofa_bins <- c(as.character(0:14), "15+")
  
  # Prepare data with binned SOFA scores
  df_binned <- df %>%
    filter(.data[[group_var]] %in% levels) %>%
    mutate(
      sofa_bin = case_when(
        .data[[score_col]] >= 15 ~ "15+",
        TRUE ~ as.character(floor(.data[[score_col]]))
      ),
      sofa_bin = factor(sofa_bin, levels = sofa_bins),
      group = factor(.data[[group_var]], levels = levels)
    ) %>%
    filter(!is.na(group), !is.na(sofa_bin))
  
  # Compute p-values for each bin comparing each group to reference
  results <- expand.grid(
    sofa_bin = sofa_bins,
    group = setdiff(levels, ref_level),
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    rowwise() %>%
    mutate(
      pval = {
        d_ref <- df_binned %>% filter(group == ref_level, sofa_bin == .data$sofa_bin)
        d_cmp <- df_binned %>% filter(group == .data$group, sofa_bin == .data$sofa_bin)
        
        # Need sufficient data in both groups
        if (nrow(d_ref) < min_n || nrow(d_cmp) < min_n) {
          NA_real_
        } else {
          # Build 2x2 contingency table
          tbl <- matrix(c(
            sum(d_ref$icu_mort == 1), sum(d_ref$icu_mort == 0),
            sum(d_cmp$icu_mort == 1), sum(d_cmp$icu_mort == 0)
          ), nrow = 2, byrow = TRUE)
          
          # Chi-square test (with continuity correction for small samples)
          test_result <- tryCatch(
            chisq.test(tbl, correct = TRUE),
            error = function(e) list(p.value = NA_real_)
          )
          test_result$p.value
        }
      }
    ) %>%
    ungroup() %>%
    mutate(
      sofa_bin = factor(sofa_bin, levels = sofa_bins),
      significant = !is.na(pval) & pval < 0.05,
      pval_label = sapply(pval, format_pval)
    )
  
  results
}

# ----- Helper: Compute overall chi-square across all groups at each SOFA score -----
# This tests whether mortality differs across ALL groups at each SOFA bin
compute_overall_chisq <- function(df, group_var, levels, score_col = "sofa_24hours", min_n = 10) {
  
  sofa_bins <- c(as.character(0:14), "15+")
  
  # Prepare data with binned SOFA scores
  df_binned <- df %>%
    filter(.data[[group_var]] %in% levels) %>%
    mutate(
      sofa_bin = case_when(
        .data[[score_col]] >= 15 ~ "15+",
        TRUE ~ as.character(floor(.data[[score_col]]))
      ),
      sofa_bin = factor(sofa_bin, levels = sofa_bins),
      group = factor(.data[[group_var]], levels = levels)
    ) %>%
    filter(!is.na(group), !is.na(sofa_bin))
  
  # Compute overall chi-square p-value for each SOFA bin
  results <- tibble(sofa_bin = sofa_bins) %>%
    rowwise() %>%
    mutate(
      pval = {
        d <- df_binned %>% filter(sofa_bin == .data$sofa_bin)
        
        # Need sufficient data
        if (nrow(d) < min_n * length(levels)) {
          NA_real_
        } else {
          # Build contingency table: groups x outcome
          tbl <- table(d$group, d$icu_mort)
          
          # Remove groups with zero observations
          tbl <- tbl[rowSums(tbl) > 0, , drop = FALSE]
          
          if (nrow(tbl) < 2 || ncol(tbl) < 2) {
            NA_real_
          } else {
            test_result <- tryCatch(
              chisq.test(tbl),
              error = function(e) list(p.value = NA_real_)
            )
            test_result$p.value
          }
        }
      }
    ) %>%
    ungroup() %>%
    mutate(
      sofa_bin = factor(sofa_bin, levels = sofa_bins),
      significant = !is.na(pval) & pval < 0.05,
      pval_label = sapply(pval, format_pval)
    )
  
  results
}

# ----- Helper: Create bar chart with error bars and p-value annotations -----
plot_mortality_barchart_with_stats <- function(mort_data, pval_data, title, group_var_name, 
                                               color_palette, min_n = 10, ref_group = NULL,
                                               use_overall_pval = FALSE) {
  
  # Filter out small sample sizes
  plot_data <- mort_data %>% filter(n >= min_n)
  
  # Determine y-position for p-value annotations (above highest bar + CI)
  pval_positions <- plot_data %>%
    group_by(sofa_bin) %>%
    summarise(
      max_y = max(ci_hi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(label_y = max_y + 3) # Position label 3% above highest CI
  
  # Merge p-value data with positions
  if (use_overall_pval) {
    pval_plot <- pval_data %>%
      filter(significant) %>%
      left_join(pval_positions, by = "sofa_bin")
  } else {
    # For pairwise comparisons, we need to handle multiple comparisons
    # Show only significant p-values
    pval_plot <- pval_data %>%
      filter(significant) %>%
      left_join(pval_positions, by = "sofa_bin") %>%
      # If multiple significant comparisons at same SOFA, take the smallest p-value
      group_by(sofa_bin) %>%
      slice_min(pval, n = 1) %>%
      ungroup()
  }
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = sofa_bin, y = mortality_pct, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(
      aes(ymin = ci_lo, ymax = ci_hi),
      position = position_dodge(width = 0.8),
      width = 0.25,
      linewidth = 0.4,
      color = "black"
    ) +
    scale_fill_manual(values = color_palette) +
    scale_y_continuous(limits = c(0, 85), breaks = seq(0, 80, by = 20)) +
    labs(
      x = "SOFA-2 Score", 
      y = "Mortality, %", 
      title = title, 
      fill = group_var_name
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 9),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 12, face = "bold")
    )
  
  # Add p-value annotations if there are significant results
  if (nrow(pval_plot) > 0) {
    p <- p +
      geom_text(
        data = pval_plot,
        aes(x = sofa_bin, y = label_y, label = pval_label),
        inherit.aes = FALSE,
        size = 2.5,
        vjust = 0,
        fontface = "italic"
      )
  }
  
  p
}

# ----- Define subgroup orderings and colors (blue palette, light to dark) -----

# Age: youngest (lightest) to oldest (darkest)
age_levels <- c("18–44", "45–64", "65–74", "≥75")
age_colors <- c("18–44" = "#C6DBEF", "45–64" = "#6BAED6", "65–74" = "#2171B5", "≥75" = "#08306B")

# Sex: Male (dark), Female (light)
sex_levels <- c("Male", "Female")
sex_colors <- c("Male" = "#2166AC", "Female" = "#92C5DE")

# Race: White (lightest) to Unknown (darkest)
race_levels <- c("White", "Black", "Hispanic", "Asian", "Other", "Unknown")
race_colors <- c("White" = "#C6DBEF", "Asian" = "#9ECAE1", "Hispanic" = "#6BAED6", 
                 "Black" = "#4292C6", "Other" = "#2171B5", "Unknown" = "#084594")

# Language: English (lightest) to Unknown (darkest)
language_levels <- c("English", "Non-English", "Unknown")
language_colors <- c("English" = "#C6DBEF", "Non-English" = "#6BAED6", "Unknown" = "#08519C")

# Insurance: Private (lightest) to Other (darkest)
insurance_levels <- c("Private", "Medicare", "Medicaid", "Other")
insurance_colors <- c("Private" = "#C6DBEF", "Medicare" = "#6BAED6", "Medicaid" = "#2171B5", "Other" = "#08306B")

# ----- Compute mortality data with Wilson CI for each demographic -----
mort_age <- compute_mortality_by_score_with_ci(sofa2, "age_group", age_levels)
mort_sex <- compute_mortality_by_score_with_ci(sofa2, "sex", sex_levels)
mort_race <- compute_mortality_by_score_with_ci(sofa2, "race_group", race_levels)
mort_lang <- compute_mortality_by_score_with_ci(sofa2, "language_group", language_levels)
mort_ins <- compute_mortality_by_score_with_ci(sofa2, "insurance_group", insurance_levels)

# ----- Compute statistical tests -----
# For Age: use overall chi-square (comparing all 4 age groups)
pval_age <- compute_overall_chisq(sofa2, "age_group", age_levels)

# For Sex: pairwise comparison (Male vs Female)
pval_sex <- compute_pairwise_chisq(sofa2, "sex", sex_levels, ref_level = "Male")

# For Race: pairwise comparisons vs White (reference)
pval_race <- compute_pairwise_chisq(sofa2, "race_group", race_levels, ref_level = "White")

# For Language: pairwise comparison vs English
pval_lang <- compute_pairwise_chisq(sofa2, "language_group", language_levels, ref_level = "English")

# For Insurance: pairwise comparisons vs Private (reference)
pval_ins <- compute_pairwise_chisq(sofa2, "insurance_group", insurance_levels, ref_level = "Private")

# ----- Create Figure 2A: Age (uses overall chi-square) -----
fig2a_age <- plot_mortality_barchart_with_stats(
  mort_data = mort_age,
  pval_data = pval_age,
  title = "A. Observed ICU Mortality by SOFA-2 Score and Age Group",
  group_var_name = "Age Group",
  color_palette = age_colors,
  use_overall_pval = TRUE
)

# ----- Create Figure 2B: Sex (uses pairwise chi-square) -----
fig2b_sex <- plot_mortality_barchart_with_stats(
  mort_data = mort_sex,
  pval_data = pval_sex,
  title = "B. Observed ICU Mortality by SOFA-2 Score and Sex",
  group_var_name = "Sex",
  color_palette = sex_colors,
  ref_group = "Male"
)

# ----- Create Supplementary Figure 7: Race/Ethnicity -----
# Use overall chi-square for multi-group comparison
pval_race_overall <- compute_overall_chisq(sofa2, "race_group", race_levels)

supp_fig7_race <- plot_mortality_barchart_with_stats(
  mort_data = mort_race,
  pval_data = pval_race_overall,
  title = "Observed ICU Mortality by SOFA-2 Score and Race/Ethnicity",
  group_var_name = "Race/Ethnicity",
  color_palette = race_colors,
  use_overall_pval = TRUE
)

# ----- Create Supplementary Figure 8: Language -----
pval_lang_overall <- compute_overall_chisq(sofa2, "language_group", language_levels)

supp_fig8_language <- plot_mortality_barchart_with_stats(
  mort_data = mort_lang,
  pval_data = pval_lang_overall,
  title = "Observed ICU Mortality by SOFA-2 Score and Primary Language",
  group_var_name = "Language",
  color_palette = language_colors,
  use_overall_pval = TRUE
)

# ----- Create Supplementary Figure 9: Insurance -----
pval_ins_overall <- compute_overall_chisq(sofa2, "insurance_group", insurance_levels)

supp_fig9_insurance <- plot_mortality_barchart_with_stats(
  mort_data = mort_ins,
  pval_data = pval_ins_overall,
  title = "Observed ICU Mortality by SOFA-2 Score and Insurance Status",
  group_var_name = "Insurance",
  color_palette = insurance_colors,
  use_overall_pval = TRUE
)


# ============================================================
# SECTION 10: ALTERNATIVE FIGURE - Race Pairwise (like example image)
# ============================================================
# 
# This creates a figure similar to the Miller et al. example showing
# only Black vs White with pairwise p-values annotated

# Create Black vs White only comparison (like example image)
race_bw_levels <- c("White", "Black")
race_bw_colors <- c("White" = "#9ECAE1", "Black" = "#08519C")

mort_race_bw <- compute_mortality_by_score_with_ci(sofa2, "race_group", race_bw_levels)
pval_race_bw <- compute_pairwise_chisq(sofa2, "race_group", race_bw_levels, ref_level = "White")

# Custom plot for Black vs White comparison with pairwise p-values
fig_race_bw <- plot_mortality_barchart_with_stats(
  mort_data = mort_race_bw,
  pval_data = pval_race_bw,
  title = "Mortality by SOFA-2 Score: Black vs White",
  group_var_name = "Race",
  color_palette = race_bw_colors,
  ref_group = "White"
)


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
    labs(x = "Mean predicted risk (logistic regression model)", y = "Observed ICU mortality", title = title, color = NULL) +
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
# SECTION 12: STATISTICAL TEST SUMMARY TABLES
# ============================================================
# 
# Create summary tables of chi-square test results for each demographic

# Helper to create a summary of p-values by SOFA score
create_pval_summary <- function(pval_data, group_var_name, is_overall = FALSE) {
  if (is_overall) {
    pval_data %>%
      select(sofa_bin, pval, significant, pval_label) %>%
      mutate(
        across(everything(), as.character)
      ) %>%
      rename(
        `SOFA-2 Score` = sofa_bin,
        `P-value` = pval_label,
        `Significant (P<0.05)` = significant
      )
  } else {
    pval_data %>%
      select(sofa_bin, group, pval, significant, pval_label) %>%
      mutate(
        across(everything(), as.character)
      ) %>%
      rename(
        `SOFA-2 Score` = sofa_bin,
        `Comparison Group` = group,
        `P-value` = pval_label,
        `Significant (P<0.05)` = significant
      )
  }
}

# Create summary tables
pval_summary_age <- create_pval_summary(pval_age, "Age", is_overall = TRUE)
pval_summary_sex <- create_pval_summary(pval_sex, "Sex")
pval_summary_race <- create_pval_summary(pval_race, "Race/Ethnicity")
pval_summary_lang <- create_pval_summary(pval_lang, "Language")
pval_summary_ins <- create_pval_summary(pval_ins, "Insurance")


# ============================================================
# SECTION 13: DISPLAY ALL OUTPUTS
# ============================================================

cat("\n========== TABLE 1 (EXPANDED) ==========\n")
print(table1_gt)

cat("\n========== TABLE 2 ==========\n")
print(table2_gt)

cat("\n========== SUPPLEMENTARY TABLE 3 (Organ Subscores - Median, IQR) ==========\n")
print(supp1b_gt)

cat("\n========== FIGURE 2: MORTALITY BAR CHARTS WITH ERROR BARS AND P-VALUES ==========\n")
print(fig2a_age)
print(fig2b_sex)

cat("\n========== SUPPLEMENTARY FIGURES: MORTALITY BY DEMOGRAPHICS ==========\n")
print(supp_fig7_race)
print(supp_fig8_language)
print(supp_fig9_insurance)

cat("\n========== ALTERNATIVE: BLACK VS WHITE COMPARISON (Like Example) ==========\n")
print(fig_race_bw)

cat("\n========== SUPPLEMENTARY: CALIBRATION PLOTS ==========\n")
print(p_cal_age)
print(p_cal_sex)
print(p_cal_race)
print(p_cal_lang)
print(p_cal_ins)

cat("\n========== STATISTICAL TEST SUMMARIES ==========\n")
cat("\n----- Age (Overall Chi-Square) -----\n")
print(pval_summary_age)
cat("\n----- Sex (Pairwise vs Male) -----\n")
print(pval_summary_sex)
cat("\n----- Race (Pairwise vs White) -----\n")
print(pval_summary_race)
cat("\n----- Language (Pairwise vs English) -----\n")
print(pval_summary_lang)
cat("\n----- Insurance (Pairwise vs Private) -----\n")
print(pval_summary_ins)


# ============================================================
# SECTION 14: EXPORT TABLES AS TAB-SEPARATED TEXT
# ============================================================

df_to_tsv <- function(df) {
  out <- df %>% mutate(across(everything(), ~ ifelse(is.na(.x), "", as.character(.x))))
  paste(apply(out, 1, function(row) paste(row, collapse = "\t")), collapse = "\n") |>
    (\(body) paste(paste(names(out), collapse = "\t"), body, sep = "\n"))()
}

cat("\n\n========== TABLE 1 EXPANDED (TSV) ==========\n")
cat(df_to_tsv(table1_expanded))

cat("\n\n========== TABLE 2 (TSV) ==========\n")
cat(df_to_tsv(table2_df))

cat("\n\n========== SUPPLEMENTARY TABLE 3 (TSV) ==========\n")
cat(df_to_tsv(supp1b_df))

cat("\n\n========== FIGURE 2A DATA - AGE WITH CI (TSV) ==========\n")
cat(df_to_tsv(mort_age))

cat("\n\n========== FIGURE 2B DATA - SEX WITH CI (TSV) ==========\n")
cat(df_to_tsv(mort_sex))

cat("\n\n========== SUPP FIG 7 DATA - RACE WITH CI (TSV) ==========\n")
cat(df_to_tsv(mort_race))

cat("\n\n========== SUPP FIG 8 DATA - LANGUAGE WITH CI (TSV) ==========\n")
cat(df_to_tsv(mort_lang))

cat("\n\n========== SUPP FIG 9 DATA - INSURANCE WITH CI (TSV) ==========\n")
cat(df_to_tsv(mort_ins))

cat("\n\n========== P-VALUE SUMMARY - AGE (TSV) ==========\n")
cat(df_to_tsv(pval_summary_age))

cat("\n\n========== P-VALUE SUMMARY - SEX (TSV) ==========\n")
cat(df_to_tsv(pval_summary_sex))

cat("\n\n========== P-VALUE SUMMARY - RACE (TSV) ==========\n")
cat(df_to_tsv(pval_summary_race))

cat("\n\n========== P-VALUE SUMMARY - LANGUAGE (TSV) ==========\n")
cat(df_to_tsv(pval_summary_lang))

cat("\n\n========== P-VALUE SUMMARY - INSURANCE (TSV) ==========\n")
cat(df_to_tsv(pval_summary_ins))


# ============================================================
# SECTION 15: SAVE FIGURES (Optional)
# ============================================================
# 
# Uncomment the following lines to save figures to files

# ggsave("fig2a_mortality_age.pdf", fig2a_age, width = 10, height = 6)
# ggsave("fig2b_mortality_sex.pdf", fig2b_sex, width = 10, height = 6)
# ggsave("supp_fig7_mortality_race.pdf", supp_fig7_race, width = 12, height = 6)
# ggsave("supp_fig8_mortality_language.pdf", supp_fig8_language, width = 10, height = 6)
# ggsave("supp_fig9_mortality_insurance.pdf", supp_fig9_insurance, width = 10, height = 6)
# ggsave("fig_race_black_vs_white.pdf", fig_race_bw, width = 10, height = 6)

cat("\n\n========== ANALYSIS COMPLETE ==========\n")
cat("Total patients analyzed:", N_total, "\n")
cat("ICU deaths:", sum(sofa2$icu_mort == 1), "(", round(100 * mean(sofa2$icu_mort), 1), "%)\n")
