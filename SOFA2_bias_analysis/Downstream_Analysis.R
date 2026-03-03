# SOFA-2 Fairness Analysis — Downstream Analysis
# 
# Evaluates discrimination and calibration of the SOFA-2 score for ICU
# mortality prediction across demographic subgroups using MIMIC-IV.
#
# Input:  mimic_first_icu_final.csv (from Data_Processing.R)
#
# Outputs (mapped to manuscript):
#   Table 1      — Baseline characteristics by subgroup
#   Table 2      — Discrimination (AUROC) and calibration by subgroup
#   eTable 2     — Component-level missingness (first 24 hours)
#   eTable 3     — Median organ subscores by subgroup
#   eFigure 1    — SOFA-2 score distribution histogram
#   Figure 2     — ICU mortality by SOFA-2 score, stratified by age and sex
#   eFigures 2–6 — Calibration plots by subgroup
#   eFigures 7–9 — ICU mortality by SOFA-2 score (race, language, insurance)
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Setup
# -----------------------------------------------------------------------------

pkgs <- c("dplyr", "tidyr", "stringr", "purrr", "tibble", "gt", "pROC", "ggplot2")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, require, character.only = TRUE))

DATA_PATH <- "/Users/jellen/dropbox/mimic_first_icu_final.csv"

sofa2 <- read.csv(
  DATA_PATH,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "NaN")
)


# -----------------------------------------------------------------------------
# 2. Helper functions
# -----------------------------------------------------------------------------

# "n (x.x%)"
fmt_n_pct <- function(n, N) {
  sprintf("%s (%.1f%%)", format(n, big.mark = ","), 100 * n / N)
}

# "mean (SD)"
fmt_mean_sd <- function(x, digits = 1) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"), mean(x), sd(x))
}

# "median (Q1–Q3)"
fmt_median_iqr <- function(x, digits = 0) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  q <- quantile(x, probs = c(0.25, 0.5, 0.75), names = FALSE)
  sprintf(paste0("%.", digits, "f (%.", digits, "f\u2013%.", digits, "f)"),
          q[2], q[1], q[3])
}

# Logit with clamping to avoid Inf
safe_logit <- function(p, eps = 1e-6) {
  qlogis(pmin(pmax(p, eps), 1 - eps))
}

# Brier reliability component (calibration metric; lower = better)
brier_reliability <- function(y, p, bins = 10) {
  df <- tibble(y = y, p = p) %>% filter(!is.na(y), !is.na(p))
  if (nrow(df) == 0) return(NA_real_)
  brks <- unique(quantile(df$p, probs = seq(0, 1, length.out = bins + 1),
                          na.rm = TRUE))
  if (length(brks) < 3) return(0)
  df <- df %>% mutate(bin = cut(p, breaks = brks, include.lowest = TRUE))
  summ <- df %>%
    group_by(bin) %>%
    summarise(n = n(), pbar = mean(p), ybar = mean(y), .groups = "drop")
  N <- sum(summ$n)
  sum((summ$n / N) * (summ$pbar - summ$ybar)^2)
}

# Bootstrap CI for difference between two groups on a given metric
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

# AUROC with DeLong 95% CI
auc_ci <- function(df, outcome_col, score_col) {
  df <- df %>% filter(!is.na(.data[[outcome_col]]), !is.na(.data[[score_col]]))
  if (nrow(df) < 10 || length(unique(df[[outcome_col]])) < 2) {
    return(list(auc = NA_real_, lo = NA_real_, hi = NA_real_))
  }
  r  <- pROC::roc(response = df[[outcome_col]], predictor = df[[score_col]],
                  quiet = TRUE, direction = "<")
  ci <- as.numeric(pROC::ci.auc(r))
  list(auc = as.numeric(pROC::auc(r)), lo = ci[1], hi = ci[3])
}

# Calibration intercept (offset model) and slope for a subgroup
get_calibration_for_subgroup <- function(d, min_n = 50) {
  if (nrow(d) < min_n || length(unique(d$icu_mort)) < 2) {
    return(list(int = NA_real_, int_ci = NA_character_,
                slope = NA_real_, slope_ci = NA_character_))
  }
  # Intercept: logistic regression with linear predictor as offset
  m_int <- tryCatch(
    glm(icu_mort ~ 1 + offset(lp_hat), data = d, family = binomial()),
    error = function(e) NULL)
  if (is.null(m_int)) {
    return(list(int = NA_real_, int_ci = NA_character_,
                slope = NA_real_, slope_ci = NA_character_))
  }
  int    <- coef(m_int)[1]
  int_se <- sqrt(vcov(m_int)[1, 1])
  int_ci <- sprintf("%.2f, %.2f", int - 1.96 * int_se, int + 1.96 * int_se)
  
  # Slope: logistic regression with linear predictor as covariate
  m_slope <- tryCatch(
    glm(icu_mort ~ lp_hat, data = d, family = binomial()),
    error = function(e) NULL)
  if (is.null(m_slope)) {
    return(list(int = int, int_ci = int_ci,
                slope = NA_real_, slope_ci = NA_character_))
  }
  b1    <- coef(m_slope)[2]
  b1_se <- sqrt(diag(vcov(m_slope)))[2]
  slope_ci <- sprintf("%.2f, %.2f", b1 - 1.96 * b1_se, b1 + 1.96 * b1_se)
  
  list(int = int, int_ci = int_ci, slope = b1, slope_ci = slope_ci)
}

# Wilson score CI for binomial proportions
wilson_ci <- function(deaths, n, z = 1.96) {
  p      <- deaths / n
  denom  <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  margin <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denom
  tibble(p = p, lo = center - margin, hi = center + margin)
}


# -----------------------------------------------------------------------------
# 3. Data cleaning — plausibility filters
# -----------------------------------------------------------------------------

# Helper: value is within [lo, hi] or is missing (NA passes through)
in_range_or_na <- function(x, lo, hi) is.na(x) | (x >= lo & x <= hi)

# Convert impossible zero values to NA for variables with positive-only ranges
sofa2 <- sofa2 %>%
  mutate(
    gcs_lo        = ifelse(!is.na(gcs_lo)        & gcs_lo        <= 0, NA, gcs_lo),
    gcs_hi        = ifelse(!is.na(gcs_hi)        & gcs_hi        <= 0, NA, gcs_hi),
    gcs_motor_lo  = ifelse(!is.na(gcs_motor_lo)  & gcs_motor_lo  <= 0, NA, gcs_motor_lo),
    gcs_motor_hi  = ifelse(!is.na(gcs_motor_hi)  & gcs_motor_hi  <= 0, NA, gcs_motor_hi),
    meanbp_lo     = ifelse(!is.na(meanbp_lo)     & meanbp_lo     <= 0, NA, meanbp_lo),
    platelet_lo   = ifelse(!is.na(platelet_lo)   & platelet_lo   <= 0, NA, platelet_lo),
    bilirubin_lo  = ifelse(!is.na(bilirubin_lo)  & bilirubin_lo  <= 0, NA, bilirubin_lo),
    creatinine_lo = ifelse(!is.na(creatinine_lo) & creatinine_lo <= 0, NA, creatinine_lo),
  )

# Exclude patients with any value outside plausible physiologic ranges (eTable 1)
sofa2 <- sofa2 %>%
  filter(
    in_range_or_na(gcs_lo,        3,    15),
    in_range_or_na(gcs_hi,        3,    15),
    in_range_or_na(gcs_motor_lo,  1,    6),
    in_range_or_na(gcs_motor_hi,  1,    6),
    in_range_or_na(platelet_lo,   1,    999),
    in_range_or_na(platelet_hi,   1,    999),
    in_range_or_na(bilirubin_lo,  0.1,  46.78),
    in_range_or_na(bilirubin_hi,  0.1,  46.78),
    in_range_or_na(meanbp_lo,     10,   300),
    in_range_or_na(meanbp_hi,     10,   300),
    in_range_or_na(creatinine_lo, 0.10, 11.23),
    in_range_or_na(creatinine_hi, 0.10, 11.23),
  )

# Exclude ICU stays < 6 hours
sofa2 <- sofa2 %>%
  filter(los >= 0.25)


# -----------------------------------------------------------------------------
# 4. Create analysis variables
# -----------------------------------------------------------------------------

sofa2 <- sofa2 %>%
  mutate(
    icu_mort = as.integer(icu_death_flag == 1),
    
    sex = case_when(
      gender %in% c("M", "Male", "MALE")   ~ "Male",
      gender %in% c("F", "Female", "FEMALE") ~ "Female"
    ),
    
    age_group = case_when(
      is.na(age)             ~ "Unknown",
      age >= 18 & age <= 44  ~ "18\u201344",
      age >= 45 & age <= 64  ~ "45\u201364",
      age >= 65 & age <= 74  ~ "65\u201374",
      age >= 75              ~ "\u226575"
    ),
    
    race_group = {
      r <- toupper(trimws(race))
      case_when(
        is.na(r) | r == "" ~ "Unknown",
        str_detect(r, "UNABLE TO OBTAIN|UNKNOWN|PATIENT DECLINED") ~ "Unknown",
        str_detect(r, "PORTUGUESE") ~ "White",
        str_detect(r, "^WHITE")     ~ "White",
        str_detect(r, "BLACK")      ~ "Black",
        str_detect(r, "HISPAN|SOUTH AMERICAN|LATIN") ~ "Hispanic",
        str_detect(r, "ASIAN")      ~ "Asian",
        TRUE ~ "Other"
      )
    },
    
    language_group = case_when(
      is.na(language) | trimws(language) == "" ~ "Unknown",
      toupper(trimws(language)) == "ENGLISH"   ~ "English",
      TRUE ~ "Non-English"
    ),
    
    insurance_group = {
      ins <- toupper(trimws(insurance))
      case_when(
        is.na(ins) | ins == ""     ~ "Other",
        str_detect(ins, "MEDICAID") ~ "Medicaid",
        str_detect(ins, "MEDICARE") ~ "Medicare",
        str_detect(ins, "PRIVATE")  ~ "Private",
        str_detect(ins, "NO CHARGE") ~ "Other",
        TRUE ~ "Other"
      )
    },
    
    # Short aliases for organ subscores
    neuro   = cns_24hours,
    cardio  = cardiovascular_24hours,
    resp    = respiration_24hours,
    hepatic = liver_24hours,
    renal   = renal_24hours,
    coag    = coagulation_24hours
  )


# -----------------------------------------------------------------------------
# 5. Finalize analytic cohort
# -----------------------------------------------------------------------------

sofa2 <- sofa2 %>%
  filter(!is.na(sofa_24hours), !is.na(icu_death_flag))

N_total <- nrow(sofa2)
cat("Analytic cohort size:", N_total, "patients\n")


# -----------------------------------------------------------------------------
# 6. eTable 2 — Component-level missingness (first 24 hours)
# -----------------------------------------------------------------------------

etable2_rows <- tibble(
  Component = c("Neurological", "Hepatic", "Cardiovascular",
                "Renal", "Respiratory", "Coagulation"),
  missing_col = c("missing_cns_score", "missing_liver_score", "missing_cv_score",
                  "missing_renal_score", "missing_resp_score", "missing_coag_score")
)

etable2_values <- etable2_rows %>%
  rowwise() %>%
  mutate(
    n_missing = sum(sofa2[[missing_col]] == 1, na.rm = TRUE),
    `n (%) Missing` = sprintf("%d (%.1f%%)", n_missing, 100 * n_missing / N_total)
  ) %>%
  ungroup() %>%
  select(Component, `n (%) Missing`)

# Complete SOFA-2 rows (number WITH all components present)
n_complete_all6 <- sum(sofa2$complete_sofa2 == 1, na.rm = TRUE)
n_complete_excl_resp <- sum(
  sofa2$missing_coag_score  == 0 &
    sofa2$missing_liver_score == 0 &
    sofa2$missing_cv_score    == 0 &
    sofa2$missing_cns_score   == 0 &
    sofa2$missing_renal_score == 0,
  na.rm = TRUE
)

etable2_values <- bind_rows(
  etable2_values,
  tibble(Component = "Complete SOFA-2 (all 6 components)",
         `n (%) Missing` = sprintf("%d (%.1f%%)", n_complete_all6,
                                   100 * n_complete_all6 / N_total)),
  tibble(Component = "Complete SOFA-2 (excl. respiratory)",
         `n (%) Missing` = sprintf("%d (%.1f%%)", n_complete_excl_resp,
                                   100 * n_complete_excl_resp / N_total))
)

etable2_gt <- etable2_values %>%
  gt() %>%
  tab_header(
    title = "eTable 2. Missingness of SOFA-2 Components During the First 24 Hours",
    subtitle = sprintf("MIMIC-IV (n = %s)", format(N_total, big.mark = ","))
  ) %>%
  cols_label(Component = "SOFA-2 Component") %>%
  tab_footnote(
    footnote = paste(
      "\"Complete\" rows report the number of patients with all listed",
      "components present (not the number missing)."
    ),
    locations = cells_body(columns = Component,
                           rows = grepl("Complete", Component))
  )

cat("\n========== eTABLE 2 ==========\n")
etable2_gt


# -----------------------------------------------------------------------------
# 7. eFigure 1 — SOFA-2 score distribution
# -----------------------------------------------------------------------------

efig1_histogram <- ggplot(sofa2, aes(x = sofa_24hours)) +
  geom_histogram(
    binwidth = 1, closed = "right",
    color = "#08306B", fill = "#6BAED6"
  ) +
  labs(x = "Maximum First-Day SOFA-2 Score", y = "Number of ICU Stays") +
  theme_minimal(base_size = 12)

efig1_histogram

# -----------------------------------------------------------------------------
# 8. Table 1 — Baseline characteristics (multi-column format)
# -----------------------------------------------------------------------------

# One data row: n (%), median SOFA-2, median LOS, ICU mortality
t1_row <- function(df, label, N_denom = NULL) {
  n <- nrow(df)
  n_display <- if (is.null(N_denom)) format(n, big.mark = ",") else fmt_n_pct(n, N_denom)
  mort_n   <- sum(df$icu_mort == 1, na.rm = TRUE)
  mort_str <- sprintf("%s (%.1f%%)", format(mort_n, big.mark = ","), 100 * mort_n / n)
  tibble(
    Characteristic           = label,
    `MIMIC-IV Patients`      = n_display,
    `Median SOFA-2 (IQR)`   = fmt_median_iqr(df$sofa_24hours, digits = 0),
    `Median LOS, days (IQR)` = fmt_median_iqr(df$los, digits = 1),
    `ICU Mortality, n (%)`   = mort_str
  )
}

# Header row (group label, empty columns)
t1_header <- function(label) {
  tibble(
    Characteristic           = label,
    `MIMIC-IV Patients`      = NA_character_,
    `Median SOFA-2 (IQR)`   = NA_character_,
    `Median LOS, days (IQR)` = NA_character_,
    `ICU Mortality, n (%)`   = NA_character_
  )
}

# Block of rows for one grouping variable
t1_block <- function(group_var, levels, block_title) {
  rows <- list(t1_header(block_title))
  for (lv in levels) {
    d_lv <- sofa2 %>% filter(.data[[group_var]] == lv)
    rows[[length(rows) + 1]] <- t1_row(d_lv, paste0("    ", lv), N_denom = N_total)
  }
  bind_rows(rows)
}

table1_df <- bind_rows(
  t1_row(sofa2, "Overall"),
  t1_block("age_group",
           c("18\u201344", "45\u201364", "65\u201374", "\u226575"),
           "Age categories (years)"),
  t1_block("sex", c("Male", "Female"), "Sex"),
  t1_block("race_group",
           c("White", "Black", "Hispanic", "Asian", "Other", "Unknown"),
           "Race/Ethnicity"),
  t1_block("language_group",
           c("English", "Non-English", "Unknown"),
           "Primary Language"),
  t1_block("insurance_group",
           c("Medicare", "Private", "Medicaid", "Other"),
           "Insurance Status")
)

table1_gt <- table1_df %>%
  gt() %>%
  tab_header(title = "Table 1. Baseline Characteristics of the Study Population") %>%
  sub_missing(everything(), missing_text = "") %>%
  tab_footnote("Values are n (%) unless otherwise specified.")

cat("\n========== TABLE 1 ==========\n")
table1_gt


# -----------------------------------------------------------------------------
# 9. Table 2 — Discrimination and calibration by subgroup
# -----------------------------------------------------------------------------

make_table2_df <- function(df, score_col = "sofa_24hours", B = 1000, seed = 1) {
  
  # Fit overall logistic model for predicted probabilities
  cal_model <- glm(reformulate(score_col, "icu_mort"),
                   data = df, family = binomial())
  df <- df %>% mutate(
    p_hat  = predict(cal_model, type = "response"),
    lp_hat = safe_logit(p_hat)
  )
  
  metric_auc <- function(d) auc_ci(d, "icu_mort", score_col)$auc
  metric_br  <- function(d) brier_reliability(d$icu_mort, d$p_hat, bins = 10)
  
  # Build one block of rows for a grouping variable
  make_block <- function(group_var, levels, ref_level, block_title) {
    rows <- list(tibble(
      Subgroup = block_title, N = NA_integer_,
      `ICU Mortality, n (%)` = NA_character_,
      `AUROC (95% CI)` = NA_character_,
      `\u0394 AUROC (95% CI)` = NA_character_,
      `Brier Reliability` = NA_character_,
      `\u0394 Brier (95% CI)` = NA_character_,
      `Calibration Intercept (95% CI)` = NA_character_,
      `Calibration Slope (95% CI)` = NA_character_
    ))
    
    for (lv in levels) {
      d_lv <- df %>% filter(.data[[group_var]] == lv)
      N    <- nrow(d_lv)
      mort <- sum(d_lv$icu_mort == 1, na.rm = TRUE)
      
      a       <- auc_ci(d_lv, "icu_mort", score_col)
      auc_str <- if (is.na(a$auc)) NA_character_ else
        sprintf("%.3f (%.3f\u2013%.3f)", a$auc, a$lo, a$hi)
      
      br     <- metric_br(d_lv)
      br_str <- if (is.na(br)) NA_character_ else sprintf("%.4f", br)
      
      cal <- get_calibration_for_subgroup(d_lv)
      
      # Delta vs reference group
      if (lv == ref_level) {
        dA <- "Ref"; dB <- "Ref"
      } else {
        d_ref <- df %>% filter(.data[[group_var]] == ref_level)
        if (nrow(d_ref) < 50 || N < 50) {
          dA <- NA_character_; dB <- NA_character_
        } else {
          d_auc <- boot_diff(d_ref, d_lv, metric_auc, B = B, seed = seed)
          d_br  <- boot_diff(d_ref, d_lv, metric_br,  B = B, seed = seed)
          dA <- sprintf("%.3f (%.3f\u2013%.3f)", d_auc$diff, d_auc$lo, d_auc$hi)
          dB <- sprintf("%.4f (%.4f\u2013%.4f)", d_br$diff,  d_br$lo,  d_br$hi)
        }
      }
      
      rows <- append(rows, list(tibble(
        Subgroup = paste0("    ", lv), N = N,
        `ICU Mortality, n (%)` = if (N == 0) NA_character_ else fmt_n_pct(mort, N),
        `AUROC (95% CI)` = auc_str,
        `\u0394 AUROC (95% CI)` = dA,
        `Brier Reliability` = br_str,
        `\u0394 Brier (95% CI)` = dB,
        `Calibration Intercept (95% CI)` =
          if (is.na(cal$int)) NA_character_ else sprintf("%.2f (%s)", cal$int, cal$int_ci),
        `Calibration Slope (95% CI)` =
          if (is.na(cal$slope)) NA_character_ else sprintf("%.2f (%s)", cal$slope, cal$slope_ci)
      )))
    }
    bind_rows(rows)
  }
  
  # Overall row
  overall_auc <- auc_ci(df, "icu_mort", score_col)
  overall_br  <- brier_reliability(df$icu_mort, df$p_hat, bins = 10)
  overall_cal <- get_calibration_for_subgroup(df)
  
  bind_rows(
    tibble(
      Subgroup = "Overall", N = nrow(df),
      `ICU Mortality, n (%)` = fmt_n_pct(sum(df$icu_mort == 1), nrow(df)),
      `AUROC (95% CI)` = sprintf("%.3f (%.3f\u2013%.3f)",
                                 overall_auc$auc, overall_auc$lo, overall_auc$hi),
      `\u0394 AUROC (95% CI)` = "\u2013",
      `Brier Reliability` = sprintf("%.4f", overall_br),
      `\u0394 Brier (95% CI)` = "\u2013",
      `Calibration Intercept (95% CI)` =
        if (is.na(overall_cal$int)) NA_character_ else
          sprintf("%.2f (%s)", overall_cal$int, overall_cal$int_ci),
      `Calibration Slope (95% CI)` =
        if (is.na(overall_cal$slope)) NA_character_ else
          sprintf("%.2f (%s)", overall_cal$slope, overall_cal$slope_ci)
    ),
    make_block("age_group",
               c("18\u201344", "45\u201364", "65\u201374", "\u226575"),
               "18\u201344", "Age"),
    make_block("sex", c("Male", "Female"), "Male", "Sex"),
    make_block("race_group",
               c("White", "Black", "Hispanic", "Asian", "Other", "Unknown"),
               "White", "Race/Ethnicity"),
    make_block("language_group",
               c("English", "Non-English", "Unknown"),
               "English", "Primary Language"),
    make_block("insurance_group",
               c("Private", "Medicare", "Medicaid", "Other"),
               "Private", "Insurance Status")
  )
}

table2_df <- make_table2_df(sofa2, score_col = "sofa_24hours", B = 1000, seed = 1)

table2_gt <- table2_df %>%
  gt() %>%
  tab_header(
    title = "Table 2. Discrimination and Calibration of SOFA-2 Score by Subgroup",
    subtitle = "Predicted risk from overall logistic model: ICU mortality ~ SOFA-2"
  ) %>%
  sub_missing(everything(), missing_text = "") %>%
  cols_align(align = "left", columns = Subgroup)

cat("\n========== TABLE 2 ==========\n")
table2_gt


# -----------------------------------------------------------------------------
# 10. eTable 3 — Median organ subscores by subgroup
# -----------------------------------------------------------------------------

summ_row_median_iqr <- function(df, label, digits = 0) {
  tibble(
    Subgroup = label, N = nrow(df),
    Neuro       = fmt_median_iqr(df$neuro, digits),
    Cardio      = fmt_median_iqr(df$cardio, digits),
    Respiratory = fmt_median_iqr(df$resp, digits),
    Hepatic     = fmt_median_iqr(df$hepatic, digits),
    Renal       = fmt_median_iqr(df$renal, digits),
    Coag        = fmt_median_iqr(df$coag, digits),
    `Median SOFA-2` = fmt_median_iqr(df$sofa_24hours, digits)
  )
}

# Spacer row for group headers
etable3_spacer <- function(label) {
  tibble(Subgroup = label, N = NA, Neuro = NA, Cardio = NA, Respiratory = NA,
         Hepatic = NA, Renal = NA, Coag = NA, `Median SOFA-2` = NA)
}

etable3_df <- bind_rows(
  summ_row_median_iqr(sofa2, "Overall"),
  
  etable3_spacer("Sex"),
  summ_row_median_iqr(filter(sofa2, sex == "Male"),   "    Male"),
  summ_row_median_iqr(filter(sofa2, sex == "Female"), "    Female"),
  
  etable3_spacer("Age"),
  summ_row_median_iqr(filter(sofa2, age_group == "18\u201344"), "    18\u201344"),
  summ_row_median_iqr(filter(sofa2, age_group == "45\u201364"), "    45\u201364"),
  summ_row_median_iqr(filter(sofa2, age_group == "65\u201374"), "    65\u201374"),
  summ_row_median_iqr(filter(sofa2, age_group == "\u226575"),   "    \u226575"),
  
  etable3_spacer("Race/Ethnicity"),
  summ_row_median_iqr(filter(sofa2, race_group == "White"),    "    White"),
  summ_row_median_iqr(filter(sofa2, race_group == "Black"),    "    Black"),
  summ_row_median_iqr(filter(sofa2, race_group == "Hispanic"), "    Hispanic"),
  summ_row_median_iqr(filter(sofa2, race_group == "Asian"),    "    Asian"),
  summ_row_median_iqr(filter(sofa2, race_group == "Other"),    "    Other"),
  summ_row_median_iqr(filter(sofa2, race_group == "Unknown"),  "    Unknown"),
  
  etable3_spacer("Primary Language"),
  summ_row_median_iqr(filter(sofa2, language_group == "English"),     "    English"),
  summ_row_median_iqr(filter(sofa2, language_group == "Non-English"), "    Non-English"),
  summ_row_median_iqr(filter(sofa2, language_group == "Unknown"),     "    Unknown"),
  
  etable3_spacer("Insurance Status"),
  summ_row_median_iqr(filter(sofa2, insurance_group == "Private"),  "    Private"),
  summ_row_median_iqr(filter(sofa2, insurance_group == "Medicare"), "    Medicare"),
  summ_row_median_iqr(filter(sofa2, insurance_group == "Medicaid"), "    Medicaid"),
  summ_row_median_iqr(filter(sofa2, insurance_group == "Other"),    "    Other")
)

etable3_gt <- etable3_df %>%
  gt() %>%
  tab_header(
    title = "eTable 3. Median SOFA-2 Organ-Specific Subscores by Demographic Subgroup",
    subtitle = "Values are median (IQR)"
  ) %>%
  sub_missing(everything(), missing_text = "")

cat("\n========== eTABLE 3 ==========\n")
etable3_gt


# -----------------------------------------------------------------------------
# 11. Figure 2 & eFigures 7–9 — Mortality by SOFA-2 score, by demographics
# -----------------------------------------------------------------------------

# Compute mortality rates per SOFA-2 score bin within each subgroup level
compute_mortality_by_score <- function(df, group_var, levels,
                                       score_col = "sofa_24hours") {
  df %>%
    filter(.data[[group_var]] %in% levels) %>%
    mutate(
      sofa_bin = case_when(
        .data[[score_col]] >= 15 ~ "15+",
        TRUE ~ as.character(floor(.data[[score_col]]))
      ),
      sofa_bin = factor(sofa_bin, levels = c(as.character(0:14), "15+")),
      group    = factor(.data[[group_var]], levels = levels)
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

# Grouped bar chart
plot_mortality_barchart <- function(mort_data, title, group_var_name,
                                    color_palette, min_n = 10) {
  mort_data %>%
    filter(n >= min_n) %>%
    ggplot(aes(x = sofa_bin, y = mortality_pct, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.7) +
    scale_fill_manual(values = color_palette) +
    scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 20)) +
    labs(x = "SOFA-2 Score", y = "Mortality, %",
         title = title, fill = group_var_name) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(size = 9),
          panel.grid.minor = element_blank())
}

# Subgroup definitions and color palettes
age_levels  <- c("18\u201344", "45\u201364", "65\u201374", "\u226575")
age_colors  <- setNames(c("#C6DBEF", "#6BAED6", "#2171B5", "#08306B"), age_levels)

sex_levels  <- c("Male", "Female")
sex_colors  <- c("Male" = "#2166AC", "Female" = "#92C5DE")

race_levels <- c("White", "Asian", "Hispanic", "Black", "Other", "Unknown")
race_colors <- c("White" = "#C6DBEF", "Asian" = "#9ECAE1", "Hispanic" = "#6BAED6",
                 "Black" = "#4292C6", "Other" = "#2171B5", "Unknown" = "#084594")

language_levels <- c("English", "Non-English", "Unknown")
language_colors <- c("English" = "#C6DBEF", "Non-English" = "#6BAED6",
                     "Unknown" = "#08519C")

insurance_levels <- c("Private", "Medicare", "Medicaid", "Other")
insurance_colors <- c("Private" = "#C6DBEF", "Medicare" = "#6BAED6",
                      "Medicaid" = "#2171B5", "Other" = "#08306B")

# Compute mortality data
mort_age  <- compute_mortality_by_score(sofa2, "age_group",       age_levels)
mort_sex  <- compute_mortality_by_score(sofa2, "sex",             sex_levels)
mort_race <- compute_mortality_by_score(sofa2, "race_group",      race_levels)
mort_lang <- compute_mortality_by_score(sofa2, "language_group",  language_levels)
mort_ins  <- compute_mortality_by_score(sofa2, "insurance_group", insurance_levels)

# Figure 2 (main manuscript): age and sex panels
fig2a_age <- plot_mortality_barchart(
  mort_age, "Mortality rate by SOFA-2 score and age", "Age Group", age_colors)
fig2b_sex <- plot_mortality_barchart(
  mort_sex, "Mortality rate by SOFA-2 score and sex", "Sex", sex_colors)

# eFigures 7–9 (supplement): race, language, insurance
efig7_race <- plot_mortality_barchart(
  mort_race, "Observed ICU Mortality by SOFA-2 Score and Race/Ethnicity",
  "Race/Ethnicity", race_colors)
efig8_lang <- plot_mortality_barchart(
  mort_lang, "Observed ICU Mortality by SOFA-2 Score and Primary Language",
  "Language", language_colors)
efig9_ins <- plot_mortality_barchart(
  mort_ins, "Observed ICU Mortality by SOFA-2 Score and Insurance Status",
  "Insurance", insurance_colors)


# -----------------------------------------------------------------------------
# 12. eFigures 2–6 — Calibration plots by subgroup
# -----------------------------------------------------------------------------

# Bin patients by predicted risk and compute observed mortality per bin
calibration_bins <- function(df, group_var, levels,
                             score_col = "sofa_24hours", n_bins = 10,
                             min_n = 20) {
  m  <- glm(reformulate(score_col, "icu_mort"), data = df, family = binomial())
  df <- df %>% mutate(p_hat = predict(m, type = "response"))
  
  purrr::map_dfr(levels, function(lv) {
    d <- df %>% filter(.data[[group_var]] == lv)
    if (nrow(d) == 0) return(NULL)
    brks <- unique(quantile(d$p_hat,
                            probs = seq(0, 1, length.out = n_bins + 1),
                            na.rm = TRUE))
    if (length(brks) < 3) return(NULL)
    cal <- d %>%
      mutate(bin = cut(p_hat, breaks = brks, include.lowest = TRUE)) %>%
      group_by(bin) %>%
      summarise(n = n(), deaths = sum(icu_mort == 1),
                p_mean = mean(p_hat), y_mean = mean(icu_mort),
                .groups = "drop") %>%
      filter(n >= min_n)
    if (nrow(cal) == 0) return(NULL)
    ci <- wilson_ci(cal$deaths, cal$n)
    bind_cols(cal, ci) %>% mutate(level = lv)
  })
}

plot_calibration <- function(cal_df, title) {
  ggplot(cal_df, aes(x = p_mean, y = y_mean, color = level, group = level)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(0, 0.5)) +
    labs(x = "Mean predicted risk", y = "Observed ICU mortality",
         title = title, color = NULL) +
    theme_minimal()
}

cal_age  <- calibration_bins(sofa2, "age_group",
                             c("18\u201344", "45\u201364", "65\u201374", "\u226575"))
cal_sex  <- calibration_bins(sofa2, "sex", c("Male", "Female"))
cal_race <- calibration_bins(sofa2, "race_group",
                             c("White", "Black", "Hispanic", "Asian", "Other", "Unknown"))
cal_lang <- calibration_bins(sofa2, "language_group",
                             c("English", "Non-English", "Unknown"))
cal_ins  <- calibration_bins(sofa2, "insurance_group",
                             c("Private", "Medicare", "Medicaid", "Other"))

efig2_cal_age  <- plot_calibration(cal_age,  "Calibration by Age")
efig3_cal_sex  <- plot_calibration(cal_sex,  "Calibration by Sex")
efig4_cal_race <- plot_calibration(cal_race, "Calibration by Race/Ethnicity")
efig5_cal_lang <- plot_calibration(cal_lang, "Calibration by Language")
efig6_cal_ins  <- plot_calibration(cal_ins,  "Calibration by Insurance")


# -----------------------------------------------------------------------------
# 13. Display all outputs
# -----------------------------------------------------------------------------

cat("\n========== eTABLE 2 ==========\n")
etable2_gt

cat("\n========== TABLE 1 ==========\n")
table1_gt

cat("\n========== TABLE 2 ==========\n")
table2_gt

cat("\n========== eTABLE 3 ==========\n")
etable3_gt

cat("\n========== eFIGURE 1: SOFA-2 DISTRIBUTION ==========\n")
print(efig1_histogram)

cat("\n========== FIGURE 2: MORTALITY BY SOFA-2 (AGE, SEX) ==========\n")
print(fig2a_age)
print(fig2b_sex)

cat("\n========== eFIGURES 7-9: MORTALITY BY SOFA-2 (RACE, LANG, INS) ==========\n")
print(efig7_race)
print(efig8_lang)
print(efig9_ins)

cat("\n========== eFIGURES 2-6: CALIBRATION PLOTS ==========\n")
print(efig2_cal_age)
print(efig3_cal_sex)
print(efig4_cal_race)
print(efig5_cal_lang)
print(efig6_cal_ins)


# -----------------------------------------------------------------------------
# 14. Export tables as tab-separated text
# -----------------------------------------------------------------------------

df_to_tsv <- function(df) {
  out <- df %>%
    mutate(across(everything(), ~ ifelse(is.na(.x), "", as.character(.x))))
  paste(apply(out, 1, function(row) paste(row, collapse = "\t")),
        collapse = "\n") |>
    (\(body) paste(paste(names(out), collapse = "\t"), body, sep = "\n"))()
}

cat("\n\n========== eTABLE 2 (TSV) ==========\n")
cat(df_to_tsv(etable2_values))

cat("\n\n========== TABLE 1 (TSV) ==========\n")
cat(df_to_tsv(table1_df))

cat("\n\n========== TABLE 2 (TSV) ==========\n")
cat(df_to_tsv(table2_df))

cat("\n\n========== eTABLE 3 (TSV) ==========\n")
cat(df_to_tsv(etable3_df))

cat("\n\n========== FIGURE 2A DATA — AGE (TSV) ==========\n")
cat(df_to_tsv(mort_age))

cat("\n\n========== FIGURE 2B DATA — SEX (TSV) ==========\n")
cat(df_to_tsv(mort_sex))

cat("\n\n========== eFIGURE 7 DATA — RACE (TSV) ==========\n")
cat(df_to_tsv(mort_race))

cat("\n\n========== eFIGURE 8 DATA — LANGUAGE (TSV) ==========\n")
cat(df_to_tsv(mort_lang))

cat("\n\n========== eFIGURE 9 DATA — INSURANCE (TSV) ==========\n")
cat(df_to_tsv(mort_ins))
