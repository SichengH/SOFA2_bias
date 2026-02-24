##############################################################################
# SOFA-2 External Validation — Data Extraction & Cohort Assembly
# Upstream SQL: SOFA2.sql (scoring), SOFA2_component.sql (inputs)




# CHANGELOG (vs. original script):
#
# [R-02] Added gcs_motor_min, spo2fio2ratio_vent, spo2fio2ratio_novent,
#        ecmo_resp, ecmo_cv to the missingness column list. These are new
#        outputs from the updated SOFA-2 SQL and are needed for eTable 2.
#
# [R-03] GCS missingness now requires BOTH gcs_min AND gcs_motor_min to be
#        absent before flagging missing. The SQL uses motor fallback when
#        total GCS is unavailable (footnote d), so a patient with only
#        gcs_motor_min still gets a valid CNS score.
#
# [R-04] Added individual S/F missingness flags and a combined
#        missing_respiratory_all flag (no P/F or S/F data in 24h).
#
# [R-05] Added ecmo_resp and ecmo_cv missingness flags for VV/VA tracking.
#
# [R-06] Added sofa2_max24: computes true MAX of each 24h rolling score
#        across hours 0-24 per stay. The old commented-out version took the
#        last row by hour, which could miss the actual peak. Now joined to
#        mimic_first_icu.
#
# [R-07] Added score-level (component) missingness for eTable 2A. Checks
#        whether each computed subscore (respiration, coagulation, etc.) was
#        NULL for every hour in 0-24h. Also computes complete_sofa2 flag.
#        This differs from variable-level missingness because a score can
#        be non-NULL even when some inputs are missing (e.g., CNS = 1 from
#        delirium drug alone when GCS is NULL).
#
# [R-08] Joined sofa2_max24 and sofa2_24h_score_missing to mimic_first_icu
#        in the cohort assembly step.
#
# [R-09] Removed stale commented-out code blocks that were superseded by
#        the changes above (old sofa2_max24, old per-variable missing flags,
#        old complete_case_24h).



# Packages
library(data.table)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(readxl)
library(gdata)
library(lubridate)
library(comorbidity)
library(table1)
library(bigrquery)
library(DBI)
library(dbplyr)
library(dplyr)
library(broom)
library(patchwork)
library(writexl)

setwd("/Users/sichenghao/Documents/GitHub 2/SOFA_AI/")

projectid = "mimic-hr"
bigrquery::bq_auth()

##############################################################################
# 1. DOWNLOAD SOFA-2 SCORES (hourly, all ICU stays)
##############################################################################

sql <- "SELECT * FROM `mimic-hr.derived.sofa2`"
bq_data <- bq_project_query(projectid, query = sql)
sofa2_mimic = bq_table_download(bq_data)

##############################################################################
# 2. DEMOGRAPHICS (admissions + age + gender)
##############################################################################

sql <- "
SELECT subject_id, hadm_id, insurance, language, race, marital_status, hospital_expire_flag
FROM `physionet-data.mimiciv_3_1_hosp.admissions`
"
bq_data <- bq_project_query(projectid, query = sql)
mimic_demographic = bq_table_download(bq_data)

sql <- "SELECT hadm_id, age FROM `physionet-data.mimiciv_3_1_derived.age`"
bq_data <- bq_project_query(projectid, query = sql)
mimic_age = bq_table_download(bq_data)

sql <- "SELECT subject_id, gender FROM `physionet-data.mimiciv_3_1_hosp.patients`"
bq_data <- bq_project_query(projectid, query = sql)
mimic_gender = bq_table_download(bq_data)

##############################################################################
# 3. ICU STAYS — identify first ICU stay per patient
##############################################################################

sql <- "
select icu.* ,adm.admission_location,adm.discharge_location,hospital_expire_flag,gender,insurance,pt.dod,
ROW_NUMBER() OVER (PARTITION BY icu.hadm_id ORDER BY icu.intime) AS row_number
from `physionet-data.mimiciv_3_1_icu.icustays` icu
left join `physionet-data.mimiciv_3_1_hosp.admissions` adm
on adm.hadm_id = icu.hadm_id
and adm.subject_id = icu.subject_id
left join `physionet-data.mimiciv_3_1_hosp.patients` pt
on adm.subject_id = pt.subject_id
"
bq_data <- bq_project_query(projectid, query = sql)
all_icus = bq_table_download(bq_data)
nrow(all_icus)

first_icu = all_icus%>%filter(row_number==1)
first_icu = first_icu%>%
  group_by(subject_id)%>%
  arrange(intime)%>%
  slice_head(n = 1) %>%
  ungroup()

mimic_demographic = left_join(mimic_demographic,mimic_age)
mimic_demographic = left_join(mimic_demographic,mimic_gender)

##############################################################################
# 4. ICU-SPECIFIC MORTALITY (death within 6h of ICU discharge)
##############################################################################

sql <- "
SELECT icu.*,adm.deathtime
, CASE when adm.deathtime <= icu.outtime + interval 6 hour then 1 else 0 end as icu_death_flag
FROM `physionet-data.mimiciv_3_1_icu.icustays` icu
LEFT JOIN `physionet-data.mimiciv_3_1_hosp.admissions` adm
ON icu.hadm_id = adm.hadm_id
"
bq_data <- bq_project_query(projectid, query = sql)
mimic_icu = bq_table_download(bq_data)

mimic_icu = left_join(mimic_icu,mimic_demographic)
mimic_first_icu = mimic_icu%>%filter(stay_id%in%first_icu$stay_id)

##############################################################################
# 5. 24H MAX SOFA-2 SCORE
# [R-06] NEW: computes true MAX of each 24h rolling score across hr 0-24.
# The old version (commented out in original) took the last row by hour,
# which could miss the actual peak if the worst score occurred earlier.
##############################################################################

sofa2_max24 = sofa2_mimic %>%
  filter(hr >= 0, hr <= 24) %>%
  group_by(stay_id) %>%
  summarise(
    sofa_24hours           = max(sofa_24hours, na.rm = TRUE),
    respiration_24hours    = max(respiration_24hours, na.rm = TRUE),
    coagulation_24hours    = max(coagulation_24hours, na.rm = TRUE),
    liver_24hours          = max(liver_24hours, na.rm = TRUE),
    cardiovascular_24hours = max(cardiovascular_24hours, na.rm = TRUE),
    cns_24hours            = max(cns_24hours, na.rm = TRUE),
    renal_24hours          = max(renal_24hours, na.rm = TRUE),
    .groups = "drop"
  )

##############################################################################
# 6. VARIABLE-LEVEL MISSINGNESS (first 24h) — feeds eTable 2B
##############################################################################

sofa2_24h = sofa2_mimic%>%
  filter(hr<=24)

# Convert treatment-absence (0) to NA for eTable 2 reporting.
sofa2_24h$ECMO[sofa2_24h$ECMO==0] = NA
sofa2_24h$mechanical_support[sofa2_24h$mechanical_support==0] = NA

sofa2_24h = sofa2_24h%>%filter(stay_id%in%mimic_first_icu$stay_id)

# [R-02] Added gcs_motor_min, spo2fio2ratio_vent, spo2fio2ratio_novent,
#        ecmo_resp, ecmo_cv to this list (new SQL output columns)
cols <- c("gcs_min"
          ,"gcs_motor_min"            # [R-02] added: motor fallback (footnote d)
          , "meanbp_min"
          , "platelet_min"
          ,"bilirubin_max"
          ,"creatinine_max"
          ,"uomlkghr_6hr"
          ,"uomlkghr_12hr"
          ,"uomlkghr_24hr"
          ,"pao2fio2ratio_vent"
          ,"pao2fio2ratio_novent"
          ,"spo2fio2ratio_vent"       # [R-02] added: S/F fallback (footnote f)
          ,"spo2fio2ratio_novent"     # [R-02] added: S/F fallback (footnote f)
          ,"mechanical_support"
          ,"dialysis_active"
          ,"delirium_drug_rate"
          ,"ECMO"
          ,"ecmo_resp"                # [R-02] added: VV-ECMO flag
          ,"ecmo_cv"                  # [R-02] added: VA-ECMO flag
          ,"rate_epinephrine"
          ,"rate_norepinephrine"
          ,"rate_dopamine"
          ,"rate_dobutamine"
          ,"rate_milrinone"
          ,"rate_vasopressin"
          ,"rate_phenylephrine"
)

sofa2_24h_missing = sofa2_24h %>%
  group_by(stay_id) %>%
  summarise(
    across(all_of(cols), ~ sum(!is.na(.)), .names = "n_value_{.col}"),
    .groups = "drop"
  )

# [R-03] GCS: now requires BOTH gcs_min AND gcs_motor_min absent to flag missing.
# Old: missing_GCS_24h = ifelse(n_value_gcs_min==0, 1, 0)
# New: patient with only motor component still gets a valid CNS score.
sofa2_24h_missing$missing_GCS_24h = ifelse(
  sofa2_24h_missing$n_value_gcs_min==0 &
  sofa2_24h_missing$n_value_gcs_motor_min==0, 1, 0)

sofa2_24h_missing$missing_bilirubin_24h = ifelse(sofa2_24h_missing$n_value_bilirubin_max==0,1,0)
sofa2_24h_missing$missing_platelet_24h = ifelse(sofa2_24h_missing$n_value_platelet_min==0,1,0)
sofa2_24h_missing$missing_MAP_24h = ifelse(sofa2_24h_missing$n_value_meanbp_min==0,1,0)
sofa2_24h_missing$missing_creatinine = ifelse(sofa2_24h_missing$n_value_creatinine_max==0,1,0)

sofa2_24h_missing$missing_uomlkghr_6hr = ifelse(sofa2_24h_missing$n_value_uomlkghr_6hr==0,1,0)
sofa2_24h_missing$missing_uomlkghr_12hr = ifelse(sofa2_24h_missing$n_value_uomlkghr_12hr==0,1,0)
sofa2_24h_missing$missing_uomlkghr_24hr = ifelse(sofa2_24h_missing$n_value_uomlkghr_24hr==0,1,0)

sofa2_24h_missing$missing_pao2fio2ratio_vent = ifelse(sofa2_24h_missing$n_value_pao2fio2ratio_vent==0,1,0)
sofa2_24h_missing$missing_pao2fio2ratio_novent = ifelse(sofa2_24h_missing$n_value_pao2fio2ratio_novent==0,1,0)

# [R-04] S/F missingness (new — not in original)
sofa2_24h_missing$missing_spo2fio2ratio_vent = ifelse(sofa2_24h_missing$n_value_spo2fio2ratio_vent==0,1,0)
sofa2_24h_missing$missing_spo2fio2ratio_novent = ifelse(sofa2_24h_missing$n_value_spo2fio2ratio_novent==0,1,0)

# [R-04] Combined respiratory: no P/F or S/F data at all in 24h
sofa2_24h_missing$missing_respiratory_all = ifelse(
  sofa2_24h_missing$n_value_pao2fio2ratio_vent==0 &
  sofa2_24h_missing$n_value_pao2fio2ratio_novent==0 &
  sofa2_24h_missing$n_value_spo2fio2ratio_vent==0 &
  sofa2_24h_missing$n_value_spo2fio2ratio_novent==0, 1, 0)

sofa2_24h_missing$missing_mechanical_support = ifelse(sofa2_24h_missing$n_value_mechanical_support==0,1,0)
sofa2_24h_missing$missing_dialysis_active = ifelse(sofa2_24h_missing$n_value_dialysis_active==0,1,0)
sofa2_24h_missing$missing_delirium_drug_rate = ifelse(sofa2_24h_missing$n_value_delirium_drug_rate==0,1,0)
sofa2_24h_missing$missing_ECMO = ifelse(sofa2_24h_missing$n_value_ECMO==0,1,0)

# [R-05] VV/VA ECMO flags (new — not in original)
sofa2_24h_missing$missing_ecmo_resp = ifelse(sofa2_24h_missing$n_value_ecmo_resp==0,1,0)
sofa2_24h_missing$missing_ecmo_cv = ifelse(sofa2_24h_missing$n_value_ecmo_cv==0,1,0)

sofa2_24h_missing$missing_vasopressor = ifelse(sofa2_24h_missing$n_value_rate_dobutamine==0&
                                               sofa2_24h_missing$n_value_rate_epinephrine==0&
                                               sofa2_24h_missing$n_value_rate_norepinephrine==0&
                                               sofa2_24h_missing$n_value_rate_dopamine==0&
                                               sofa2_24h_missing$n_value_rate_milrinone==0&
                                               sofa2_24h_missing$n_value_rate_vasopressin==0&
                                               sofa2_24h_missing$n_value_rate_phenylephrine==0,1,0)

##############################################################################
# 7. SCORE-LEVEL (COMPONENT) MISSINGNESS (first 24h) — feeds eTable 2A
# [R-07] NEW SECTION: checks whether each computed subscore was NULL for
# every hour in 0-24h. This differs from variable-level missingness because
# a score can be non-NULL even when some inputs are missing (e.g., CNS = 1
# from delirium drug alone when GCS is NULL).
# complete_sofa2 = all 6 component scores non-NULL for at least 1 hour.
##############################################################################

sofa2_24h_score_missing = sofa2_24h %>%
  group_by(stay_id) %>%
  summarise(
    missing_resp_score  = as.integer(all(is.na(respiration))),
    missing_coag_score  = as.integer(all(is.na(coagulation))),
    missing_liver_score = as.integer(all(is.na(liver))),
    missing_cv_score    = as.integer(all(is.na(cardiovascular))),
    missing_cns_score   = as.integer(all(is.na(cns))),
    missing_renal_score = as.integer(all(is.na(renal))),
    .groups = "drop"
  )

sofa2_24h_score_missing$complete_sofa2 = ifelse(
  sofa2_24h_score_missing$missing_resp_score==0 &
  sofa2_24h_score_missing$missing_coag_score==0 &
  sofa2_24h_score_missing$missing_liver_score==0 &
  sofa2_24h_score_missing$missing_cv_score==0 &
  sofa2_24h_score_missing$missing_cns_score==0 &
  sofa2_24h_score_missing$missing_renal_score==0, 1, 0)

##############################################################################
# 8. ASSEMBLE ANALYTIC COHORT
# [R-08] Now joins sofa2_max24 and sofa2_24h_score_missing in addition
#        to sofa2_24h_missing. Original only joined sofa2_24h_missing.
##############################################################################

mimic_first_icu$los_less_6h = ifelse(mimic_first_icu$los<=0.25,1,0)
mimic_first_icu = left_join(mimic_first_icu, sofa2_max24)             # [R-06] new
mimic_first_icu = left_join(mimic_first_icu, sofa2_24h_missing)
mimic_first_icu = left_join(mimic_first_icu, sofa2_24h_score_missing) # [R-07] new

##############################################################################
# 9. SAVE OUTPUTS
##############################################################################

fwrite(sofa2_mimic, file = "sofa2_mimic_all.csv")
fwrite(mimic_first_icu, file = "mimic_first_icu.csv")

##############################################################################
# 10. ABG DATA CHECK (exploratory)
##############################################################################

sql <- "SELECT * FROM `physionet-data.mimiciv_3_1_derived.bg`"
bq_data <- bq_project_query(projectid, query = sql)
bg = bq_table_download(bq_data)

abg = bg%>%filter(specimen == 'ART.')%>%filter(po2>0)%>%filter(!is.infinite(po2))%>%filter(po2<300)
hist(abg$po2)
