
library(data.table)
library(ggplot2)
library(data.table)
library(tidyr)
library(tidyverse)
library(readxl)
library(gdata)
library(lubridate)
library(comorbidity)

library(table1)

library(bigrquery)
library(tidyr)
library(DBI)
library(dbplyr)
library(dplyr)
library(broom)
library(patchwork)
library(writexl)

#setwd("/Users/sh/SCCM_T5")
#setwd("/Users/sichenghao/Desktop/Projects/Extubation_Prediction/")

setwd("/Users/sichenghao/Documents/GitHub 2/SOFA_AI/")

projectid = "mort-prediction-icu"#replace with your own project id
bigrquery::bq_auth()#login with google account associated with physionet account

sql <- "
SELECT * FROM `mort-prediction-icu.derived.sofa2`
"

bq_data <- bq_project_query(projectid, query = sql)
sofa2_mimic = bq_table_download(bq_data)


sql <- "
SELECT subject_id, hadm_id,insurance,language,race,marital_status,hospital_expire_flag
FROM `physionet-data.mimiciv_3_1_hosp.admissions` 
"

bq_data <- bq_project_query(projectid, query = sql)
mimic_demographic = bq_table_download(bq_data) # 


sql <- "
SELECT hadm_id,age FROM `physionet-data.mimiciv_3_1_derived.age`
"

bq_data <- bq_project_query(projectid, query = sql)
mimic_age = bq_table_download(bq_data) # 


sql <- "
SELECT subject_id,gender FROM `physionet-data.mimiciv_3_1_hosp.patients`
"

bq_data <- bq_project_query(projectid, query = sql)
mimic_gender = bq_table_download(bq_data) # 


sql <- "
select icu.* ,adm.admission_location,adm.discharge_location,hospital_expire_flag,gender,insurance,pt.dod,ROW_NUMBER() OVER (PARTITION BY icu.hadm_id ORDER BY icu.intime DESC) AS row_number
from `physionet-data.mimiciv_3_1_icu.icustays` icu
left join `physionet-data.mimiciv_3_1_hosp.admissions` adm
on adm.hadm_id = icu.hadm_id
and adm.subject_id = icu.subject_id
left join `physionet-data.mimiciv_3_1_hosp.patients` pt
on adm.subject_id = pt.subject_id
"
bq_data <- bq_project_query(projectid, query = sql)

all_icus = bq_table_download(bq_data)#
nrow(all_icus)

first_icu = all_icus%>%filter(row_number==1)
first_icu = first_icu%>%
  group_by(subject_id)%>%
  arrange(intime)%>%
  slice_head(n = 1) %>%
  ungroup()


mimic_demographic = left_join(mimic_demographic,mimic_age)
mimic_demographic = left_join(mimic_demographic,mimic_gender)


## ICU Specific Mortality 
sql <- "
SELECT icu.*,adm.deathtime
, CASE when adm.deathtime <= icu.outtime + interval 6 hour then 1 else 0 end as icu_death_flag
FROM `physionet-data.mimiciv_3_1_icu.icustays` icu 
LEFT JOIN `physionet-data.mimiciv_3_1_hosp.admissions` adm 
ON icu.hadm_id = adm.hadm_id
"


bq_data <- bq_project_query(projectid, query = sql)
mimic_icu = bq_table_download(bq_data) # 



mimic_icu = left_join(mimic_icu,mimic_demographic)
mimic_first_icu = mimic_icu%>%filter(stay_id%in%first_icu$stay_id)

## 24H max SOFA

sofa2_24h = sofa2_mimic%>%
  filter(hr<=24)

sofa2_24h%>%group_by(stay_id) %>%
  summarise(
    across(all_of(cols), ~ sum(is.na(.)), .names = "n_miss_{.col}"),
    .groups = "drop"
  )

cols <- c("gcs_min"
          , "meanbp_min"
          , "platelet_min"
          ,"bilirubin_max"
          ,"creatinine_max"
#          ,"uomlkghr_6hr"
#          ,"uomlkghr_12hr"
#          ,"uomlkghr_24hr"
#          ,"pao2fio2ratio_vent"
#          ,"pao2fio2ratio_novent"
)

sofa2_24h_missing = sofa2_24h %>%
  group_by(stay_id) %>%
  summarise(
    across(all_of(cols), ~ sum(!is.na(.)), .names = "n_value_{.col}"),
    .groups = "drop"
  )

sofa2_24h_missing$missing_GCS_24h = ifelse(sofa2_24h_missing$n_value_gcs_min==0,1,0)
sofa2_24h_missing$missing_bilirubin_24h = ifelse(sofa2_24h_missing$n_value_bilirubin_max==0,1,0)
sofa2_24h_missing$missing_platelet_24h = ifelse(sofa2_24h_missing$n_value_platelet_min==0,1,0)
sofa2_24h_missing$missing_MAP_24h = ifelse(sofa2_24h_missing$n_value_meanbp_min==0,1,0)
sofa2_24h_missing$missing_creatinine = ifelse(sofa2_24h_missing$n_value_creatinine_max==0,1,0)

# 
# out <- sofa2_24h %>%
#   group_by(stay_id) %>%
#   summarise(
#     n = n(),
#     across(all_of(cols), list(
# #      miss_n    = ~ sum(is.na(.x)),
#       miss_rate = ~ mean(is.na(.x))
#     )),
#     .groups = "drop"
#   )

#sofa2_max24$missing_GCS = is.na(sofa2_max24$gcs_min)
#sofa2_max24$missing_MAP = is.na(sofa2_max24$meanbp_min)
#sofa2_max24$missing_bilirubin = is.na(sofa2_max24$bilirubin_max)
#sofa2_max24$missing_creatinine = is.na(sofa2_max24$creatinine_max)
#sofa2_max24$missing_platelet = is.na(sofa2_max24$platelet_min)

##Missing Urine

#sofa2_max24$missing_urine_6hr = is.na(sofa2_max24$uomlkghr_6hr)


##Missing PF
#sofa2_max24$missing_PF_vent = is.na(sofa2_max24$pao2fio2ratio_vent)
#sofa2_max24$missing_PF_novent = is.na(sofa2_max24$pao2fio2ratio_novent)




##Missing vasopressors (not ready)
# sofa2_24h$missing_pressors = is.na(sofa2_24h$rate_epinephrine)&
#   is.na(sofa2_24h$rate_norepinephrine)&
#   is.na(sofa2_24h$rate_dopamine)&
#   is.na(sofa2_24h$rate_dobutamine)&
#   is.na(sofa2_24h$rate_milrinone)&
#   is.na(sofa2_24h$rate_vasopressin)




# sofa2_max24 = sofa2_mimic%>%
#   filter(hr<=24)%>%
#   group_by(stay_id)%>%
#   arrange(desc(hr),by_group = TRUE)%>%
#   filter(row_number() == 1)






mimic_first_icu$los_less_6h = ifelse(mimic_first_icu$los<=0.25,1,0)
mimic_first_icu = left_join(mimic_first_icu,sofa2_max24)
mimic_first_icu = left_join(mimic_first_icu,sofa2_24h_missing)

mimic_first_icu$complete_case_24h = ifelse(mimic_first_icu$missing_GCS_24h==1|
                                             mimic_first_icu$missing_bilirubin_24h==1|
                                             mimic_first_icu$missing_platelet_24h==1|
                                             mimic_first_icu$missing_MAP_24h==1|
                                             mimic_first_icu$missing_creatinine,0,1)
mimic_icu = left_join(mimic_icu,mimic_demographic)
fwrite(sofa2_mimic,file = "sofa2_mimic_all.csv")
fwrite(mimic_icu,file = "mimic_all_icu.csv")
fwrite(mimic_first_icu,file = "mimic_first_icu.csv")

mimic_icu = fread("mimic_icu.csv")



mouth_care = fread("mouthcare_interval_frequency.csv")


#### Check ABG data###
sql <- "
SELECT * FROM `physionet-data.mimiciv_3_1_derived.bg`
"

bq_data <- bq_project_query(projectid, query = sql)
bg = bq_table_download(bq_data) # 

abg = bg%>%filter(specimen == 'ART.')%>%filter(po2>0)%>%filter(!is.infinite(po2))%>%filter(po2<300)
hist(abg$po2)
