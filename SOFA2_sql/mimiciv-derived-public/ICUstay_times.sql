

DROP TABLE IF EXISTS `mimic-hr.derived.icustay_times`;
CREATE TABLE `mimic-hr.derived.icustay_times` AS 
WITH t1 AS (
    SELECT ce.stay_id
        , MIN(charttime) AS intime_hr
        , MAX(charttime) AS outtime_hr
    FROM `physionet-data.mimiciv_3_1_icu.chartevents` ce
    -- only look at heart rate
    WHERE ce.itemid = 220045
    GROUP BY ce.stay_id
)

-- add in subject_id/hadm_id
SELECT
    ie.subject_id, ie.hadm_id, ie.stay_id
    , t1.intime_hr
    , t1.outtime_hr
FROM `physionet-data.mimiciv_3_1_icu.icustays` ie
LEFT JOIN t1
          ON ie.stay_id = t1.stay_id;