# re-write this with ICU days and then shifts(if needed)
DROP TABLE IF EXISTS `mimic-hr.derived.icustay_daily`;
CREATE TABLE `mimic-hr.derived.icustay_daily` AS 
WITH icu_days AS (
    SELECT
        it.stay_id
        ,it.hadm_id
        ,it.intime
        ,it.outtime
        , CASE WHEN extract(hour from intime) >= 8
        THEN DATETIME_ADD(CAST(CAST(intime AS DATE) AS DATETIME) , INTERVAL '8' hour)
        ELSE DATETIME_SUB(CAST(CAST(intime AS DATE) AS DATETIME) , INTERVAL '16' hour) END AS day_start
         , CASE WHEN extract(hour from outtime) < 8
        THEN DATETIME_ADD(CAST(CAST(outtime AS DATE) AS DATETIME) , INTERVAL '8' hour)
        ELSE DATETIME_ADD(CAST(CAST(outtime AS DATE) AS DATETIME) , INTERVAL '32' hour) END AS day_end

    FROM `physionet-data.mimiciv_3_1_icu.icustays` it
)

, all_days AS (
    SELECT *,
    GENERATE_ARRAY(0, DATETIME_DIFF(day_end, day_start, day)) AS days
    FROM icu_days
)

#select * from all_days limit 5

SELECT DISTINCT stay_id,hadm_id
    , CAST(days+1 AS INT64) AS days
    , DATETIME_ADD(day_start, INTERVAL CAST(days AS INT64) day) AS starttime
    , DATETIME_ADD(day_start, INTERVAL CAST(days+1 AS INT64) day) AS endtime
FROM all_days
CROSS JOIN UNNEST(all_days.days) AS days