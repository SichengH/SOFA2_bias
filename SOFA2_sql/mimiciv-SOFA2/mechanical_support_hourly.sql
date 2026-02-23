-- Mechanical circulatory support hourly flag for SOFA-2
-- Detects IABP, Impella, VAD, and VA-ECMO from chartevents labels
-- Any presence → mechanical_support = 1 → cardiovascular score 4 (footnote n)

DROP TABLE IF EXISTS `mimic-hr.derived.mechanical_support_hourly`;
CREATE TABLE `mimic-hr.derived.mechanical_support_hourly` AS 

WITH co AS (
    SELECT ih.stay_id, ie.hadm_id
        , hr
        , DATETIME_SUB(ih.endtime, INTERVAL '1' HOUR) AS starttime
        , ih.endtime
    FROM `mimic-hr.derived.icustay_hourly` ih
    INNER JOIN `physionet-data.mimiciv_3_1_icu.icustays` ie
        ON ih.stay_id = ie.stay_id
)

, mechanical_support AS (
    SELECT *
    FROM `physionet-data.mimiciv_3_1_icu.chartevents` chart 
    LEFT JOIN `physionet-data.mimiciv_3_1_icu.d_items` d
        ON chart.itemid = d.itemid
    WHERE LOWER(label) LIKE '%balloon%'
       OR LOWER(label) LIKE '%iabp%'
       OR LOWER(label) LIKE '%impella%'
       OR LOWER(label) LIKE '%vad%'
       OR (label = 'Circuit Configuration (ECMO)' AND value = 'VA')
)

, mechanical_support_hour AS (
    SELECT co.stay_id, co.hr
        , CASE WHEN MAX(mechanical_support.value) IS NOT NULL THEN 1 ELSE 0 END AS mechanical_support
    FROM co
    LEFT JOIN mechanical_support
        ON co.stay_id = mechanical_support.stay_id
            AND co.starttime < mechanical_support.charttime
            AND co.endtime >= mechanical_support.charttime
    GROUP BY co.stay_id, co.hr
)

SELECT * FROM mechanical_support_hour
