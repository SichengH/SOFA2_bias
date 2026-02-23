-- ECMO hourly flags for SOFA-2
-- Outputs: ECMO (any), ecmo_resp (VV), ecmo_cv (VA) per ICU hour
-- VV-ECMO → respiratory score 4 only
-- VA-ECMO → respiratory 4 AND cardiovascular 4 (footnote i)

DROP TABLE IF EXISTS `mimic-hr.derived.ECMO_hourly`;
CREATE TABLE `mimic-hr.derived.ECMO_hourly` AS 

WITH co AS (
    SELECT ih.stay_id, ie.hadm_id
        , hr
        , DATETIME_SUB(ih.endtime, INTERVAL '1' HOUR) AS starttime
        , ih.endtime
    FROM `mimic-hr.derived.icustay_hourly` ih
    INNER JOIN `physionet-data.mimiciv_3_1_icu.icustays` ie
        ON ih.stay_id = ie.stay_id
)

, ecmo AS (
    SELECT *
    FROM `physionet-data.mimiciv_3_1_icu.chartevents` chart 
    LEFT JOIN `physionet-data.mimiciv_3_1_icu.d_items` d
        ON chart.itemid = d.itemid
    WHERE d.label LIKE '%ECMO%' OR d.category = 'ECMO'
)

, ecmo_hr AS (
    SELECT co.stay_id, co.hr
        , CASE WHEN MAX(ecmo.value) IS NOT NULL THEN 1 ELSE 0 END AS ECMO
        -- VV/VA from Circuit Configuration (ECMO) rows
        , MAX(CASE WHEN ecmo.value = 'VV' THEN 1 ELSE 0 END) AS ecmo_resp
        , MAX(CASE WHEN ecmo.value = 'VA' THEN 1 ELSE 0 END) AS ecmo_cv
    FROM co
    LEFT JOIN ecmo
        ON co.stay_id = ecmo.stay_id
            AND co.starttime < ecmo.charttime
            AND co.endtime >= ecmo.charttime
    GROUP BY co.stay_id, co.hr
)

SELECT * FROM ecmo_hr
