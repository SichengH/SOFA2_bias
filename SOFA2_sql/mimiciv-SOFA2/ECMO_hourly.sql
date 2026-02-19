DROP TABLE IF EXISTS `mimic-hr.derived.ECMO_hourly`;
CREATE TABLE `mimic-hr.derived.ECMO_hourly` AS 

WITH co AS (
    SELECT ih.stay_id, ie.hadm_id
        , hr
        -- start/endtime can be used to filter to values within this hour
        , DATETIME_SUB(ih.endtime, INTERVAL '1' HOUR) AS starttime
        , ih.endtime
    FROM `mimic-hr.derived.icustay_hourly` ih
    INNER JOIN `physionet-data.mimiciv_3_1_icu.icustays` ie
        ON ih.stay_id = ie.stay_id
)

,ecmo AS (
  select *
  from `physionet-data.mimiciv_3_1_icu.chartevents` chart 
  left join `physionet-data.mimiciv_3_1_icu.d_items` d
  on chart.itemid = d.itemid
  where d.label like '%ECMO%' or d.category = 'ECMO'
)
, ecmo_hr AS (
    SELECT co.stay_id, co.hr
        -- vitals
        , CASE WHEN MAX(ecmo.value) is not null then 1 else 0 end as ECMO
        -- ============================================================
        -- [CHANGE-01] Added VV/VA ECMO split columns (footnote i).
        -- VV-ECMO → respiratory score 4 only.
        -- VA-ECMO → respiratory 4 AND cardiovascular 4.
        -- Without this split, VV-ECMO patients incorrectly get CV score 4.
        -- The value 'VV' or 'VA' comes from Circuit Configuration (ECMO) rows.
        , MAX(CASE WHEN ecmo.value = 'VV' THEN 1 ELSE 0 END) AS ecmo_resp
        , MAX(CASE WHEN ecmo.value = 'VA' THEN 1 ELSE 0 END) AS ecmo_cv
        -- [END CHANGE-01]
        -- ============================================================
    FROM co
    LEFT JOIN ecmo
        ON co.stay_id = ecmo.stay_id
            AND co.starttime < ecmo.charttime
            AND co.endtime >= ecmo.charttime
    GROUP BY co.stay_id, co.hr
)

select * from ecmo_hr
