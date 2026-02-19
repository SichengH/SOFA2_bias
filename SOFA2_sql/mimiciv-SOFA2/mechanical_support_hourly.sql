DROP TABLE IF EXISTS `mimic-hr.derived.mechanical_support_hourly`;
CREATE TABLE `mimic-hr.derived.mechanical_support_hourly` AS 

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

,mechanical_support AS (
  select *
  from `physionet-data.mimiciv_3_1_icu.chartevents` chart 
  left join `physionet-data.mimiciv_3_1_icu.d_items` d
  on chart.itemid = d.itemid
  WHERE LOWER(label) LIKE '%balloon%'
   OR LOWER(label) LIKE '%iabp%'
   OR LOWER(label) LIKE '%impella%'
   OR LOWER(label) LIKE '%vad%'
   OR (label = 'Circuit Configuration (ECMO)' AND value = 'VA')
)
, mechanical_support_hour AS (
--update information about mechanical support
    SELECT co.stay_id, co.hr
        -- vitals
        , CASE WHEN MAX(mechanical_support.value) is not null then 1 else 0 end as mechanical_support
    FROM co
    LEFT JOIN mechanical_support
        ON co.stay_id = mechanical_support.stay_id
            AND co.starttime < mechanical_support.charttime
            AND co.endtime >= mechanical_support.charttime
    GROUP BY co.stay_id, co.hr
)

select * from mechanical_support_hour