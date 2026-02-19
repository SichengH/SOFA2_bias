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
--update information about ECMO
    SELECT co.stay_id, co.hr
        -- vitals
        , CASE WHEN MAX(ecmo.value) is not null then 1 else 0 end as ECMO
    FROM co
    LEFT JOIN ecmo
        ON co.stay_id = ecmo.stay_id
            AND co.starttime < ecmo.charttime
            AND co.endtime >= ecmo.charttime
    GROUP BY co.stay_id, co.hr
)

select * from ecmo_hr