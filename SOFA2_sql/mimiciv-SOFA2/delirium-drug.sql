-- This query extracts dose+durations of vasopressin administration

DROP TABLE IF EXISTS `mimic-hr.derived.delirium_drug`;
CREATE TABLE `mimic-hr.derived.delirium_drug` AS 
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

,delirium_drug AS (
    SELECT co.stay_id, co.hadm_id, co.hr
    ,MAX(rate) as delirium_drug_rate
    FROM co
    LEFT JOIN `physionet-data.mimiciv_3_1_icu.inputevents` c
    ON co.stay_id = c.stay_id
        AND co.starttime < c.starttime
        AND co.endtime >= c.endtime
    WHERE c.itemid in (
        225150--Dexmedetomidine
        ,229420--Dexmedetomidine
        ,221824--Haloperidol (Haldol)
    )
    GROUP BY co.stay_id,co.hadm_id, co.hr
    
)

,delirium_drug_emar AS (
    SELECT co.stay_id, co.hadm_id, co.hr
    ,MAX(CASE WHEN event_txt = 'Administered' THEN 1 ELSE 0 END) as delirium_drug_rate
    FROM co
    LEFT JOIN `physionet-data.mimiciv_3_1_hosp.emar` c
    ON co.hadm_id = c.hadm_id
        AND co.starttime < c.charttime
        AND co.endtime >= c.charttime
    WHERE lower(c.medication) like 'quetiapine'
    OR lower(c.medication) like 'ziprasidone'
    OR lower(c.medication) like 'olanzapine'
    OR lower(c.medication) like 'zyprexa'
    GROUP BY co.stay_id, co.hadm_id, co.hr
    
)

select *except(hadm_id) from delirium_drug
union all
select *except(hadm_id) from delirium_drug_emar;