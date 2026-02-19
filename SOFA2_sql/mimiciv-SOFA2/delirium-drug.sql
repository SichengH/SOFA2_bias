-- This query extracts delirium drug administration per ICU hour.

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

-- ==================================================================
-- [CHANGE-04] Fixed IV delirium drug JOIN condition.
-- OLD: co.starttime < c.starttime AND co.endtime >= c.endtime
--   This required the ENTIRE infusion interval to fit inside a 1-hour
--   window. A 4-hour dexmedetomidine infusion (08:00-12:00) would NOT
--   match any hourly window, causing massive under-detection.
-- NEW: co.endtime > c.starttime AND co.endtime <= c.endtime
--   Now same overlap pattern as vaso_hourly.sql — checks if the hour's
--   endpoint falls within the infusion interval ("was drug active at
--   the end of this hour?").
-- [END CHANGE-04]
-- ==================================================================

,delirium_drug AS (
    SELECT co.stay_id, co.hadm_id, co.hr
    ,MAX(rate) as delirium_drug_rate
    FROM co
    LEFT JOIN `physionet-data.mimiciv_3_1_icu.inputevents` c
    ON co.stay_id = c.stay_id
        AND co.endtime > c.starttime      -- [CHANGE-04] was: co.starttime < c.starttime
        AND co.endtime <= c.endtime        -- [CHANGE-04] was: co.endtime >= c.endtime
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

-- ==================================================================
-- [CHANGE-05] Added deduplication wrapper around UNION ALL.
-- OLD: select *except(hadm_id) from delirium_drug
--      union all
--      select *except(hadm_id) from delirium_drug_emar
--   A patient receiving both IV dex and oral quetiapine in the same
--   hour gets duplicate (stay_id, hr) rows, which inflates other
--   component values when joined in SOFA2_component (fan-out).
-- NEW: Wrap in GROUP BY with MAX to collapse to 1 row per (stay_id, hr).
-- [END CHANGE-05]
-- ==================================================================
SELECT stay_id, hr, MAX(delirium_drug_rate) AS delirium_drug_rate
FROM (
    select *except(hadm_id) from delirium_drug
    union all
    select *except(hadm_id) from delirium_drug_emar
)
GROUP BY stay_id, hr
