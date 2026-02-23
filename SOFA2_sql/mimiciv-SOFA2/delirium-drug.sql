-- Delirium drug administration per ICU hour for SOFA-2 neurological subscore
-- Footnote e: delirium drug → minimum brain score 1 (even if GCS = 15)
--
-- IV drugs (from inputevents): dexmedetomidine (225150, 229420), haloperidol (221824)
-- Oral drugs (from emar): quetiapine, ziprasidone, olanzapine/zyprexa

DROP TABLE IF EXISTS `mimic-hr.derived.delirium_drug`;
CREATE TABLE `mimic-hr.derived.delirium_drug` AS 

WITH co AS (
    SELECT ih.stay_id, ie.hadm_id
        , hr
        , DATETIME_SUB(ih.endtime, INTERVAL '1' HOUR) AS starttime
        , ih.endtime
    FROM `mimic-hr.derived.icustay_hourly` ih
    INNER JOIN `physionet-data.mimiciv_3_1_icu.icustays` ie
        ON ih.stay_id = ie.stay_id
)

-- IV delirium drugs: overlap join (hour endpoint falls within infusion interval)
, delirium_drug AS (
    SELECT co.stay_id, co.hadm_id, co.hr
        , MAX(rate) AS delirium_drug_rate
    FROM co
    LEFT JOIN `physionet-data.mimiciv_3_1_icu.inputevents` c
        ON co.stay_id = c.stay_id
            AND co.endtime > c.starttime
            AND co.endtime <= c.endtime
    WHERE c.itemid IN (225150, 229420, 221824)
    GROUP BY co.stay_id, co.hadm_id, co.hr
)

-- Oral delirium drugs: point-in-time from emar (only 'Administered' events)
, delirium_drug_emar AS (
    SELECT co.stay_id, co.hadm_id, co.hr
        , MAX(CASE WHEN event_txt = 'Administered' THEN 1 ELSE 0 END) AS delirium_drug_rate
    FROM co
    LEFT JOIN `physionet-data.mimiciv_3_1_hosp.emar` c
        ON co.hadm_id = c.hadm_id
            AND co.starttime < c.charttime
            AND co.endtime >= c.charttime
    WHERE LOWER(c.medication) LIKE 'quetiapine'
       OR LOWER(c.medication) LIKE 'ziprasidone'
       OR LOWER(c.medication) LIKE 'olanzapine'
       OR LOWER(c.medication) LIKE 'zyprexa'
    GROUP BY co.stay_id, co.hadm_id, co.hr
)

-- Deduplicate IV + oral into 1 row per (stay_id, hr)
SELECT stay_id, hr, MAX(delirium_drug_rate) AS delirium_drug_rate
FROM (
    SELECT * EXCEPT(hadm_id) FROM delirium_drug
    UNION ALL
    SELECT * EXCEPT(hadm_id) FROM delirium_drug_emar
)
GROUP BY stay_id, hr
