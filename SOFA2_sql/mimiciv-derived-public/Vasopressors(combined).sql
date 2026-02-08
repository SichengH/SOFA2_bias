-- This query extracts dose+durations of dobutamine administration
-- Local hospital dosage guidance: 2 mcg/kg/min (low) - 40 mcg/kg/min (max)
DROP TABLE IF EXISTS `mimic-hr.derived.dobutamine`;
CREATE TABLE `mimic-hr.derived.dobutamine` AS 
SELECT
    stay_id, linkorderid
    -- all rows in mcg/kg/min
    , rate AS vaso_rate
    , amount AS vaso_amount
    , starttime
    , endtime
FROM `physionet-data.mimiciv_3_1_icu.inputevents`
WHERE itemid = 221653; -- dobutamine;

-- This query extracts dose+durations of dopamine administration
-- Local hospital dosage guidance: 2 mcg/kg/min (low) - 10 mcg/kg/min (high)
DROP TABLE IF EXISTS `mimic-hr.derived.dopamine`;
CREATE TABLE `mimic-hr.derived.dopamine` AS 
SELECT
    stay_id, linkorderid
    -- all rows in mcg/kg/min
    , rate AS vaso_rate
    , amount AS vaso_amount
    , starttime
    , endtime
FROM `physionet-data.mimiciv_3_1_icu.inputevents`
WHERE itemid = 221662; -- dopamine;

-- This query extracts dose+durations of epinephrine administration
-- Local hospital dosage guidance: 0.2 mcg/kg/min (low) - 2 mcg/kg/min (high)
DROP TABLE IF EXISTS `mimic-hr.derived.epinephrine`;
CREATE TABLE `mimic-hr.derived.epinephrine` AS 
SELECT
    stay_id, linkorderid
    -- all rows in mcg/kg/min
    , rate AS vaso_rate
    , amount AS vaso_amount
    , starttime
    , endtime
FROM `physionet-data.mimiciv_3_1_icu.inputevents`
WHERE itemid = 221289; -- epinephrine;


-- This query extracts dose+durations of milrinone administration
-- Local hospital dosage guidance: 0.5 mcg/kg/min (usual)
DROP TABLE IF EXISTS `mimic-hr.derived.milrinone`;
CREATE TABLE `mimic-hr.derived.milrinone` AS 
SELECT
    stay_id, linkorderid
    -- all rows in mcg/kg/min
    , rate AS vaso_rate
    , amount AS vaso_amount
    , starttime
    , endtime
FROM `physionet-data.mimiciv_3_1_icu.inputevents`
WHERE itemid = 221986; -- milrinone;


-- This query extracts dose+durations of norepinephrine administration
-- Local hospital dosage guidance: 0.03 mcg/kg/min (low), 0.5 mcg/kg/min (high)
DROP TABLE IF EXISTS `mimic-hr.derived.norepinephrine`;
CREATE TABLE `mimic-hr.derived.norepinephrine` AS 
SELECT
    stay_id, linkorderid
    -- two rows in mg/kg/min... rest in mcg/kg/min
    -- the rows in mg/kg/min are documented incorrectly
    -- all rows converted into mcg/kg/min (equiv to ug/kg/min)
    , CASE WHEN rateuom = 'mg/kg/min' AND patientweight = 1 THEN rate
        -- below row is written for completion, but doesn't impact rows
        WHEN rateuom = 'mg/kg/min' THEN rate * 1000.0
        ELSE rate END AS vaso_rate
    , amount AS vaso_amount
    , starttime
    , endtime
FROM `physionet-data.mimiciv_3_1_icu.inputevents`
WHERE itemid = 221906; -- norepinephrine;

-- This query extracts dose+durations of phenylephrine administration
-- Local hospital dosage guidance: 0.5 mcg/kg/min (low) - 5 mcg/kg/min (high)
DROP TABLE IF EXISTS `mimic-hr.derived.phenylephrine`;
CREATE TABLE `mimic-hr.derived.phenylephrine` AS 
SELECT
    stay_id, linkorderid
    -- one row in mcg/min, the rest in mcg/kg/min
    , CASE WHEN rateuom = 'mcg/min' THEN rate / patientweight
        ELSE rate END AS vaso_rate
    , amount AS vaso_amount
    , starttime
    , endtime
FROM `physionet-data.mimiciv_3_1_icu.inputevents`
WHERE itemid = 221749; -- phenylephrine;


-- This query extracts dose+durations of vasopressin administration
-- Local hospital dosage guidance: 1.2 units/hour (low) - 2.4 units/hour (high)
DROP TABLE IF EXISTS `mimic-hr.derived.vasopressin`;
CREATE TABLE `mimic-hr.derived.vasopressin` AS 
SELECT
    stay_id, linkorderid
    -- three rows in units/min, rest in units/hour
    -- the three rows in units/min look reasonable and
    -- fit with the patient course

    -- convert all rows to units/hour
    , CASE WHEN rateuom = 'units/min' THEN rate * 60.0
        ELSE rate END AS vaso_rate
    , amount AS vaso_amount
    , starttime
    , endtime
FROM `physionet-data.mimiciv_3_1_icu.inputevents`
WHERE itemid = 222315; -- vasopressin;

