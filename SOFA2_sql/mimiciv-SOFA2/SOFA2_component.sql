DROP TABLE IF EXISTS `mimic-hr.derived.sofa2_component`;
CREATE TABLE `mimic-hr.derived.sofa2_component` AS 
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
, pafi AS (
    -- join blood gas to ventilation durations to determine if patient was vent
    SELECT ie.stay_id
        , bg.charttime
        -- because pafi has an interaction between vent/PaO2:FiO2,
        -- we need two columns for the score
        -- it can happen that the lowest unventilated PaO2/FiO2 is 68,
        -- but the lowest ventilated PaO2/FiO2 is 120
        -- in this case, the SOFA score is 3, *not* 4.
        , CASE
            WHEN vd.stay_id IS NULL THEN pao2fio2ratio ELSE null
        END AS pao2fio2ratio_novent
        , CASE
            WHEN vd.stay_id IS NOT NULL THEN pao2fio2ratio ELSE null
        END AS pao2fio2ratio_vent
        , CASE
            WHEN vd.stay_id IS NULL THEN spo2fio2ratio ELSE null
        END AS spo2fio2ratio_novent
        , CASE
            WHEN vd.stay_id IS NOT NULL THEN spo2fio2ratio ELSE null
        END AS spo2fio2ratio_vent
    FROM `physionet-data.mimiciv_3_1_icu.icustays` ie
    INNER JOIN `mimic-hr.derived.bg_spo2` bg
        ON ie.subject_id = bg.subject_id
    LEFT JOIN `mimic-hr.derived.ventilation` vd
        ON ie.stay_id = vd.stay_id
            AND bg.charttime >= vd.starttime
            AND bg.charttime <= vd.endtime
            AND vd.ventilation_status in  ('InvasiveVent','NonInvasiveVent','HFNC')
    WHERE specimen = 'ART.'
)

-- ==================================================================
-- [CHANGE-06] pH aggregation changed from MAX to MIN (footnote p).
-- For RRT criteria, we need the WORST (most acidotic) pH.
-- OLD: MAX(bg.ph) AS ph
-- NEW: MIN(bg.ph) AS ph
-- A patient with pH 7.15 and 7.35 in the same hour now correctly
-- shows ph=7.15 (triggering pH <= 7.20 check) instead of ph=7.35.
-- [END CHANGE-06]
-- ==================================================================
, bg_ph AS (

    SELECT co.hadm_id, co.hr
        -- vitals
        , MIN(bg.ph) AS ph            -- [CHANGE-06] was: MAX(bg.ph)
    FROM co
    LEFT JOIN `mimic-hr.derived.bg` bg
        ON co.hadm_id = bg.hadm_id
            AND co.starttime < bg.charttime
            AND co.endtime >= bg.charttime
    GROUP BY co.hadm_id, co.hr
)

-- ==================================================================
-- [CHANGE-07] Bicarbonate aggregation changed from MAX to MIN (footnote p).
-- For metabolic acidosis detection (bicarbonate <= 12), we need the
-- WORST (lowest) bicarbonate value.
-- OLD: MAX(chem.bicarbonate) AS bicarbonate
-- NEW: MIN(chem.bicarbonate) AS bicarbonate
-- [END CHANGE-07]
-- ==================================================================
, blood_chem AS (
    SELECT co.hadm_id, co.hr

        , MIN(chem.bicarbonate) AS bicarbonate  -- [CHANGE-07] was: MAX(chem.bicarbonate)
        , MAX(chem.potassium) AS potassium
    FROM co
    LEFT JOIN `mimic-hr.derived.chemistry` chem
        ON co.hadm_id = chem.hadm_id
            AND co.starttime < chem.charttime
            AND co.endtime >= chem.charttime
    GROUP BY co.hadm_id, co.hr
)
, vs AS (

    SELECT co.stay_id, co.hr
        -- vitals
        , MIN(vs.mbp) AS meanbp_min
    FROM co
    LEFT JOIN `mimic-hr.derived.vitalsign` vs
        ON co.stay_id = vs.stay_id
            AND co.starttime < vs.charttime
            AND co.endtime >= vs.charttime
    GROUP BY co.stay_id, co.hr
)

, rrt AS (

    SELECT co.stay_id, co.hr
        -- rrt
        , MAX(rrt.dialysis_active) AS dialysis_active --dialysis active instead of dialysis present
        , MAX(rrt.dialysis_present) AS dialysis_present --dialysis active instead of dialysis present
    FROM co
    LEFT JOIN `mimic-hr.derived.rrt` rrt
        ON co.stay_id = rrt.stay_id
            AND co.starttime < rrt.charttime
            AND co.endtime >= rrt.charttime
    GROUP BY co.stay_id, co.hr
)

-- ==================================================================
-- [CHANGE-08] Added gcs_motor (footnote d: motor fallback).
-- When gcs_total is unavailable (e.g. intubated patients), use
-- gcs_motor with the following mapping:
--   Motor 6 → score 0, Motor 5 → 1, Motor 4 → 2, Motor 3 → 3, Motor 1-2 → 4
-- OLD: only MIN(gcs.gcs) AS gcs_min
-- NEW: also MIN(gcs.gcs_motor) AS gcs_motor_min
-- [END CHANGE-08]
-- ==================================================================
, gcs AS (
    SELECT co.stay_id, co.hr
        -- gcs
        , MIN(gcs.gcs) AS gcs_min
        , MIN(gcs.gcs_motor) AS gcs_motor_min      -- [CHANGE-08] added motor fallback
    FROM co
    LEFT JOIN `mimic-hr.derived.gcs` gcs
        ON co.stay_id = gcs.stay_id
            AND co.starttime < gcs.charttime
            AND co.endtime >= gcs.charttime
    GROUP BY co.stay_id, co.hr
)

, bili AS (
    SELECT co.stay_id, co.hr
        , MAX(enz.bilirubin_total) AS bilirubin_max
    FROM co
    LEFT JOIN `mimic-hr.derived.enzyme` enz
        ON co.hadm_id = enz.hadm_id
            AND co.starttime < enz.charttime
            AND co.endtime >= enz.charttime
    GROUP BY co.stay_id, co.hr
)

, cr AS (
    SELECT co.stay_id, co.hr
        , MAX(chem.creatinine) AS creatinine_max
    FROM co
    LEFT JOIN `mimic-hr.derived.chemistry` chem
        ON co.hadm_id = chem.hadm_id
            AND co.starttime < chem.charttime
            AND co.endtime >= chem.charttime
    GROUP BY co.stay_id, co.hr
)

, plt AS (
    SELECT co.stay_id, co.hr
        , MIN(cbc.platelet) AS platelet_min
    FROM co
    LEFT JOIN `mimic-hr.derived.complete_blood_count` cbc
        ON co.hadm_id = cbc.hadm_id
            AND co.starttime < cbc.charttime
            AND co.endtime >= cbc.charttime
    GROUP BY co.stay_id, co.hr
)

, pf AS (
    SELECT co.stay_id, co.hr
        , MIN(pafi.pao2fio2ratio_novent) AS pao2fio2ratio_novent
        , MIN(pafi.pao2fio2ratio_vent) AS pao2fio2ratio_vent
        , MIN(pafi.spo2fio2ratio_novent) AS spo2fio2ratio_novent
        , MIN(pafi.spo2fio2ratio_vent) AS spo2fio2ratio_vent
    FROM co
    -- bring in blood gases that occurred during this hour
    LEFT JOIN pafi
        ON co.stay_id = pafi.stay_id
            AND co.starttime < pafi.charttime
            AND co.endtime >= pafi.charttime
    GROUP BY co.stay_id, co.hr
)

-- sum uo separately to prevent duplicating values
, uo AS (
    SELECT co.stay_id, co.hr
        -- uo
        , MAX(uo_mlkghr_24hr) AS uomlkghr_24hr
        , MAX(uo_mlkghr_12hr) AS uomlkghr_12hr
        , MAX(uo_mlkghr_6hr) AS uomlkghr_6hr
    FROM co
    LEFT JOIN `mimic-hr.derived.urine_output_rate` uo
        ON co.stay_id = uo.stay_id
            AND co.starttime < uo.charttime
            AND co.endtime >= uo.charttime
    GROUP BY co.stay_id, co.hr
)

-- collapse vasopressors into 1 row per hour
-- also ensures only 1 row per chart time

, scorecomp AS (
    SELECT
        co.stay_id
        , co.hadm_id
        , co.hr
        , co.starttime, co.endtime
        , ecmo_hr.ECMO 
        -- ============================================================
        -- [CHANGE-09] Added ecmo_resp and ecmo_cv columns from
        -- updated ECMO_hourly table (see CHANGE-01). These enable
        -- correct VV/VA scoring in SOFA2 (2).sql.
        , ecmo_hr.ecmo_resp               -- [CHANGE-09]
        , ecmo_hr.ecmo_cv                 -- [CHANGE-09]
        -- [END CHANGE-09]
        -- ============================================================
        , ms_hr.mechanical_support
        , pf.pao2fio2ratio_novent
        , pf.pao2fio2ratio_vent
        , pf.spo2fio2ratio_novent
        , pf.spo2fio2ratio_vent
        , vaso.rate_epinephrine
        , vaso.rate_norepinephrine
        , vaso.rate_dopamine
        , vaso.rate_dobutamine
        , vaso.rate_milrinone
        , vaso.rate_vasopressin
        , vaso.rate_phenylephrine
        , vs.meanbp_min
        , gcs.gcs_min
        , gcs.gcs_motor_min              -- [CHANGE-08] added motor fallback
        -- uo
        , uo.uomlkghr_24hr
        , uo.uomlkghr_12hr
        , uo.uomlkghr_6hr
        -- labs
        , bili.bilirubin_max
        , cr.creatinine_max
        , plt.platelet_min
        , rrt.dialysis_present
        , rrt.dialysis_active
        , bg_ph.ph
        , blood_chem.bicarbonate
        , blood_chem.potassium
        , dd.delirium_drug_rate
    FROM co
    LEFT JOIN vs
        ON co.stay_id = vs.stay_id
            AND co.hr = vs.hr
    LEFT JOIN gcs
        ON co.stay_id = gcs.stay_id
            AND co.hr = gcs.hr
    LEFT JOIN bili
        ON co.stay_id = bili.stay_id
            AND co.hr = bili.hr
    LEFT JOIN cr
        ON co.stay_id = cr.stay_id
            AND co.hr = cr.hr
    LEFT JOIN plt
        ON co.stay_id = plt.stay_id
            AND co.hr = plt.hr
    LEFT JOIN pf
        ON co.stay_id = pf.stay_id
            AND co.hr = pf.hr
    LEFT JOIN uo
        ON co.stay_id = uo.stay_id
            AND co.hr = uo.hr
    LEFT JOIN `mimic-hr.derived.vaso_hourly` vaso
        ON co.stay_id = vaso.stay_id
            AND co.hr = vaso.hr
    LEFT JOIN `mimic-hr.derived.ECMO_hourly` ecmo_hr
        ON co.stay_id = ecmo_hr.stay_id
            AND co.hr = ecmo_hr.hr
    LEFT JOIN `mimic-hr.derived.mechanical_support_hourly` ms_hr
        ON co.stay_id = ms_hr.stay_id
            AND co.hr = ms_hr.hr
    LEFT JOIN rrt
        ON co.stay_id = rrt.stay_id
            AND co.hr = rrt.hr
    -- ==================================================================
    -- [CHANGE-10] Delirium drug dedup is now handled in the source
    -- table (delirium-drug.sql, CHANGE-05). If using the OLD source
    -- table without dedup, uncomment the subquery wrapper below.
    -- OLD: LEFT JOIN `mimic-hr.derived.delirium_drug` dd
    -- (This could produce duplicate rows from UNION ALL of IV + oral)
    -- [END CHANGE-10]
    -- ==================================================================
    LEFT JOIN `mimic-hr.derived.delirium_drug` dd
        ON co.stay_id = dd.stay_id
            AND co.hr = dd.hr
    LEFT JOIN bg_ph 
        ON co.hadm_id = bg_ph.hadm_id
            AND co.hr = bg_ph.hr
    LEFT JOIN blood_chem
        ON co.hadm_id = blood_chem.hadm_id
            AND co.hr = blood_chem.hr
)

SELECT * FROM scorecomp;
