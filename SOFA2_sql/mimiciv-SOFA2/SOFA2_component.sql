-- SOFA-2 component assembly: one row per (stay_id, hr) with all raw inputs
-- Joins labs, vitals, vasopressors, ECMO, mechanical support, GCS, RRT,
-- urine output, delirium drugs, pH, bicarbonate, and potassium
-- Downstream: SOFA2 scoring script reads from this table

DROP TABLE IF EXISTS `mimic-hr.derived.sofa2_component`;
CREATE TABLE `mimic-hr.derived.sofa2_component` AS 

WITH co AS (
    SELECT ih.stay_id, ie.hadm_id
        , hr
        , DATETIME_SUB(ih.endtime, INTERVAL '1' HOUR) AS starttime
        , ih.endtime
    FROM `mimic-hr.derived.icustay_hourly` ih
    INNER JOIN `physionet-data.mimiciv_3_1_icu.icustays` ie
        ON ih.stay_id = ie.stay_id
)

-- PaO2/FiO2 and SpO2/FiO2 split by ventilation status
-- Scores 3-4 require ventilatory support
, pafi AS (
    SELECT ie.stay_id
        , bg.charttime
        , CASE WHEN vd.stay_id IS NULL THEN pao2fio2ratio ELSE NULL END AS pao2fio2ratio_novent
        , CASE WHEN vd.stay_id IS NOT NULL THEN pao2fio2ratio ELSE NULL END AS pao2fio2ratio_vent
        , CASE WHEN vd.stay_id IS NULL THEN spo2fio2ratio ELSE NULL END AS spo2fio2ratio_novent
        , CASE WHEN vd.stay_id IS NOT NULL THEN spo2fio2ratio ELSE NULL END AS spo2fio2ratio_vent
    FROM `physionet-data.mimiciv_3_1_icu.icustays` ie
    INNER JOIN `mimic-hr.derived.bg_spo2` bg
        ON ie.subject_id = bg.subject_id
    LEFT JOIN `mimic-hr.derived.ventilation` vd
        ON ie.stay_id = vd.stay_id
            AND bg.charttime >= vd.starttime
            AND bg.charttime <= vd.endtime
            AND vd.ventilation_status IN ('InvasiveVent', 'NonInvasiveVent', 'HFNC')
    WHERE specimen = 'ART.'
)

-- pH: MIN = worst (most acidotic) for footnote p RRT criteria
, bg_ph AS (
    SELECT co.hadm_id, co.hr
        , MIN(bg.ph) AS ph
    FROM co
    LEFT JOIN `mimic-hr.derived.bg` bg
        ON co.hadm_id = bg.hadm_id
            AND co.starttime < bg.charttime
            AND co.endtime >= bg.charttime
    GROUP BY co.hadm_id, co.hr
)

-- Bicarbonate: MIN = worst (lowest) for acidosis check
-- Potassium: MAX = worst (highest) for hyperkalemia check
, blood_chem AS (
    SELECT co.hadm_id, co.hr
        , MIN(chem.bicarbonate) AS bicarbonate
        , MAX(chem.potassium) AS potassium
    FROM co
    LEFT JOIN `mimic-hr.derived.chemistry` chem
        ON co.hadm_id = chem.hadm_id
            AND co.starttime < chem.charttime
            AND co.endtime >= chem.charttime
    GROUP BY co.hadm_id, co.hr
)

-- Mean arterial pressure
, vs AS (
    SELECT co.stay_id, co.hr
        , MIN(vs.mbp) AS meanbp_min
    FROM co
    LEFT JOIN `mimic-hr.derived.vitalsign` vs
        ON co.stay_id = vs.stay_id
            AND co.starttime < vs.charttime
            AND co.endtime >= vs.charttime
    GROUP BY co.stay_id, co.hr
)

-- Renal replacement therapy flags
, rrt AS (
    SELECT co.stay_id, co.hr
        , MAX(rrt.dialysis_active) AS dialysis_active
        , MAX(rrt.dialysis_present) AS dialysis_present
    FROM co
    LEFT JOIN `mimic-hr.derived.rrt` rrt
        ON co.stay_id = rrt.stay_id
            AND co.starttime < rrt.charttime
            AND co.endtime >= rrt.charttime
    GROUP BY co.stay_id, co.hr
)

-- GCS total + motor component (footnote d: motor fallback for intubated patients)
, gcs AS (
    SELECT co.stay_id, co.hr
        , MIN(gcs.gcs) AS gcs_min
        , MIN(gcs.gcs_motor) AS gcs_motor_min
    FROM co
    LEFT JOIN `mimic-hr.derived.gcs` gcs
        ON co.stay_id = gcs.stay_id
            AND co.starttime < gcs.charttime
            AND co.endtime >= gcs.charttime
    GROUP BY co.stay_id, co.hr
)

-- Bilirubin (MAX = worst)
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

-- Creatinine (MAX = worst)
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

-- Platelets (MIN = worst)
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

-- P/F and S/F ratios (MIN = worst)
, pf AS (
    SELECT co.stay_id, co.hr
        , MIN(pafi.pao2fio2ratio_novent) AS pao2fio2ratio_novent
        , MIN(pafi.pao2fio2ratio_vent) AS pao2fio2ratio_vent
        , MIN(pafi.spo2fio2ratio_novent) AS spo2fio2ratio_novent
        , MIN(pafi.spo2fio2ratio_vent) AS spo2fio2ratio_vent
    FROM co
    LEFT JOIN pafi
        ON co.stay_id = pafi.stay_id
            AND co.starttime < pafi.charttime
            AND co.endtime >= pafi.charttime
    GROUP BY co.stay_id, co.hr
)

-- Urine output rates at 6h, 12h, 24h windows
, uo AS (
    SELECT co.stay_id, co.hr
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

-- Assemble all components
, scorecomp AS (
    SELECT
        co.stay_id
        , co.hadm_id
        , co.hr
        , co.starttime, co.endtime
        -- ECMO
        , ecmo_hr.ECMO
        , ecmo_hr.ecmo_resp
        , ecmo_hr.ecmo_cv
        -- Mechanical support
        , ms_hr.mechanical_support
        -- Respiratory ratios
        , pf.pao2fio2ratio_novent
        , pf.pao2fio2ratio_vent
        , pf.spo2fio2ratio_novent
        , pf.spo2fio2ratio_vent
        -- Vasopressors
        , vaso.rate_epinephrine
        , vaso.rate_norepinephrine
        , vaso.rate_dopamine
        , vaso.rate_dobutamine
        , vaso.rate_milrinone
        , vaso.rate_vasopressin
        , vaso.rate_phenylephrine
        -- Vitals
        , vs.meanbp_min
        -- Neurological
        , gcs.gcs_min
        , gcs.gcs_motor_min
        -- Urine output
        , uo.uomlkghr_24hr
        , uo.uomlkghr_12hr
        , uo.uomlkghr_6hr
        -- Labs
        , bili.bilirubin_max
        , cr.creatinine_max
        , plt.platelet_min
        -- RRT
        , rrt.dialysis_present
        , rrt.dialysis_active
        -- Acid-base / electrolytes (for footnote p RRT criteria)
        , bg_ph.ph
        , blood_chem.bicarbonate
        , blood_chem.potassium
        -- Delirium
        , dd.delirium_drug_rate
    FROM co
    LEFT JOIN vs
        ON co.stay_id = vs.stay_id AND co.hr = vs.hr
    LEFT JOIN gcs
        ON co.stay_id = gcs.stay_id AND co.hr = gcs.hr
    LEFT JOIN bili
        ON co.stay_id = bili.stay_id AND co.hr = bili.hr
    LEFT JOIN cr
        ON co.stay_id = cr.stay_id AND co.hr = cr.hr
    LEFT JOIN plt
        ON co.stay_id = plt.stay_id AND co.hr = plt.hr
    LEFT JOIN pf
        ON co.stay_id = pf.stay_id AND co.hr = pf.hr
    LEFT JOIN uo
        ON co.stay_id = uo.stay_id AND co.hr = uo.hr
    LEFT JOIN `mimic-hr.derived.vaso_hourly` vaso
        ON co.stay_id = vaso.stay_id AND co.hr = vaso.hr
    LEFT JOIN `mimic-hr.derived.ECMO_hourly` ecmo_hr
        ON co.stay_id = ecmo_hr.stay_id AND co.hr = ecmo_hr.hr
    LEFT JOIN `mimic-hr.derived.mechanical_support_hourly` ms_hr
        ON co.stay_id = ms_hr.stay_id AND co.hr = ms_hr.hr
    LEFT JOIN rrt
        ON co.stay_id = rrt.stay_id AND co.hr = rrt.hr
    LEFT JOIN `mimic-hr.derived.delirium_drug` dd
        ON co.stay_id = dd.stay_id AND co.hr = dd.hr
    LEFT JOIN bg_ph
        ON co.hadm_id = bg_ph.hadm_id AND co.hr = bg_ph.hr
    LEFT JOIN blood_chem
        ON co.hadm_id = blood_chem.hadm_id AND co.hr = blood_chem.hr
)

SELECT * FROM scorecomp;
