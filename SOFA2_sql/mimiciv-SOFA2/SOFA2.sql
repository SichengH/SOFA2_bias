-- ------------------------------------------------------------------
-- Title: Sequential Organ Failure Assessment2(SOFA2)

-- ------------------------------------------------------------------
-- This script needs optimization, currently it taks 3.5h to run. 

DROP TABLE IF EXISTS `mimic-hr.derived.sofa2`;
CREATE TABLE `mimic-hr.derived.sofa2` AS 
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

,mechanical_support AS (
  select *
  from `physionet-data.mimiciv_3_1_icu.chartevents` chart 
  left join `physionet-data.mimiciv_3_1_icu.d_items` d
  on chart.itemid = d.itemid
  WHERE LOWER(label) LIKE '%balloon%'
   OR LOWER(label) LIKE '%iabp%'
   OR LOWER(label) LIKE '%impella%'
   OR LOWER(label) LIKE '%vad%'
   OR (label = 'Circuit Configuration (ECMO)' AND value = 'VV')
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
    FROM `physionet-data.mimiciv_3_1_icu.icustays` ie
    INNER JOIN `mimic-hr.derived.bg` bg
        ON ie.subject_id = bg.subject_id
    LEFT JOIN `mimic-hr.derived.ventilation` vd
        ON ie.stay_id = vd.stay_id
            AND bg.charttime >= vd.starttime
            AND bg.charttime <= vd.endtime
            AND vd.ventilation_status in  ('InvasiveVent','NonInvasiveVent','HFNC')
    WHERE specimen = 'ART.'
)

, bg_ph AS (

    SELECT co.hadm_id, co.hr
        -- vitals
        , MAX(bg.ph) AS ph
    FROM co
    LEFT JOIN `mimic-hr.derived.bg` bg
        ON co.hadm_id = bg.hadm_id
            AND co.starttime < bg.charttime
            AND co.endtime >= bg.charttime
    GROUP BY co.hadm_id, co.hr
)

, blood_chem AS (
    SELECT co.hadm_id, co.hr

        , MAX(chem.bicarbonate) AS bicarbonate
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

, gcs AS (
    SELECT co.stay_id, co.hr
        -- gcs
        , MIN(gcs.gcs) AS gcs_min
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
, vaso AS (
    SELECT
        co.stay_id
        , co.hr
        , MAX(epi.vaso_rate) AS rate_epinephrine
        , MAX(nor.vaso_rate) AS rate_norepinephrine
        , MAX(dop.vaso_rate) AS rate_dopamine
        , MAX(dob.vaso_rate) AS rate_dobutamine
        , MAX(mil.vaso_rate) AS rate_milrinone
        , MAX(vas.vaso_rate) AS rate_vasopressin
        , MAX(phe.vaso_rate) AS rate_phenylephrine
    FROM co
    LEFT JOIN `mimic-hr.derived.epinephrine` epi
        ON co.stay_id = epi.stay_id
            AND co.endtime > epi.starttime
            AND co.endtime <= epi.endtime
    LEFT JOIN `mimic-hr.derived.norepinephrine` nor
        ON co.stay_id = nor.stay_id
            AND co.endtime > nor.starttime
            AND co.endtime <= nor.endtime
    LEFT JOIN `mimic-hr.derived.dopamine` dop
        ON co.stay_id = dop.stay_id
            AND co.endtime > dop.starttime
            AND co.endtime <= dop.endtime
    LEFT JOIN `mimic-hr.derived.dobutamine` dob
        ON co.stay_id = dob.stay_id
            AND co.endtime > dob.starttime
            AND co.endtime <= dob.endtime

    LEFT JOIN `mimic-hr.derived.milrinone` mil
        ON co.stay_id = mil.stay_id
            AND co.endtime > mil.starttime
            AND co.endtime <= mil.endtime

    LEFT JOIN `mimic-hr.derived.vasopressin` vas
        ON co.stay_id = vas.stay_id
            AND co.endtime > vas.starttime
            AND co.endtime <= vas.endtime

    LEFT JOIN `mimic-hr.derived.phenylephrine` phe
        ON co.stay_id = phe.stay_id
            AND co.endtime > phe.starttime
            AND co.endtime <= phe.endtime
    WHERE epi.stay_id IS NOT NULL
        OR nor.stay_id IS NOT NULL
        OR dop.stay_id IS NOT NULL
        OR dob.stay_id IS NOT NULL
    GROUP BY co.stay_id, co.hr
)

, scorecomp AS (
    SELECT
        co.stay_id
        , co.hadm_id
        , co.hr
        , co.starttime, co.endtime
        , ecmo_hr.ECMO 
        , mechanical_support_hour.mechanical_support
        , pf.pao2fio2ratio_novent
        , pf.pao2fio2ratio_vent
        , vaso.rate_epinephrine
        , vaso.rate_norepinephrine
        , vaso.rate_dopamine
        , vaso.rate_dobutamine
        , vaso.rate_milrinone
        , vaso.rate_vasopressin
        , vaso.rate_phenylephrine
        , vs.meanbp_min
        , gcs.gcs_min
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
    LEFT JOIN vaso
        ON co.stay_id = vaso.stay_id
            AND co.hr = vaso.hr
    LEFT JOIN ecmo_hr
        ON co.stay_id = ecmo_hr.stay_id
            AND co.hr = ecmo_hr.hr
    LEFT JOIN mechanical_support_hour
        ON co.stay_id = ecmo_hr.stay_id
            AND co.hr = ecmo_hr.hr
    LEFT JOIN rrt
        ON co.stay_id = rrt.stay_id
            AND co.hr = rrt.hr
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

, scorecalc AS (
    -- Calculate the final score
    -- note that if the underlying data is missing,
    -- the component is null
    -- eventually these are treated as 0 (normal),
    -- but knowing when data is missing is useful for debugging
    SELECT scorecomp.*
        -- Respiration
        , CASE
            WHEN ECMO = 1 
            OR pao2fio2ratio_vent < 75 THEN 4
            WHEN pao2fio2ratio_vent < 150 THEN 3
            WHEN pao2fio2ratio_novent < 225 THEN 2
            WHEN pao2fio2ratio_vent < 225 THEN 2
            WHEN pao2fio2ratio_novent < 300 THEN 1
            WHEN pao2fio2ratio_vent < 300 THEN 1
            WHEN
                COALESCE(
                    pao2fio2ratio_vent, pao2fio2ratio_novent
                ) IS NULL THEN null
            ELSE 0
        END AS respiration

        -- Coagulation
        , CASE
            WHEN platelet_min < 50 THEN 4
            WHEN platelet_min < 80 THEN 3
            WHEN platelet_min < 100 THEN 2
            WHEN platelet_min < 150 THEN 1
            WHEN platelet_min IS NULL THEN null
            ELSE 0
        END AS coagulation

        -- Liver
        , CASE
            -- Bilirubin checks in mg/dL
            WHEN bilirubin_max >= 12.0 THEN 4
            WHEN bilirubin_max >= 6.0 THEN 3
            WHEN bilirubin_max >= 3.0 THEN 2
            WHEN bilirubin_max >= 1.2 THEN 1
            WHEN bilirubin_max IS NULL THEN null
            ELSE 0
        END AS liver

        -- Cardiovascular
        , CASE
            WHEN rate_dopamine > 15
                OR rate_epinephrine > 0.1
                OR rate_norepinephrine > 0.1
                OR mechanical_support = 1
                THEN 4
            WHEN rate_dopamine > 5
                OR rate_epinephrine <= 0.1
                OR rate_norepinephrine <= 0.1
                THEN 3
            WHEN rate_dopamine > 0
                OR rate_dobutamine > 0
                THEN 2
            WHEN meanbp_min < 70 THEN 1
            WHEN
                COALESCE(
                    meanbp_min
                    , rate_dopamine
                    , rate_dobutamine
                    , rate_epinephrine
                    , rate_norepinephrine
                ) IS NULL THEN null
            ELSE 0
        END AS cardiovascular

        -- Neurological failure (GCS)
        , CASE
            WHEN (gcs_min >= 13 AND gcs_min <= 14) THEN 1
            WHEN (gcs_min >= 10 AND gcs_min <= 12) or delirium_drug_rate > 0 THEN 2
            WHEN (gcs_min >= 6 AND gcs_min <= 9) THEN 3
            WHEN gcs_min < 6 THEN 4
            WHEN gcs_min IS NULL THEN null
            ELSE 0
        END AS cns

        -- Renal failure - high creatinine or low urine output
        , CASE
            WHEN dialysis_present = 1 THEN 4
            WHEN (creatinine_max >= 3.5 AND creatinine_max < 5.0) THEN 3
            WHEN uomlkghr_24hr < 0.3 THEN 3
            WHEN (creatinine_max >= 2.0 AND creatinine_max < 3.5) 
            OR uomlkghr_12hr < 0.3 THEN 2
            WHEN (creatinine_max >= 1.2 AND creatinine_max < 2.0) 
            OR uomlkghr_6hr < 0.5 THEN 1
            WHEN COALESCE(uomlkghr_24hr,uomlkghr_12hr,uomlkghr_6hr, creatinine_max) IS NULL THEN null
            ELSE 0
        END AS renal
    FROM scorecomp
)

, score_final AS (
    SELECT s.*
        -- Combine all the scores to get SOFA
        -- Impute 0 if the score is missing
        -- the window function takes the max over the last 24 hours
        , COALESCE(
            MAX(respiration) OVER w
            , 0) AS respiration_24hours
        , COALESCE(
            MAX(coagulation) OVER w
            , 0) AS coagulation_24hours
        , COALESCE(
            MAX(liver) OVER w
            , 0) AS liver_24hours
        , COALESCE(
            MAX(cardiovascular) OVER w
            , 0) AS cardiovascular_24hours
        , COALESCE(
            MAX(cns) OVER w
            , 0) AS cns_24hours
        , COALESCE(
            MAX(renal) OVER w
            , 0) AS renal_24hours

        -- sum together data for final SOFA
        , COALESCE(
            MAX(respiration) OVER w
            , 0)
        + COALESCE(
            MAX(coagulation) OVER w
            , 0)
        + COALESCE(
            MAX(liver) OVER w
            , 0)
        + COALESCE(
            MAX(cardiovascular) OVER w
            , 0)
        + COALESCE(
            MAX(cns) OVER w
            , 0)
        + COALESCE(
            MAX(renal) OVER w
            , 0)
        AS sofa_24hours
    FROM scorecalc s
    WINDOW w AS
        (
            PARTITION BY stay_id
            ORDER BY hr
            ROWS BETWEEN 23 PRECEDING AND 0 FOLLOWING
        )
)

SELECT * FROM score_final
WHERE hr >= 0;
