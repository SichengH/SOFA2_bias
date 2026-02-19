-- Sequential Organ Failure Assessment 2 (SOFA-2)
-- Aligned with Ranzani et al. (2025) SOFA-2 specification

DROP TABLE IF EXISTS `mimic-hr.derived.sofa2`;
CREATE TABLE `mimic-hr.derived.sofa2` AS
WITH co AS (
    SELECT
        ih.stay_id,
        ie.hadm_id,
        hr,
        -- start/endtime define the 1-hour window [starttime, endtime]
        DATETIME_SUB(ih.endtime, INTERVAL '1' HOUR) AS starttime,
        ih.endtime
    FROM `mimic-hr.derived.icustay_hourly` ih
    INNER JOIN `physionet-data.mimiciv_3_1_icu.icustays` ie
        ON ih.stay_id = ie.stay_id
)

-- =========================================================================
-- ECMO: split into respiratory (VV) and cardiovascular (VA) flags
-- Operationalization:
--   - ecmo_any: any ECMO presence in hour
--   - ecmo_resp: value='VV' in hour (respiratory ECMO)
--   - ecmo_cv:   value='VA' in hour (cardiovascular ECMO)


, ecmo AS (
    SELECT
        chart.stay_id,
        chart.charttime,
        chart.value
    FROM `physionet-data.mimiciv_3_1_icu.chartevents` chart
    INNER JOIN `physionet-data.mimiciv_3_1_icu.d_items` d
        ON chart.itemid = d.itemid
    WHERE d.label LIKE '%ECMO%'
       OR d.category = 'ECMO'
)

, ecmo_hr AS (
    SELECT
        co.stay_id,
        co.hr,
        MAX(CASE WHEN ecmo.value IS NOT NULL THEN 1 ELSE 0 END) AS ecmo_any,
        MAX(CASE WHEN ecmo.value = 'VV' THEN 1 ELSE 0 END) AS ecmo_resp,
        MAX(CASE WHEN ecmo.value = 'VA' THEN 1 ELSE 0 END) AS ecmo_cv
    FROM co
    LEFT JOIN ecmo
        ON co.stay_id = ecmo.stay_id
       AND co.starttime < ecmo.charttime
       AND co.endtime   >= ecmo.charttime
    GROUP BY co.stay_id, co.hr
)

-- =========================================================================
-- Mechanical circulatory support (non-ECMO): IABP, Impella, VAD


, mechanical_support AS (
    SELECT
        chart.stay_id,
        chart.charttime,
        chart.value
    FROM `physionet-data.mimiciv_3_1_icu.chartevents` chart
    INNER JOIN `physionet-data.mimiciv_3_1_icu.d_items` d
        ON chart.itemid = d.itemid
    WHERE LOWER(d.label) LIKE '%balloon%'
       OR LOWER(d.label) LIKE '%iabp%'
       OR LOWER(d.label) LIKE '%impella%'
       OR LOWER(d.label) LIKE '%vad%'
)

, mechanical_support_hour AS (
    SELECT
        co.stay_id,
        co.hr,
        CASE WHEN MAX(mechanical_support.value) IS NOT NULL THEN 1 ELSE 0 END AS mechanical_support
    FROM co
    LEFT JOIN mechanical_support
        ON co.stay_id = mechanical_support.stay_id
       AND co.starttime < mechanical_support.charttime
       AND co.endtime   >= mechanical_support.charttime
    GROUP BY co.stay_id, co.hr
)

-- =========================================================================
-- PaO2/FiO2 ratio with ventilation status

-- We split PaO2/FiO2 into:
--   - novent: ABG not during an "advanced vent support" interval
--   - vent:   ABG during invasive/NIV/HFNC interval
--
-- SOFA-2: scores 3-4 require advanced vent support, but score 2 can be assigned without advanced vent support.

, pafi AS (
    SELECT
        ie.stay_id,
        bg.charttime,
        CASE WHEN vd.stay_id IS NULL THEN pao2fio2ratio ELSE NULL END AS pao2fio2ratio_novent,
        CASE WHEN vd.stay_id IS NOT NULL THEN pao2fio2ratio ELSE NULL END AS pao2fio2ratio_vent
    FROM `physionet-data.mimiciv_3_1_icu.icustays` ie
    INNER JOIN `mimic-hr.derived.bg` bg
        ON ie.subject_id = bg.subject_id
    LEFT JOIN `mimic-hr.derived.ventilation` vd
        ON ie.stay_id = vd.stay_id
       AND bg.charttime >= vd.starttime
       AND bg.charttime <= vd.endtime
       AND vd.ventilation_status IN ('InvasiveVent', 'NonInvasiveVent', 'HFNC')
    WHERE specimen = 'ART.'
)

-- =========================================================================
-- Labs: pH, bicarbonate, potassium
-- Retained from original for completeness and potential future use.

, bg_ph AS (
    SELECT
        co.hadm_id,
        co.hr,
        MAX(bg.ph) AS ph
    FROM co
    LEFT JOIN `mimic-hr.derived.bg` bg
        ON co.hadm_id = bg.hadm_id
       AND co.starttime < bg.charttime
       AND co.endtime   >= bg.charttime
    GROUP BY co.hadm_id, co.hr
)

, blood_chem AS (
    SELECT
        co.hadm_id,
        co.hr,
        MAX(chem.bicarbonate) AS bicarbonate,
        MAX(chem.potassium) AS potassium
    FROM co
    LEFT JOIN `mimic-hr.derived.chemistry` chem
        ON co.hadm_id = chem.hadm_id
       AND co.starttime < chem.charttime
       AND co.endtime   >= chem.charttime
    GROUP BY co.hadm_id, co.hr
)

-- =========================================================================
-- Vitals: MAP (mean blood pressure)

, vs AS (
    SELECT
        co.stay_id,
        co.hr,
        MIN(vs.mbp) AS meanbp_min
    FROM co
    LEFT JOIN `mimic-hr.derived.vitalsign` vs
        ON co.stay_id = vs.stay_id
       AND co.starttime < vs.charttime
       AND co.endtime   >= vs.charttime
    GROUP BY co.stay_id, co.hr
)

-- =========================================================================
-- RRT (dialysis flags)

, rrt AS (
    SELECT
        co.stay_id,
        co.hr,
        MAX(rrt.dialysis_active) AS dialysis_active,
        MAX(rrt.dialysis_present) AS dialysis_present
    FROM co
    LEFT JOIN `mimic-hr.derived.rrt` rrt
        ON co.stay_id = rrt.stay_id
       AND co.starttime < rrt.charttime
       AND co.endtime   >= rrt.charttime
    GROUP BY co.stay_id, co.hr
)

-- =========================================================================
-- GCS

, gcs AS (
    SELECT
        co.stay_id,
        co.hr,
        MIN(gcs.gcs) AS gcs_min
    FROM co
    LEFT JOIN `mimic-hr.derived.gcs` gcs
        ON co.stay_id = gcs.stay_id
       AND co.starttime < gcs.charttime
       AND co.endtime   >= gcs.charttime
    GROUP BY co.stay_id, co.hr
)

-- =========================================================================
-- Labs: bilirubin, creatinine, platelets

, bili AS (
    SELECT
        co.stay_id,
        co.hr,
        MAX(enz.bilirubin_total) AS bilirubin_max
    FROM co
    LEFT JOIN `mimic-hr.derived.enzyme` enz
        ON co.hadm_id = enz.hadm_id
       AND co.starttime < enz.charttime
       AND co.endtime   >= enz.charttime
    GROUP BY co.stay_id, co.hr
)

, cr AS (
    SELECT
        co.stay_id,
        co.hr,
        MAX(chem.creatinine) AS creatinine_max
    FROM co
    LEFT JOIN `mimic-hr.derived.chemistry` chem
        ON co.hadm_id = chem.hadm_id
       AND co.starttime < chem.charttime
       AND co.endtime   >= chem.charttime
    GROUP BY co.stay_id, co.hr
)

, plt AS (
    SELECT
        co.stay_id,
        co.hr,
        MIN(cbc.platelet) AS platelet_min
    FROM co
    LEFT JOIN `mimic-hr.derived.complete_blood_count` cbc
        ON co.hadm_id = cbc.hadm_id
       AND co.starttime < cbc.charttime
       AND co.endtime   >= cbc.charttime
    GROUP BY co.stay_id, co.hr
)

-- =========================================================================
-- PaO2/FiO2 ratio hourly aggregation

, pf AS (
    SELECT
        co.stay_id,
        co.hr,
        MIN(pafi.pao2fio2ratio_novent) AS pao2fio2ratio_novent,
        MIN(pafi.pao2fio2ratio_vent)   AS pao2fio2ratio_vent
    FROM co
    LEFT JOIN pafi
        ON co.stay_id = pafi.stay_id
       AND co.starttime < pafi.charttime
       AND co.endtime   >= pafi.charttime
    GROUP BY co.stay_id, co.hr
)

-- =========================================================================
-- Urine output rate (mL/kg/h) over rolling windows (6h, 12h, 24h)

, uo AS (
    SELECT
        co.stay_id,
        co.hr,
        MAX(uo_mlkghr_24hr) AS uomlkghr_24hr,
        MAX(uo_mlkghr_12hr) AS uomlkghr_12hr,
        MAX(uo_mlkghr_6hr)  AS uomlkghr_6hr
    FROM co
    LEFT JOIN `mimic-hr.derived.urine_output_rate` uo
        ON co.stay_id = uo.stay_id
       AND co.starttime < uo.charttime
       AND co.endtime   >= uo.charttime
    GROUP BY co.stay_id, co.hr
)

-- =========================================================================
-- Vasopressors/inotropes
-- Rates are NULL when agent not active.

, vaso AS (
    SELECT
        co.stay_id,
        co.hr,
        MAX(epi.vaso_rate) AS rate_epinephrine,
        MAX(nor.vaso_rate) AS rate_norepinephrine,
        MAX(dop.vaso_rate) AS rate_dopamine,
        MAX(dob.vaso_rate) AS rate_dobutamine,
        MAX(mil.vaso_rate) AS rate_milrinone,
        MAX(vas.vaso_rate) AS rate_vasopressin,
        MAX(phe.vaso_rate) AS rate_phenylephrine
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
    GROUP BY co.stay_id, co.hr
)

-- =========================================================================
-- Assemble all components into a single per-(stay_id, hr) row

, scorecomp AS (
    SELECT
        co.stay_id,
        co.hadm_id,
        co.hr,
        co.starttime,
        co.endtime,

        -- ECMO flags
        ecmo_hr.ecmo_any,
        ecmo_hr.ecmo_resp,
        ecmo_hr.ecmo_cv,

        -- Mechanical circulatory support (non-ECMO devices)
        mechanical_support_hour.mechanical_support,

        -- Respiratory (PaO2/FiO2 separated by vent status)
        pf.pao2fio2ratio_novent,
        pf.pao2fio2ratio_vent,

        -- Vasopressors/inotropes (7 agents)
        vaso.rate_epinephrine,
        vaso.rate_norepinephrine,
        vaso.rate_dopamine,
        vaso.rate_dobutamine,
        vaso.rate_milrinone,
        vaso.rate_vasopressin,
        vaso.rate_phenylephrine,

        -- Vitals
        vs.meanbp_min,

        -- Neurological
        gcs.gcs_min,

        -- Urine output windows
        uo.uomlkghr_24hr,
        uo.uomlkghr_12hr,
        uo.uomlkghr_6hr,

        -- Labs
        bili.bilirubin_max,
        cr.creatinine_max,
        plt.platelet_min,

        -- RRT
        rrt.dialysis_present,
        rrt.dialysis_active,

        -- Acid-base / electrolytes (renal footnote p)
        bg_ph.ph,
        blood_chem.bicarbonate,
        blood_chem.potassium,

        -- Delirium drugs (from separate derived table)
        dd.delirium_drug_rate

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
    LEFT JOIN vaso
        ON co.stay_id = vaso.stay_id AND co.hr = vaso.hr
    LEFT JOIN ecmo_hr
        ON co.stay_id = ecmo_hr.stay_id AND co.hr = ecmo_hr.hr
    LEFT JOIN mechanical_support_hour
        ON co.stay_id = mechanical_support_hour.stay_id AND co.hr = mechanical_support_hour.hr
    LEFT JOIN rrt
        ON co.stay_id = rrt.stay_id AND co.hr = rrt.hr
    -- [BUG-13] Deduplicate delirium_drug: the underlying table uses UNION ALL
    -- across IV (inputevents) and oral (emar) routes. A patient receiving both
    -- in the same hour gets duplicate (stay_id, hr) rows, which would inflate
    -- other component values through the join. Collapsing here with MAX.
    LEFT JOIN (
        SELECT stay_id, hr, MAX(delirium_drug_rate) AS delirium_drug_rate
        FROM `mimic-hr.derived.delirium_drug`
        GROUP BY stay_id, hr
    ) dd
        ON co.stay_id = dd.stay_id AND co.hr = dd.hr
    LEFT JOIN bg_ph
        ON co.hadm_id = bg_ph.hadm_id AND co.hr = bg_ph.hr
    LEFT JOIN blood_chem
        ON co.hadm_id = blood_chem.hadm_id AND co.hr = blood_chem.hr
)

-- =========================================================================
-- Score each organ system per hour

, scorecalc AS (
    SELECT
        scorecomp.*

        -- =================================================================
        -- RESPIRATORY SCORE
        -- Rules:
        --   - Any ECMO (VV or VA) -> respiratory score 4
        --   - Scores 3-4 depend on advanced vent support (handled by _vent field)
        --   - Boundary conditions use <= where table indicates cutoffs
        , CASE
            WHEN COALESCE(ecmo_resp, 0) = 1 OR COALESCE(ecmo_cv, 0) = 1 THEN 4
            WHEN pao2fio2ratio_vent   <=  75 THEN 4
            WHEN pao2fio2ratio_vent   <= 150 THEN 3
            WHEN pao2fio2ratio_novent <= 225 THEN 2
            WHEN pao2fio2ratio_vent   <= 225 THEN 2
            WHEN pao2fio2ratio_novent <= 300 THEN 1
            WHEN pao2fio2ratio_vent   <= 300 THEN 1
            WHEN COALESCE(pao2fio2ratio_vent, pao2fio2ratio_novent) IS NULL THEN NULL
            ELSE 0
        END AS respiration

        -- =================================================================
        -- COAGULATION (Hemostasis) SCORE
        -- Platelet thresholds use <= cutoffs.
        , CASE
            WHEN platelet_min <=  50 THEN 4
            WHEN platelet_min <=  80 THEN 3
            WHEN platelet_min <= 100 THEN 2
            WHEN platelet_min <= 150 THEN 1
            WHEN platelet_min IS NULL THEN NULL
            ELSE 0
        END AS coagulation

        -- =================================================================
        -- HEPATIC (Liver) SCORE

        , CASE
            WHEN bilirubin_max IS NULL THEN NULL
            WHEN bilirubin_max > 12.0 THEN 4
            WHEN bilirubin_max >  6.0 AND bilirubin_max <= 12.0 THEN 3
            WHEN bilirubin_max >  3.0 AND bilirubin_max <=  6.0 THEN 2
            WHEN bilirubin_max >  1.2 AND bilirubin_max <=  3.0 THEN 1
            ELSE 0
        END AS liver

        -- =================================================================
        -- CARDIOVASCULAR SCORE
        -- Primary SOFA-2 pathway:
        --   - mechanical support -> 4
        --   - NE+EPI sum thresholds -> 2/3/4
        --   - escalation if NE+EPI plus "other vasoactive" agents
        --   - dopamine sole-agent special thresholds
        --   - MAP <70 with no vasopressors -> 1

        , CASE
            -- ---- Score 4 (mechanical support) ----
            WHEN mechanical_support = 1 THEN 4
            WHEN COALESCE(ecmo_cv, 0) = 1 THEN 4

            -- ---- Score 4 (high-dose NE+EPI) ----
            WHEN (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) > 0.4 THEN 4

            -- ---- Score 4 (medium-dose NE+EPI + any other agent) ----
            WHEN (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) > 0.2
             AND (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) <= 0.4
             AND (
                    COALESCE(rate_dopamine, 0) > 0
                 OR COALESCE(rate_dobutamine, 0) > 0
                 OR COALESCE(rate_milrinone, 0) > 0
                 OR COALESCE(rate_vasopressin, 0) > 0
                 OR COALESCE(rate_phenylephrine, 0) > 0
                 )
            THEN 4

            -- ---- Score 4 (dopamine sole-agent > 40) ----
            WHEN COALESCE(rate_dopamine, 0) > 40
             AND (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) = 0
            THEN 4

            -- ---- Score 3 (medium-dose NE+EPI; no other agent) ----
            WHEN (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) > 0.2
             AND (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) <= 0.4
            THEN 3

            -- ---- Score 3 (low-dose NE+EPI + any other agent) ----
            WHEN (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) > 0
             AND (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) <= 0.2
             AND (
                    COALESCE(rate_dopamine, 0) > 0
                 OR COALESCE(rate_dobutamine, 0) > 0
                 OR COALESCE(rate_milrinone, 0) > 0
                 OR COALESCE(rate_vasopressin, 0) > 0
                 OR COALESCE(rate_phenylephrine, 0) > 0
                 )
            THEN 3

            -- ---- Score 3 (dopamine sole-agent >20 to <=40) ----
            WHEN COALESCE(rate_dopamine, 0) > 20
             AND COALESCE(rate_dopamine, 0) <= 40
             AND (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) = 0
            THEN 3

            -- ---- Score 2 (low-dose NE+EPI; no other agent) ----
            WHEN (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) > 0
             AND (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) <= 0.2
            THEN 2

            -- ---- Score 2 (any other vasoactive/inotrope alone; no NE/EPI) ----
            WHEN (
                    COALESCE(rate_dopamine, 0) > 0
                 OR COALESCE(rate_dobutamine, 0) > 0
                 OR COALESCE(rate_milrinone, 0) > 0
                 OR COALESCE(rate_vasopressin, 0) > 0
                 OR COALESCE(rate_phenylephrine, 0) > 0
                 )
            THEN 2

            -- ---- Score 1: MAP <70, no vasopressor/inotrope ----
            WHEN meanbp_min < 70 THEN 1

            -- ---- Missingness handling ----
            WHEN COALESCE(
                meanbp_min,
                rate_dopamine, rate_dobutamine,
                rate_epinephrine, rate_norepinephrine,
                rate_milrinone, rate_vasopressin, rate_phenylephrine,
                mechanical_support, ecmo_cv
            ) IS NULL THEN NULL

            ELSE 0
        END AS cardiovascular

        -- =================================================================
        -- NEUROLOGICAL (Brain) SCORE
        -- Delirium-drug treatment forces minimum score 1 when GCS=15

        , CASE
            WHEN gcs_min >= 3  AND gcs_min <= 5  THEN 4   -- GCS 3-5
            WHEN gcs_min >= 6  AND gcs_min <= 8  THEN 3   -- GCS 6-8
            WHEN gcs_min >= 9  AND gcs_min <= 12 THEN 2   -- GCS 9-12
            WHEN gcs_min >= 13 AND gcs_min <= 14 THEN 1   -- GCS 13-14
            WHEN gcs_min = 15 AND COALESCE(delirium_drug_rate, 0) > 0 THEN 1
            -- If GCS is missing but delirium drugs are active, score 1
            WHEN gcs_min IS NULL AND COALESCE(delirium_drug_rate, 0) > 0 THEN 1
            WHEN gcs_min IS NULL THEN NULL
            ELSE 0
        END AS cns

        -- =================================================================
        -- RENAL (Kidney) SCORE
        -- Hierarchy:
        --   4: receiving RRT (dialysis_present=1)
        --   3: creatinine >3.50 OR UO<0.3 over 24h OR anuria>=12h
        --   2: creatinine >2.0 and <=3.50 OR UO<0.5 over 12h
        --   1: creatinine >1.20 and <=2.0 OR UO<0.5 over 6h
        --
        -- Anuria>=12h: urine output rate over 12h window <= 0 mL/kg/h.

        , CASE
            -- ---- Score 4: receiving RRT (includes chronic dialysis) ----
            WHEN dialysis_present = 1 THEN 4

            -- ---- Score 3: creatinine >3.50 ----
            -- No upper bound: in SOFA-2, score 4 is RRT-based, not creatinine-based
            WHEN creatinine_max > 3.50 THEN 3

            -- ---- Score 3: urine output <0.3 mL/kg/h over 24h ----
            WHEN uomlkghr_24hr < 0.3 THEN 3

            -- ---- Score 3: anuria for >=12h (rate <= 0 over 12h window) ----
            WHEN uomlkghr_12hr <= 0 THEN 3

            -- ---- Score 2: creatinine >2.0 and <=3.50 ----
            WHEN (creatinine_max > 2.0 AND creatinine_max <= 3.50) THEN 2

            -- ---- Score 2: urine output <0.3 mL/kg/h over >=12h ----
            WHEN uomlkghr_12hr < 0.3 THEN 2

            -- ---- Score 1: creatinine >1.20 and <=2.0 ----
            WHEN (creatinine_max > 1.20 AND creatinine_max <= 2.0) THEN 1

            -- ---- Score 1: urine output <0.5 mL/kg/h over 6h ----
            WHEN uomlkghr_6hr < 0.5 THEN 1

            -- ---- Missingness handling ----
            WHEN COALESCE(
                uomlkghr_24hr, uomlkghr_12hr, uomlkghr_6hr,
                creatinine_max, dialysis_present
            ) IS NULL THEN NULL

            ELSE 0
        END AS renal

    FROM scorecomp
)

-- =========================================================================
-- 24-hour rolling maximum per organ system + total SOFA-2
-- SOFA-2 final score: sum of the maximum points of each organ system within
-- a rolling 24-hour window.

, score_final AS (
    SELECT
        s.*,

        COALESCE(MAX(respiration)     OVER w, 0) AS respiration_24hours,
        COALESCE(MAX(coagulation)     OVER w, 0) AS coagulation_24hours,
        COALESCE(MAX(liver)           OVER w, 0) AS liver_24hours,
        COALESCE(MAX(cardiovascular)  OVER w, 0) AS cardiovascular_24hours,
        COALESCE(MAX(cns)             OVER w, 0) AS cns_24hours,
        COALESCE(MAX(renal)           OVER w, 0) AS renal_24hours,

        -- Total SOFA-2: sum of 6 organ-system 24h maxima
        COALESCE(MAX(respiration)    OVER w, 0)
      + COALESCE(MAX(coagulation)    OVER w, 0)
      + COALESCE(MAX(liver)          OVER w, 0)
      + COALESCE(MAX(cardiovascular) OVER w, 0)
      + COALESCE(MAX(cns)            OVER w, 0)
      + COALESCE(MAX(renal)          OVER w, 0)
        AS sofa_24hours

    FROM scorecalc s
    WINDOW w AS (
        PARTITION BY stay_id
        ORDER BY hr
        ROWS BETWEEN 23 PRECEDING AND 0 FOLLOWING
    )
)

SELECT *
FROM score_final
WHERE hr >= 0;

-- =============================================================================
-- LIMITATION / NOTE: SpO2/FiO2 fallback NOT IMPLEMENTED
-- SOFA-2 specifies using SpO2/FiO2 when PaO2/FiO2 is unavailable AND SpO2 < 98%.
-- Need to:
--   1) require SpO2 < 98% for the substitution,
--   2) apply the footnote (f) thresholds for scores 0–4,
--   3) preserve the “advanced vent support required for scores 3–4” rule.
