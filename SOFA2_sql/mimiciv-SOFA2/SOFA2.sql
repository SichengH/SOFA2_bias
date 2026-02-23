-- Sequential Organ Failure Assessment 2 (SOFA-2)
-- Scores 6 organ systems per hour, then computes 24h rolling max
-- Reads from: mimic-hr.derived.sofa2_component
-- Reference: Ranzani et al. (2025) SOFA-2 specification

DROP TABLE IF EXISTS `mimic-hr.derived.sofa2`;
CREATE TABLE `mimic-hr.derived.sofa2` AS 

WITH scorecalc AS (
    -- Hourly organ scores. NULL = data missing (treated as 0 in 24h rollup).
    SELECT scorecomp.*

        -- ==========================================================
        -- RESPIRATORY
        -- PaO2/FiO2 is primary; SpO2/FiO2 is fallback (different thresholds)
        -- Scores 3-4 require ventilatory support (_vent columns)
        -- ECMO (any type) → automatic score 4
        , CASE
            -- ECMO → 4 (footnote g)
            WHEN COALESCE(ecmo_resp, 0) = 1 OR COALESCE(ecmo_cv, 0) = 1 THEN 4
            -- PaO2/FiO2 (thresholds: 75 / 150 / 225 / 300)
            WHEN pao2fio2ratio_vent   <=  75 THEN 4
            WHEN pao2fio2ratio_vent   <= 150 THEN 3
            WHEN pao2fio2ratio_novent <= 225 THEN 2
            WHEN pao2fio2ratio_vent   <= 225 THEN 2
            WHEN pao2fio2ratio_novent <= 300 THEN 1
            WHEN pao2fio2ratio_vent   <= 300 THEN 1
            -- P/F > 300 → 0; guard prevents fallthrough to S/F
            WHEN COALESCE(pao2fio2ratio_vent, pao2fio2ratio_novent) IS NOT NULL THEN 0
            -- SpO2/FiO2 fallback (thresholds: 120 / 200 / 250 / 300, footnote d)
            WHEN spo2fio2ratio_vent   <= 120 THEN 4
            WHEN spo2fio2ratio_vent   <= 200 THEN 3
            WHEN spo2fio2ratio_novent <= 250 THEN 2
            WHEN spo2fio2ratio_vent   <= 250 THEN 2
            WHEN spo2fio2ratio_novent <= 300 THEN 1
            WHEN spo2fio2ratio_vent   <= 300 THEN 1
            WHEN COALESCE(spo2fio2ratio_vent, spo2fio2ratio_novent) IS NOT NULL THEN 0
            ELSE NULL
        END AS respiration

        -- ==========================================================
        -- COAGULATION (hemostasis)
        , CASE
            WHEN platelet_min <=  50 THEN 4
            WHEN platelet_min <=  80 THEN 3
            WHEN platelet_min <= 100 THEN 2
            WHEN platelet_min <= 150 THEN 1
            WHEN platelet_min IS NULL THEN NULL
            ELSE 0
        END AS coagulation

        -- ==========================================================
        -- LIVER (bilirubin in mg/dL)
        , CASE
            WHEN bilirubin_max > 12.0 THEN 4
            WHEN bilirubin_max >  6.0 THEN 3
            WHEN bilirubin_max >  3.0 THEN 2
            WHEN bilirubin_max >  1.2 THEN 1
            WHEN bilirubin_max IS NULL THEN NULL
            ELSE 0
        END AS liver

        -- ==========================================================
        -- CARDIOVASCULAR (footnotes i-n)
        -- Hierarchy: mechanical support → NE+EPI dose tiers →
        --   dopamine sole-agent tiers → other agents → MAP-only tiers
        -- NE+EPI sum (mcg/kg/min): >0.4 = high, >0.2 = medium, >0 = low
        -- "Other agents": dopamine, dobutamine, milrinone, vasopressin, phenylephrine
        , CASE
            -- Score 4: mechanical support (footnote n)
            WHEN mechanical_support = 1 THEN 4
            WHEN COALESCE(ecmo_cv, 0) = 1 THEN 4
            -- Score 4: high-dose NE+EPI
            WHEN (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) > 0.4 THEN 4
            -- Score 4: medium-dose NE+EPI + any other agent
            WHEN (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) > 0.2
             AND (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) <= 0.4
             AND (   COALESCE(rate_dopamine, 0) > 0
                  OR COALESCE(rate_dobutamine, 0) > 0
                  OR COALESCE(rate_milrinone, 0) > 0
                  OR COALESCE(rate_vasopressin, 0) > 0
                  OR COALESCE(rate_phenylephrine, 0) > 0)
            THEN 4
            -- Score 4: dopamine sole-agent > 40 (footnote l)
            WHEN COALESCE(rate_dopamine, 0) > 40
             AND (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) = 0
            THEN 4
            -- Score 3: medium-dose NE+EPI alone
            WHEN (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) > 0.2
             AND (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) <= 0.4
            THEN 3
            -- Score 3: low-dose NE+EPI + any other agent
            WHEN (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) > 0
             AND (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) <= 0.2
             AND (   COALESCE(rate_dopamine, 0) > 0
                  OR COALESCE(rate_dobutamine, 0) > 0
                  OR COALESCE(rate_milrinone, 0) > 0
                  OR COALESCE(rate_vasopressin, 0) > 0
                  OR COALESCE(rate_phenylephrine, 0) > 0)
            THEN 3
            -- Score 3: dopamine sole-agent > 20 to <= 40 (footnote l)
            WHEN COALESCE(rate_dopamine, 0) > 20
             AND COALESCE(rate_dopamine, 0) <= 40
             AND (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) = 0
            THEN 3
            -- Score 2: low-dose NE+EPI alone
            WHEN (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) > 0
             AND (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) <= 0.2
            THEN 2
            -- Score 2: any other agent alone (includes dopamine <= 20)
            WHEN (   COALESCE(rate_dopamine, 0) > 0
                  OR COALESCE(rate_dobutamine, 0) > 0
                  OR COALESCE(rate_milrinone, 0) > 0
                  OR COALESCE(rate_vasopressin, 0) > 0
                  OR COALESCE(rate_phenylephrine, 0) > 0)
            THEN 2
            -- MAP-only tiers when no vasopressors (footnote m)
            WHEN meanbp_min < 40 THEN 4
            WHEN meanbp_min < 50 THEN 3
            WHEN meanbp_min < 60 THEN 2
            WHEN meanbp_min < 70 THEN 1
            -- All inputs missing
            WHEN COALESCE(
                meanbp_min, rate_dopamine, rate_dobutamine,
                rate_epinephrine, rate_norepinephrine,
                rate_milrinone, rate_vasopressin, rate_phenylephrine,
                mechanical_support, ecmo_cv
            ) IS NULL THEN NULL
            ELSE 0
        END AS cardiovascular

        -- ==========================================================
        -- NEUROLOGICAL / BRAIN (footnotes a-c)
        -- GCS total → standard scoring
        -- GCS unavailable → motor fallback (footnote b): 6→0, 5→1, 4→2, 3→3, 1-2→4
        -- Delirium drug → minimum floor of 1 (footnote c)
        , CASE
            -- Total GCS available
            WHEN gcs_min >= 3  AND gcs_min <= 5  THEN 4
            WHEN gcs_min >= 6  AND gcs_min <= 8  THEN 3
            WHEN gcs_min >= 9  AND gcs_min <= 12 THEN 2
            WHEN gcs_min >= 13 AND gcs_min <= 14 THEN 1
            WHEN gcs_min = 15 AND COALESCE(delirium_drug_rate, 0) > 0 THEN 1
            WHEN gcs_min = 15 THEN 0
            -- Motor fallback when total GCS unavailable (footnote b)
            WHEN gcs_min IS NULL AND gcs_motor_min IS NOT NULL THEN
                CASE
                    WHEN gcs_motor_min <= 2 THEN 4
                    WHEN gcs_motor_min = 3  THEN 3
                    WHEN gcs_motor_min = 4  THEN 2
                    WHEN gcs_motor_min = 5  THEN 1
                    WHEN gcs_motor_min = 6 AND COALESCE(delirium_drug_rate, 0) > 0 THEN 1
                    WHEN gcs_motor_min = 6  THEN 0
                    ELSE NULL
                END
            -- No GCS at all but delirium drug present → minimum 1
            WHEN gcs_min IS NULL AND COALESCE(delirium_drug_rate, 0) > 0 THEN 1
            WHEN gcs_min IS NULL THEN NULL
            ELSE 0
        END AS cns

        -- ==========================================================
        -- RENAL (footnotes h-i, p)
        -- Score 4: on RRT, or meets RRT criteria (footnote p, version B: Cr > 3.50)
        -- RRT criteria: (Cr > 3.50 OR oliguria) AND (K+ >= 6.0 OR acidosis)
        , CASE
            -- On RRT
            WHEN dialysis_present = 1 OR dialysis_active = 1 THEN 4
            -- Meets RRT criteria but not on RRT (footnote p)
            WHEN (creatinine_max > 3.50 OR uomlkghr_6hr < 0.3)
             AND (potassium >= 6.0 OR (ph <= 7.20 AND bicarbonate <= 12))
            THEN 4
            -- Score 3
            WHEN creatinine_max > 3.5 THEN 3
            WHEN uomlkghr_24hr < 0.3 THEN 3
            WHEN uomlkghr_12hr <= 0 THEN 3   -- anuria >= 12h
            -- Score 2
            WHEN creatinine_max > 2.0 THEN 2
            WHEN uomlkghr_12hr < 0.3 THEN 2
            -- Score 1
            WHEN creatinine_max > 1.2 THEN 1
            WHEN uomlkghr_6hr < 0.5 THEN 1
            -- Score 0
            WHEN creatinine_max <= 1.2 THEN 0
            WHEN COALESCE(uomlkghr_24hr, uomlkghr_12hr, uomlkghr_6hr,
                          creatinine_max, dialysis_present) IS NULL THEN NULL
            ELSE 0
        END AS renal
    FROM `mimic-hr.derived.sofa2_component` scorecomp
)

-- 24-hour rolling maximum per organ system
, score_final AS (
    SELECT s.*
        , COALESCE(MAX(respiration)    OVER w, 0) AS respiration_24hours
        , COALESCE(MAX(coagulation)    OVER w, 0) AS coagulation_24hours
        , COALESCE(MAX(liver)          OVER w, 0) AS liver_24hours
        , COALESCE(MAX(cardiovascular) OVER w, 0) AS cardiovascular_24hours
        , COALESCE(MAX(cns)            OVER w, 0) AS cns_24hours
        , COALESCE(MAX(renal)          OVER w, 0) AS renal_24hours
        -- Total SOFA-2
        , COALESCE(MAX(respiration)    OVER w, 0)
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

SELECT * FROM score_final
WHERE hr >= 0;
