-- Title: Sequential Organ Failure Assessment2 (SOFA2)

DROP TABLE IF EXISTS `mimic-hr.derived.sofa2`;
CREATE TABLE `mimic-hr.derived.sofa2` AS 
WITH scorecalc AS (
    -- Calculate the final score
    -- note that if the underlying data is missing,
    -- the component is null
    -- eventually these are treated as 0 (normal),
    -- but knowing when data is missing is useful for debugging
    SELECT scorecomp.*

        -- ==============================================================
        -- RESPIRATORY SCORE
        -- ==============================================================
        -- [CHANGE-11] Rewrite of respiratory scoring.
        -- THREE issues fixed:
        --   (a) Thresholds changed from < to <= (protocol specifies <=)
        --   (b) P/F and S/F now have SEPARATE cascades with DIFFERENT
        --       thresholds per footnote f:
        --         P/F: 300 / 225 / 150 / 75
        --         S/F: 300 / 250 / 200 / 120
        --   (c) Added guard clause to prevent P/F fallthrough to S/F
        --       when P/F data exists but is > 300 (score 0).
        --   (d) ECMO now uses ecmo_resp/ecmo_cv (CHANGE-01) so VV and
        --       VA both correctly trigger resp score 4.
        --
        -- OLD:
        --   , CASE
        --       WHEN ECMO = 1
        --       OR coalesce(pao2fio2ratio_vent,spo2fio2ratio_vent) < 75 THEN 4
        --       WHEN coalesce(pao2fio2ratio_vent,spo2fio2ratio_vent) < 150 THEN 3
        --       WHEN coalesce(pao2fio2ratio_novent,spo2fio2ratio_novent) < 225 THEN 2
        --       WHEN coalesce(pao2fio2ratio_vent,spo2fio2ratio_vent) < 225 THEN 2
        --       WHEN coalesce(pao2fio2ratio_novent,spo2fio2ratio_novent) < 300 THEN 1
        --       WHEN coalesce(pao2fio2ratio_vent,spo2fio2ratio_vent) < 300 THEN 1
        --       WHEN COALESCE(...) IS NULL THEN null
        --       ELSE 0
        --   END AS respiration
        --
        -- NEW: see below
        -- [END CHANGE-11]
        -- ==============================================================
        , CASE
            -- ECMO: any type → respiratory score 4 (footnote g/i)
            WHEN COALESCE(ecmo_resp, 0) = 1 OR COALESCE(ecmo_cv, 0) = 1 THEN 4

            -- ---- PaO2/FiO2 pathway (primary) ----
            WHEN pao2fio2ratio_vent   <=  75 THEN 4
            WHEN pao2fio2ratio_vent   <= 150 THEN 3
            WHEN pao2fio2ratio_novent <= 225 THEN 2
            WHEN pao2fio2ratio_vent   <= 225 THEN 2
            WHEN pao2fio2ratio_novent <= 300 THEN 1
            WHEN pao2fio2ratio_vent   <= 300 THEN 1

            -- P/F exists and is > 300 → score 0 (do NOT fall through to S/F)
            WHEN COALESCE(pao2fio2ratio_vent, pao2fio2ratio_novent) IS NOT NULL THEN 0

            -- ---- SpO2/FiO2 pathway (fallback, footnote f) ----
            -- Only reached when PaO2/FiO2 is entirely NULL for this hour.
            -- Note: S/F uses DIFFERENT thresholds than P/F.
            -- SpO2 < 98% filter is applied in bg_spo2.sql (CHANGE-03).
            WHEN spo2fio2ratio_vent   <= 120 THEN 4
            WHEN spo2fio2ratio_vent   <= 200 THEN 3
            WHEN spo2fio2ratio_novent <= 250 THEN 2
            WHEN spo2fio2ratio_vent   <= 250 THEN 2
            WHEN spo2fio2ratio_novent <= 300 THEN 1
            WHEN spo2fio2ratio_vent   <= 300 THEN 1

            -- S/F exists and is > 300 → score 0
            WHEN COALESCE(spo2fio2ratio_vent, spo2fio2ratio_novent) IS NOT NULL THEN 0

            -- Neither P/F nor S/F available → NULL
            ELSE NULL
        END AS respiration

        -- Coagulation
        , CASE
            WHEN platelet_min <= 50 THEN 4
            WHEN platelet_min <= 80 THEN 3
            WHEN platelet_min <= 100 THEN 2
            WHEN platelet_min <= 150 THEN 1
            WHEN platelet_min IS NULL THEN null
            ELSE 0
        END AS coagulation

        -- Liver
        , CASE
            -- Bilirubin checks in mg/dL
            WHEN bilirubin_max > 12.0 THEN 4
            WHEN bilirubin_max > 6.0 THEN 3
            WHEN bilirubin_max > 3.0 THEN 2
            WHEN bilirubin_max > 1.2 THEN 1
            WHEN bilirubin_max IS NULL THEN null
            ELSE 0
        END AS liver

        -- ==============================================================
        -- CARDIOVASCULAR SCORE
        -- [CHANGE-12] Adjustment of cardiovascular scoring.
        -- SIX issues fixed:
        --   (a) Added mechanical_support → score 4 (footnote n).
        --       The mechanical_support_hourly table was joined but
        --       never used in the scoring cascade.
        --   (b) Added VA-ECMO → CV score 4 using ecmo_cv (CHANGE-01).
        --   (c) Added dopamine sole-agent thresholds (footnote l):
        --       dopa ≤20 → 2, >20-40 → 3, >40 → 4 (when no NE/EPI).
        --   (d) Added vasopressin to "other agent" checks. Was missing.
        --   (e) Changed "other agent" detection from
        --       COALESCE(rate_X, rate_Y, ...) IS NOT NULL
        --       to explicit COALESCE(rate_X, 0) > 0 checks per agent.
        --       Old pattern could false-positive if any rate was
        --       explicitly 0 (not NULL).
        --   (f) Added explicit NULL handling when all CV inputs missing.
        --
        -- OLD:
        --   , case
        --     when (coalesce(rate_epinephrine,0) + coalesce(rate_norepinephrine,0)) > 0.4 then 4
        --     when (coalesce(rate_epinephrine,0) + coalesce(rate_norepinephrine,0)) > 0.2
        --       and coalesce(rate_dobutamine,rate_dopamine,rate_milrinone,rate_phenylephrine) is not null then 4
        --     when (coalesce(rate_epinephrine,0) + coalesce(rate_norepinephrine,0)) > 0.2 then 3
        --     when (coalesce(rate_epinephrine,0) + coalesce(rate_norepinephrine,0)) > 0.0
        --       and coalesce(rate_dobutamine,rate_dopamine,rate_milrinone,rate_phenylephrine) is not null then 3
        --     when (coalesce(rate_epinephrine,0) + coalesce(rate_norepinephrine,0)) > 0.0 then 2
        --     when coalesce(rate_dobutamine,rate_dopamine,rate_milrinone,rate_phenylephrine) is not null then 2
        --     when MeanBP_Min < 70 and coalesce(...) is null then 1
        --     else 0
        --   end as cardiovascular
        --
        -- NEW: see below
        -- [END CHANGE-12]
        -- ==============================================================
        , CASE
            -- Score 4: mechanical circulatory support (footnote n)
            WHEN mechanical_support = 1 THEN 4
            -- Score 4: VA-ECMO (footnote i — VV-ECMO is resp only)
            WHEN COALESCE(ecmo_cv, 0) = 1 THEN 4

            -- Score 4: high-dose NE+EPI (> 0.4 mcg/kg/min)
            WHEN (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) > 0.4 THEN 4

            -- Score 4: medium-dose NE+EPI (> 0.2) + any other agent
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

            -- Score 4: dopamine sole-agent > 40 mcg/kg/min (footnote l)
            WHEN COALESCE(rate_dopamine, 0) > 40
             AND (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) = 0
            THEN 4

            -- Score 3: medium-dose NE+EPI (> 0.2, no other agent)
            WHEN (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) > 0.2
             AND (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) <= 0.4
            THEN 3

            -- Score 3: low-dose NE+EPI + any other agent
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

            -- Score 3: dopamine sole-agent > 20 to <= 40 (footnote l)
            WHEN COALESCE(rate_dopamine, 0) > 20
             AND COALESCE(rate_dopamine, 0) <= 40
             AND (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) = 0
            THEN 3

            -- Score 2: low-dose NE+EPI (no other agent)
            WHEN (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) > 0
             AND (COALESCE(rate_norepinephrine, 0) + COALESCE(rate_epinephrine, 0)) <= 0.2
            THEN 2

            -- Score 2: any other vasoactive/inotrope alone (no NE/EPI)
            -- Includes dopamine <= 20 as sole agent (footnote l)
            WHEN (
                    COALESCE(rate_dopamine, 0) > 0
                 OR COALESCE(rate_dobutamine, 0) > 0
                 OR COALESCE(rate_milrinone, 0) > 0
                 OR COALESCE(rate_vasopressin, 0) > 0
                 OR COALESCE(rate_phenylephrine, 0) > 0
                 )
            THEN 2

            -- Score 1: MAP < 70, no vasopressor/inotrope
            WHEN meanbp_min < 70 THEN 1

            -- NULL handling: if ALL CV inputs are missing
            WHEN COALESCE(
                meanbp_min,
                rate_dopamine, rate_dobutamine,
                rate_epinephrine, rate_norepinephrine,
                rate_milrinone, rate_vasopressin, rate_phenylephrine,
                mechanical_support, ecmo_cv
            ) IS NULL THEN NULL

            ELSE 0
        END AS cardiovascular

        -- ==============================================================
        -- NEUROLOGICAL (Brain) SCORE
        -- ==============================================================
        -- [CHANGE-13] Rewrite of neurological scoring. THREE issues fixed:
        --   (a) Delirium drug moved from first-match to minimum-floor.
        --       OLD: WHEN (gcs_min >= 13 AND gcs_min <= 14) OR delirium_drug_rate > 0 THEN 1
        --       This meant GCS=8 + delirium drug scored 1 instead of 3.
        --       NEW: delirium drug only applies at GCS=15 (or motor=6)
        --       to bump from 0 to 1. All worse GCS values score normally.
        --   (b) Added GCS motor fallback (footnote d, CHANGE-08).
        --       When gcs_total is NULL, use gcs_motor with mapping:
        --         motor 6→0, 5→1, 4→2, 3→3, 1-2→4
        --   (c) Added delirium drug check when ALL GCS is NULL.
        --
        -- OLD:
        --   , CASE
        --       WHEN (gcs_min >= 13 AND gcs_min <= 14) OR delirium_drug_rate > 0 THEN 1
        --       WHEN (gcs_min >= 9 AND gcs_min <= 12) THEN 2
        --       WHEN (gcs_min >= 6 AND gcs_min <= 8) THEN 3
        --       WHEN gcs_min <= 5 THEN 4
        --       WHEN gcs_min IS NULL THEN null
        --       ELSE 0
        --   END AS cns
        --
        -- NEW: see below
        -- [END CHANGE-13]
        -- ==============================================================
        , CASE
            -- Total GCS available: standard scoring
            WHEN gcs_min >= 3  AND gcs_min <= 5  THEN 4
            WHEN gcs_min >= 6  AND gcs_min <= 8  THEN 3
            WHEN gcs_min >= 9  AND gcs_min <= 12 THEN 2
            WHEN gcs_min >= 13 AND gcs_min <= 14 THEN 1
            -- Footnote e: delirium drug with GCS=15 → minimum 1 point
            WHEN gcs_min = 15 AND COALESCE(delirium_drug_rate, 0) > 0 THEN 1
            WHEN gcs_min = 15 THEN 0

            -- Total GCS unavailable: motor fallback (footnote d)
            -- Motor mapping from protocol footnote b:
            --   Motor 6 (obeys commands)       → score 0
            --   Motor 5 (localizing to pain)   → score 1
            --   Motor 4 (withdrawal to pain)   → score 2
            --   Motor 3 (flexion to pain)      → score 3
            --   Motor 1-2 (extension/none)     → score 4
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

            -- No GCS at all: delirium drugs still force score 1
            WHEN gcs_min IS NULL AND COALESCE(delirium_drug_rate, 0) > 0 THEN 1

            -- No GCS data → NULL
            WHEN gcs_min IS NULL THEN NULL

            ELSE 0
        END AS cns

        -- ==============================================================
        -- RENAL (Kidney) SCORE
        -- ==============================================================
        -- [CHANGE-14] Two small additions to renal scoring:
        --   (a) Added footnote p: RRT criteria fallback. Patients who
        --       meet criteria for RRT (creatinine >3.50 AND
        --       potassium >= 6.0 OR metabolic acidosis) score renal 4
        --       even if not on dialysis. Requires updated bg_ph (MIN)
        --       and blood_chem (MIN bicarb) from CHANGES 06-07.
        --   (b) Added anuria >= 12h check → score 3
        --       (uomlkghr_12hr <= 0 means zero output over 12h window)
        --
        -- OLD:
        --   , CASE
        --       WHEN dialysis_present = 1 or dialysis_active = 1 THEN 4
        --       WHEN creatinine_max > 3.5 OR uomlkghr_24hr < 0.3 THEN 3
        --       WHEN creatinine_max > 2.0 OR uomlkghr_12hr < 0.3 THEN 2
        --       WHEN creatinine_max > 1.2 OR uomlkghr_6hr < 0.5 THEN 1
        --       WHEN creatinine_max <= 1.2 THEN 0
        --       WHEN COALESCE(...) IS NULL THEN null
        --       ELSE 0
        --   END AS renal
        --
        -- NEW: see below
        -- [END CHANGE-14]
        -- ==============================================================
        , CASE
            -- Score 4: receiving RRT
            WHEN dialysis_present = 1 or dialysis_active = 1 THEN 4

            -- Score 4: fulfills criteria for RRT (footnote p)
            -- For patients NOT on RRT but who meet indications.
            -- Requires: (Cr > 3.50 OR oliguria) AND (K+ >= 6.0 OR acidosis)
            WHEN (creatinine_max > 3.50 OR uomlkghr_6hr < 0.3)
             AND (
                    potassium >= 6.0
                 OR (ph <= 7.20 AND bicarbonate <= 12)
                 )
            THEN 4

            -- Score 3: creatinine > 3.50
            WHEN creatinine_max > 3.5 THEN 3
            -- Score 3: urine output < 0.3 mL/kg/h over 24h
            WHEN uomlkghr_24hr < 0.3 THEN 3
            -- Score 3: anuria for >= 12h (rate <= 0 over 12h window)
            WHEN uomlkghr_12hr <= 0 THEN 3

            -- Score 2: creatinine > 2.0
            WHEN creatinine_max > 2.0 THEN 2
            -- Score 2: urine output < 0.3 mL/kg/h over >= 12h
            WHEN uomlkghr_12hr < 0.3 THEN 2

            -- Score 1: creatinine > 1.2
            WHEN creatinine_max > 1.2 THEN 1
            -- Score 1: urine output < 0.5 mL/kg/h over 6h
            WHEN uomlkghr_6hr < 0.5 THEN 1

            -- Score 0: creatinine <= 1.2 and no UO criteria met
            WHEN creatinine_max <= 1.2 THEN 0

            -- NULL handling
            WHEN COALESCE(uomlkghr_24hr, uomlkghr_12hr, uomlkghr_6hr,
                          creatinine_max, dialysis_present) IS NULL THEN null

            ELSE 0
        END AS renal
    FROM `mimic-hr.derived.sofa2_component` scorecomp
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
