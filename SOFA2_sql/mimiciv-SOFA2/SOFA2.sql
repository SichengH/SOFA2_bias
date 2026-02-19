-- ------------------------------------------------------------------
-- Title: Sequential Organ Failure Assessment2(SOFA2)

-- ------------------------------------------------------------------
-- This script needs optimization, currently it taks 3.5h to run. 

DROP TABLE IF EXISTS `mimic-hr.derived.sofa2`;
CREATE TABLE `mimic-hr.derived.sofa2` AS 
WITH scorecalc AS (
    -- Calculate the final score
    -- note that if the underlying data is missing,
    -- the component is null
    -- eventually these are treated as 0 (normal),
    -- but knowing when data is missing is useful for debugging
    SELECT scorecomp.*
        -- Respiration
        , CASE
            WHEN ECMO = 1 
            OR coalesce(pao2fio2ratio_vent,spo2fio2ratio_vent) < 75 THEN 4
            WHEN coalesce(pao2fio2ratio_vent,spo2fio2ratio_vent) < 150 THEN 3
            WHEN coalesce(pao2fio2ratio_novent,spo2fio2ratio_novent) < 225 THEN 2
            WHEN coalesce(pao2fio2ratio_vent,spo2fio2ratio_vent) < 225 THEN 2
            WHEN coalesce(pao2fio2ratio_novent,spo2fio2ratio_novent) < 300 THEN 1
            WHEN coalesce(pao2fio2ratio_vent,spo2fio2ratio_vent) < 300 THEN 1
            WHEN
                COALESCE(
                    coalesce(pao2fio2ratio_vent,spo2fio2ratio_vent), 
                    coalesce(pao2fio2ratio_novent,spo2fio2ratio_novent)
                ) IS NULL THEN null
            ELSE 0
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

        -- Cardiovascular
        , case
      when (coalesce(rate_epinephrine,0) + coalesce(rate_norepinephrine,0)) > 0.4 then 4
      when (coalesce(rate_epinephrine,0) + coalesce(rate_norepinephrine,0)) > 0.2 and coalesce(rate_dobutamine,rate_dopamine,rate_milrinone,rate_phenylephrine) is not null then 4
      when (coalesce(rate_epinephrine,0) + coalesce(rate_norepinephrine,0)) > 0.2 then 3
      when (coalesce(rate_epinephrine,0) + coalesce(rate_norepinephrine,0)) > 0.0 and coalesce(rate_dobutamine,rate_dopamine,rate_milrinone,rate_phenylephrine) is not null then 3
      when (coalesce(rate_epinephrine,0) + coalesce(rate_norepinephrine,0)) > 0.0 then 2
      when coalesce(rate_dobutamine,rate_dopamine,rate_milrinone,rate_phenylephrine) is not null then 2
      when MeanBP_Min < 70 and coalesce(rate_dobutamine,rate_dopamine,rate_milrinone,rate_phenylephrine) is null then 1
      else 0
    end as cardiovascular

        -- Neurological failure (GCS)
        , CASE
            WHEN (gcs_min >= 13 AND gcs_min <= 14) OR delirium_drug_rate > 0 THEN 1
            WHEN (gcs_min >= 9 AND gcs_min <= 12) THEN 2
            WHEN (gcs_min >= 6 AND gcs_min <= 8) THEN 3
            WHEN gcs_min <= 5 THEN 4
            WHEN gcs_min IS NULL THEN null
            ELSE 0
        END AS cns

        -- Renal failure - high creatinine or low urine output
        , CASE
            WHEN dialysis_present = 1 or dialysis_active = 1 THEN 4
            WHEN creatinine_max > 3.5 OR uomlkghr_24hr < 0.3 THEN 3
            WHEN creatinine_max > 2.0 OR uomlkghr_12hr < 0.3 THEN 2
            WHEN creatinine_max > 1.2 OR uomlkghr_6hr < 0.5 THEN 1 
            WHEN creatinine_max <= 1.2 THEN 0
            WHEN COALESCE(uomlkghr_24hr,uomlkghr_12hr,uomlkghr_6hr, creatinine_max) IS NULL THEN null
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
