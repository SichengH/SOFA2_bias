-- Blood gas pivot table with paired SpO2 and FiO2 from chartevents
-- Computes PaO2/FiO2 and SpO2/FiO2 ratios for respiratory scoring
-- SpO2/FiO2 only computed when SpO2 < 98% (footnote f)

DROP TABLE IF EXISTS `mimic-hr.derived.bg_spo2`;
CREATE TABLE `mimic-hr.derived.bg_spo2` AS 

WITH bg AS (
    -- Pivot blood gas labs from labevents, one row per specimen_id
    SELECT
        MAX(subject_id) AS subject_id
        , MAX(hadm_id) AS hadm_id
        , MAX(charttime) AS charttime
        , MAX(storetime) AS storetime
        , le.specimen_id
        , MAX(CASE WHEN itemid = 52033 THEN value ELSE NULL END) AS specimen
        , MAX(CASE WHEN itemid = 50801 THEN valuenum ELSE NULL END) AS aado2
        , MAX(CASE WHEN itemid = 50802 THEN valuenum ELSE NULL END) AS baseexcess
        , MAX(CASE WHEN itemid = 50803 THEN valuenum ELSE NULL END) AS bicarbonate
        , MAX(CASE WHEN itemid = 50804 THEN valuenum ELSE NULL END) AS totalco2
        , MAX(CASE WHEN itemid = 50805 THEN valuenum ELSE NULL END) AS carboxyhemoglobin
        , MAX(CASE WHEN itemid = 50806 THEN valuenum ELSE NULL END) AS chloride
        , MAX(CASE WHEN itemid = 50808 THEN valuenum ELSE NULL END) AS calcium
        , MAX(CASE WHEN itemid = 50809 AND valuenum <= 10000 THEN valuenum ELSE NULL END) AS glucose
        , MAX(CASE WHEN itemid = 50810 AND valuenum <= 100 THEN valuenum ELSE NULL END) AS hematocrit
        , MAX(CASE WHEN itemid = 50811 THEN valuenum ELSE NULL END) AS hemoglobin
        , MAX(CASE WHEN itemid = 50813 AND valuenum <= 10000 THEN valuenum ELSE NULL END) AS lactate
        , MAX(CASE WHEN itemid = 50814 THEN valuenum ELSE NULL END) AS methemoglobin
        , MAX(CASE WHEN itemid = 50815 THEN valuenum ELSE NULL END) AS o2flow
        -- FiO2 from lab: correct common unit errors (values <= 20% are unphysiologic)
        , MAX(CASE WHEN itemid = 50816 THEN
                CASE
                    WHEN valuenum > 20 AND valuenum <= 100 THEN valuenum
                    WHEN valuenum > 0.2 AND valuenum <= 1.0 THEN valuenum * 100.0
                    ELSE NULL END
            ELSE NULL END) AS fio2
        , MAX(CASE WHEN itemid = 50817 AND valuenum <= 100 THEN valuenum ELSE NULL END) AS so2
        , MAX(CASE WHEN itemid = 50818 THEN valuenum ELSE NULL END) AS pco2
        , MAX(CASE WHEN itemid = 50819 THEN valuenum ELSE NULL END) AS peep
        , MAX(CASE WHEN itemid = 50820 THEN valuenum ELSE NULL END) AS ph
        , MAX(CASE WHEN itemid = 50821 THEN valuenum ELSE NULL END) AS po2
        , MAX(CASE WHEN itemid = 50822 THEN valuenum ELSE NULL END) AS potassium
        , MAX(CASE WHEN itemid = 50823 THEN valuenum ELSE NULL END) AS requiredo2
        , MAX(CASE WHEN itemid = 50824 THEN valuenum ELSE NULL END) AS sodium
        , MAX(CASE WHEN itemid = 50825 THEN valuenum ELSE NULL END) AS temperature
        , MAX(CASE WHEN itemid = 50807 THEN value ELSE NULL END) AS comments
    FROM `physionet-data.mimiciv_3_1_hosp.labevents` le
    WHERE le.itemid IN (
        52033, 50801, 50802, 50803, 50804, 50805, 50806, 50807,
        50808, 50809, 50810, 50811, 50813, 50814, 50815, 50816,
        50817, 50818, 50819, 50820, 50821, 50822, 50823, 50824, 50825
    )
    GROUP BY le.specimen_id
)

-- Nearest SpO2 from pulse oximetry (within 2h before ABG)
, stg_spo2 AS (
    SELECT subject_id, charttime
        , AVG(valuenum) AS spo2
    FROM `physionet-data.mimiciv_3_1_icu.chartevents`
    WHERE itemid = 220277
        AND valuenum > 0 AND valuenum <= 100
    GROUP BY subject_id, charttime
)

-- Nearest FiO2 from chartevents (within 4h before ABG)
, stg_fio2 AS (
    SELECT subject_id, charttime
        , MAX(
            CASE
                WHEN valuenum > 0.2 AND valuenum <= 1 THEN valuenum * 100
                WHEN valuenum > 1 AND valuenum < 20 THEN NULL
                WHEN valuenum >= 20 AND valuenum <= 100 THEN valuenum
                ELSE NULL END
        ) AS fio2_chartevents
    FROM `physionet-data.mimiciv_3_1_icu.chartevents`
    WHERE itemid = 223835
        AND valuenum > 0 AND valuenum <= 100
    GROUP BY subject_id, charttime
)

-- Pair ABG with nearest SpO2 (most recent within 2h)
, stg2 AS (
    SELECT bg.*
        , ROW_NUMBER() OVER (
            PARTITION BY bg.subject_id, bg.charttime ORDER BY s1.charttime DESC
        ) AS lastrowspo2
        , s1.spo2
    FROM bg
    LEFT JOIN stg_spo2 s1
        ON bg.subject_id = s1.subject_id
            AND s1.charttime BETWEEN DATETIME_SUB(bg.charttime, INTERVAL '2' HOUR)
            AND bg.charttime
    WHERE bg.po2 IS NOT NULL
)

-- Pair with nearest FiO2 from chartevents (most recent within 4h)
, stg3 AS (
    SELECT bg.*
        , ROW_NUMBER() OVER (
            PARTITION BY bg.subject_id, bg.charttime ORDER BY s2.charttime DESC
        ) AS lastrowfio2
        , s2.fio2_chartevents
    FROM stg2 bg
    LEFT JOIN stg_fio2 s2
        ON bg.subject_id = s2.subject_id
            AND s2.charttime >= DATETIME_SUB(bg.charttime, INTERVAL '4' HOUR)
            AND s2.charttime <= bg.charttime
            AND s2.fio2_chartevents > 0
    WHERE bg.lastrowspo2 = 1
)

SELECT
    stg3.subject_id
    , stg3.hadm_id
    , stg3.charttime
    , specimen
    , spo2
    , so2, po2, pco2
    , fio2_chartevents, fio2
    , aado2
    -- Calculated A-a gradient
    , CASE
        WHEN po2 IS NULL OR pco2 IS NULL THEN NULL
        WHEN fio2 IS NOT NULL
            THEN (fio2 / 100) * (760 - 47) - (pco2 / 0.8) - po2
        WHEN fio2_chartevents IS NOT NULL
            THEN (fio2_chartevents / 100) * (760 - 47) - (pco2 / 0.8) - po2
        ELSE NULL
    END AS aado2_calc
    -- PaO2/FiO2 ratio (primary respiratory metric)
    , CASE
        WHEN po2 IS NULL THEN NULL
        WHEN fio2 IS NOT NULL THEN 100 * po2 / fio2
        WHEN fio2_chartevents IS NOT NULL THEN 100 * po2 / fio2_chartevents
        ELSE NULL
    END AS pao2fio2ratio
    -- SpO2/FiO2 ratio (fallback; footnote f: only when SpO2 < 98%)
    , CASE
        WHEN spo2 IS NULL THEN NULL
        WHEN spo2 >= 98 THEN NULL
        WHEN fio2 IS NOT NULL THEN 100 * spo2 / fio2
        WHEN fio2_chartevents IS NOT NULL THEN 100 * spo2 / fio2_chartevents
        ELSE NULL
    END AS spo2fio2ratio
    -- Acid-base
    , ph, baseexcess, bicarbonate, totalco2
    -- Blood count
    , hematocrit, hemoglobin, carboxyhemoglobin, methemoglobin
    -- Chemistry
    , chloride, calcium, temperature, potassium, sodium, lactate, glucose
FROM stg3
WHERE lastrowfio2 = 1
;
