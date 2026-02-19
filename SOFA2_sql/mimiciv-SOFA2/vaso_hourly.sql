
DROP TABLE IF EXISTS `mimic-hr.derived.vaso_hourly`;
CREATE TABLE `mimic-hr.derived.vaso_hourly` AS 

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
        OR vas.stay_id IS NOT NULL
        OR phe.stay_id IS NOT NULL
        OR mil.stay_id IS NOT NULL
    GROUP BY co.stay_id, co.hr
)

select * from vaso
