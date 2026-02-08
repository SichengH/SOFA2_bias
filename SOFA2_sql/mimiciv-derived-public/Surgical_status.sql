DROP TABLE IF EXISTS `mimic-hr.derived.surgical_status`;
CREATE TABLE `mimic-hr.derived.surgical_status` AS (
  SELECT hadm_id,
  CASE WHEN hadm_id IN (
    SELECT hadm_id 
    FROM `physionet-data.mimiciv_3_1_hosp.transfers` 
    WHERE careunit in ('Surgery','Surgery/Trauma','Cardiac Surgery','Thoracic Surgery')
    and eventtype = 'admit'
  ) THEN 'Surgical admission' 
  ELSE 'Non Surgical' END AS Surgical_status,
  CASE WHEN edregtime is not null THEN 'ED'
  ELSE 'Elective' END AS ED_admission
  FROM `physionet-data.mimiciv_3_1_hosp.admissions`

)

--First, Surgical vs non-surgical

--Second for Surgical, ED first vs Elective. 