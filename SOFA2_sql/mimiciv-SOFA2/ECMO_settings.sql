

DROP TABLE IF EXISTS `mimic-hr.derived.ECMO_settings`;
CREATE TABLE `mimic-hr.derived.ECMO_settings` AS 

with ecmo as (
  select *
  from `physionet-data.mimiciv_3_1_icu.chartevents` chart 
  left join `physionet-data.mimiciv_3_1_icu.d_items` d
  on chart.itemid = d.itemid
  where d.label like '%ECMO%'
)

SELECT stay_id,charttime
  , MAX(CASE WHEN label = 'Cannula sites visually inspected (ECMO)' THEN value ELSE NULL END) AS cannula_sites_visually_inspected
  , MAX(CASE WHEN label = 'Circuit Configuration (ECMO)' THEN value ELSE NULL END) AS circuit_configutation
  , MAX(CASE WHEN label = 'Circuit inspected for clot (ECMO)' THEN value ELSE NULL END) AS circuit_inspected_for_clot
  , MAX(CASE WHEN label = 'Emergency Equipment at bedside (ECMO)' THEN value ELSE NULL END) AS emergency_equipment_at_bedside
  , MAX(CASE WHEN label = 'FiO2 (ECMO)' THEN value ELSE NULL END) AS Fio2
  , MAX(CASE WHEN label = 'Flow (ECMO) ' THEN value ELSE NULL END) AS Flow
  , MAX(CASE WHEN label = 'Flow Alarm (Hi) (ECMO)' THEN value ELSE NULL END) AS Flow_alarm_hi
  , MAX(CASE WHEN label = 'Flow Alarm (Lo) (ECMO)' THEN value ELSE NULL END) AS Flow_alarm_low
  , MAX(CASE WHEN label = 'Flow Sensor repositioned (ECMO)' THEN value ELSE NULL END) AS Flow_sensor_repositioned
  , MAX(CASE WHEN label = 'Oxygenator visible (ECMO)' THEN value ELSE NULL END) AS oxygenator_visible
  , MAX(CASE WHEN label = 'Pump plugged into RED outlet (ECMO) ' THEN value ELSE NULL END) AS pump_plugged_into_red_outlet
  , MAX(CASE WHEN label = 'Speed (ECMO) ' THEN value ELSE NULL END) AS speed
  , MAX(CASE WHEN label = 'Suction events (ECMO) ' THEN value ELSE NULL END) AS suction_events
  , MAX(CASE WHEN label = 'Sweep (ECMO) ' THEN value ELSE NULL END) AS sweep
FROM ecmo
GROUP BY stay_id,charttime

