/* Table 6 - Vital Table
   
   Note that: observation time window is within [admit_date - 365d, discharge_date + 30d]
*/

/*connect to most up-to-date CDM instance containing COVID patients*/

-- make sure that the eligible encounter table is accessible
select * from covid_eligible_enc;

whenever sqlerror continue;
drop table covid_vital;
whenever sqlerror exit;

/*collect the demographic table*/
create table covid_vital as
select enc.patid
      ,vital.vitalid
      ,vital.measure_date
      ,vital.measure_time
      ,vital.ht
      ,vital.wt
      ,vital.original_bmi
      ,vital.systolic
      ,vital.diastolic
      ,vital.smoking
      ,vital.tabacco
from covid_eligible_enc enc
join "&&CDM_schema".VITAL vital on vital.patid = enc.patid and
      vital.px_date between enc.admit_date - 365 and enc.discharge_date + 30     
;