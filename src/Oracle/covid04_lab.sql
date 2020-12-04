/* Table 4 - Lab Table
   
   Note that: observation time window is within [admit_date - 365d, discharge_date + 30d], 
              to effectively reduce the data amount and still leave a grace period to allow 
              identification of baseline labs
*/

/*connect to most up-to-date CDM instance containing COVID patients*/

-- master encounter table
select * from covid_eligible_enc;

whenever sqlerror continue;
drop table covid_dx;
whenever sqlerror exit;

/*collect the demographic table*/
create table covid_dx as
select enc.patid
      ,labs.encounterid
      ,labs.lab_result_cm_id
      ,labs.lab_order_date
      ,labs.specimen_date
      ,labs.specimen_time
      ,labs.result_date
      ,labs.result_time
      ,labs.specimen_source
      ,labs.result_num
      ,labs.result_unit
      ,labs.lab_px
      ,labs.lab_px_type
      ,labs.result_modifier
      ,labs.norm_range_low
      ,labs.norm_modifier_low
      ,labs.norm_range_high
      ,labs.norm_modifier_high
      ,labs.lab_loinc
      ,labs.result_loc
      ,labs.raw_lab_name -- espically, when lab values were not originally recorded as numerical values or mapped to LOINC
      ,labs.raw_lab_code -- espically, when lab values were not originally recorded as numerical values or mapped to LOINC
      ,labs.raw_result -- espically, when lab values were not originally recorded as numerical values or mapped to LOINC
      ,labs.raw_unit -- espically, when lab values were not originally recorded as numerical values or mapped to LOINC
from covid_eligible_enc enc
join "&&CDM_schema".LAB_RESULT_CM labs on labs.patid = enc.patid and
     labs.result_date between enc.admit_date - 365 and enc.discharge_date + 30
;