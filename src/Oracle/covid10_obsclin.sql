/* Table 10 - OBS_CLIN Table
   
   Note that: observation time window is within [admit_date - 365d, discharge_date + 30d]
*/

/*connect to most up-to-date i2b2 instance containing COVID patients*/

-- make sure that the eligible encounter table and code mapping table are both accessible
select * from covid_eligible_enc;
select * from code_map_obsclinc;

whenever sqlerror continue;
drop table covid_obsclin;
whenever sqlerror exit;

/*collect the clinical observations directly from local i2b2*/
create table covid_obsclin as
select enc.patid
      ,obs.encounter_num encounterid
      ,obs.start_date obsclin_date_time
--      ,obs.obsclin_time
      ,obs.nval_num obsclin_result_num
      ,obs.tval_char obsclin_result_text
      ,cd.obsclin_type
      ,cd.obsclin_code
      ,cd.raw_obsclin_name    
from covid_eligible_enc enc
join "&&i2b2_schema".observation_fact obs on obs.patient_num = enc.patid   
join code_map_obsclinc cd on cd.i2b2_code = obs.concept_cd
where obs.start_date between enc.admit_date - 30 and enc.discharge_date + 30  
;

