/* Table 9 - MED_ADMIN Table
   
   Note that: observation time window is within [admit_date - 365d, discharge_date + 30d]
*/

/*connect to most up-to-date CDM instance containing COVID patients*/

-- make sure that the eligible encounter table is accessible
select * from covid_eligible_enc;

whenever sqlerror continue;
drop table covid_amed;
whenever sqlerror exit;

/*collect the demographic table*/
create table covid_amed as
select enc.patid
      ,amed.encounterid
      ,amed.prescribingid
      ,amed.medadminid
      ,amed.medadmin_start_date
      ,amed.medadmin_start_time
      ,amed.medadmin_stop_date
      ,amed.medadmin_stop_time
      ,amed.medadmin_type
      ,amed.medadmin_code
      ,amed.medadmin_dose_admin
      ,amed.medadmin_dose_admin_unit
      ,amed.medadmin_route
      ,amed.medadmin_source
from covid_eligible_enc enc
join "&&CDM_schema".MED_ADMIN amed on amed.patid = enc.patid and
      amed.rx_start_date between enc.admit_date - 365 and enc.discharge_date + 30     
;