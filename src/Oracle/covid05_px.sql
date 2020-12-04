/* Table 5 - Procedure Table
   
   Note that: observation time window is within [admit_date - 365d, discharge_date + 30d]
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
      ,px.encounterid
      ,px.procedureid
      ,px.enc_type
      ,px.px_date
      ,px.px
      ,px.px_type
      ,px.px_source
      ,px.ppx
from covid_eligible_enc enc
join "&&CDM_schema".PROCEDURES px on px.patid = enc.patid and
      px.px_date between enc.admit_date - 365 and enc.discharge_date + 30     
;