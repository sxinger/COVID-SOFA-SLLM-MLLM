/* Table 3 - Diagnosis Table
   
   Note that: there is no time restrictions for diagnosis, as we will want to identify all the patients historical commorbidity
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
      ,dx.encounterid
      ,dx.diagnosisid
      ,dx.dx
      ,dx.dx_date
      ,dx.pdx
      ,dx.dx_type
      ,dx.dx_source
      ,dx.dx_origin
from covid_eligible_enc enc
join "&&CDM_schema".DIAGNOSIS dx on dx.patid = enc.patid
;