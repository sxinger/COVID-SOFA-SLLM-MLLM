/* Table 2 - Demographic Table with Death Date integrated*/

/*connect to most up-to-date CDM instance containing COVID patients*/

-- master encounter table
select * from covid_eligible_enc;

whenever sqlerror continue;
drop table covid_demo;
whenever sqlerror exit;

/*collect the demographic table*/
create table covid_demo as
select enc.patid
      ,demo.birth_date
      ,demo.sex
      ,demo.race
      ,demo.hispanic
      ,death.death_date
from covid_eligible_enc enc
join "&&CDM_schema".DEMOGRAPHIC demo on demo.patid = enc.patid
join "&&CDM_schema".DEATH death on death.patid = enc.patid
;



