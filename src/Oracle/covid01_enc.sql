/* Table 1 - Eligible Encounters
   - Include encoutners that are
     1.	ED, IP and EI type; AND
     2. after 01/01/2020; AND
     3. Admitted with 
     - a. any lab tested for COVID; OR
     - b. any of the listed respiratory conditions or infection symptoms
       - b.1 viral infection (VIRAL)
       - b.2 pneumonia (PNEUM)
       - b.3 influenza (FLU)
       - b.4 respiratory failure; insufficiency; arrest (RESP)
       - b.5 fever (FEVER)

   - No additional exclusion criteria is applied, as we want to minimize the selection bias and build more generalizable models. 
    
   - Output table layout will very much resemble the PCORnet CDM Encounter Table, with the following additional columns
     1. COVID_STATUS
     
refences: 
    - https://www.hcup-us.ahrq.gov/toolssoftware/ccs/AppendixASingleDX.txt
    - https://www.hcup-us.ahrq.gov/toolssoftware/ccsr/DXCCSR_v2021-1.zip 
*/


/*connect to most up-to-date CDM instance containing COVID patients*/

whenever sqlerror continue;
drop table icd10_list;
drop table covid_eligible_enc;
whenever sqlerror exit;


--make sure the resulting table from covid00_prep.sql script has been generated and accessible
select * from covid_enc;


-- To increase the diversity of control cohort, we also want to include inpatients who was not tested for COVID19 but 
-- but may present similar symptoms as COVID19
create table icd10_list as
select 'A60' icd_key, 'VIRAL' dx_ccs_grp1, null dx_ccs_grp2 from dual
union 
select 'A8' icd_key, 'VIRAL', null  from dual
union 
select 'A9' icd_key, 'VIRAL', null from dual
union 
select 'B0' icd_key, 'VIRAL', null from dual
union 
select 'B10' icd_key, 'VIRAL', null from dual
union 
select 'B25' icd_key, 'VIRAL', null from dual
union 
select 'B26' icd_key, 'VIRAL', null from dual
union 
select 'B27' icd_key, 'VIRAL', null from dual
union 
select 'B33' icd_key, 'VIRAL', null from dual
union 
select 'B34' icd_key, 'VIRAL', null from dual
union 
select 'B37' icd_key, 'VIRAL', null from dual
union 
select 'J12' icd_key, 'VIRAL', null from dual
union 
select 'J12' icd_key, 'VIRAL', 'PNEUM' from dual
union 
select 'J20.3' icd_key, 'VIRAL', 'BRONC' from dual
union 
select 'J20.4' icd_key, 'VIRAL', 'BRONC' from dual
union 
select 'J20.5' icd_key, 'VIRAL', 'BRONC' from dual
union 
select 'J20.6' icd_key, 'VIRAL', 'BRONC' from dual
union 
select 'J20.7' icd_key, 'VIRAL', 'BRONC' from dual
union 
select 'J21.0' icd_key, 'VIRAL', 'BRONC'  from dual
union 
select 'J21.1' icd_key, 'VIRAL', 'BRONC' from dual
union 
select 'J16.8' icd_key, 'PNEUM', null from dual
union 
select 'J17' icd_key, 'PNEUM', null from dual
union 
select 'J18' icd_key, 'PNEUM', null from dual
union 
select 'J85.1' icd_key, 'PNEUM', null from dual
union 
select 'J95.85' icd_key, 'PNEUM', null from dual
union 
select 'J09.X1' icd_key, 'PNEUM', 'FLU' from dual
union 
select 'J10.0' icd_key, 'PNEUM', 'FLU' from dual
union 
select 'J11.1' icd_key, 'PNEUM', 'FLU' from dual
union 
select 'J09.X2' icd_key, 'FLU', null from dual
union 
select 'J09.X3' icd_key, 'FLU', null from dual
union 
select 'J09.X9' icd_key, 'FLU', null from dual
union 
select 'J10.1' icd_key, 'FLU', null from dual
union 
select 'J11.1' icd_key, 'FLU', null from dual
union 
select 'J20.8' icd_key, 'BRONC', null from dual
union 
select 'J20.9' icd_key, 'BRONC', null from dual
union 
select 'J21.8' icd_key, 'BRONC', null from dual
union 
select 'J21.9' icd_key, 'BRONC', null from dual
union 
select 'J40' icd_key, 'BRONC', null from dual
union 
select 'J80' icd_key, 'RESP', null from dual
union 
select 'J95.82' icd_key, 'RESP', null from dual
union 
select 'J96' icd_key, 'RESP', null from dual
union 
select 'R09.2' icd_key, 'RESP', null from dual
union 
select 'R50.8' icd_key, 'FEVER', null from dual
;

/*collect the eligible encounter table*/
undefine CDM_schema;
create table covid_eligible_enc as
with eligb_icd as (
select distinct patid, encounterid
from "&&CDM_schema".DIAGNOSIS dx
join icd10_list icd
on dx.DX_TYPE = '10' and
   dx.DX like (icd.icd_key || '%') and
   dx.admit_date >= Date '2020-01-01'
)
select cdm.patid
      ,cdm.encounterid
      ,cdm.admit_date
      ,cdm.admit_time
      ,cdm.discharge_date
      ,cdm.discharge_time
      ,cdm.enc_type
      ,cdm.discharge_disposition
      ,cdm.discharge_status
      ,cdm.admitting_source
      ,coalesce(covid.pos_confirmed, -1) COVID_STATUS
from "&&CDM_schema".ENCOUNTER cdm
left join covid_enc covid
on cdm.patid = covid.patient_num and cdm.encounterid = covid.encounter_num  -- assume i2b2.patient_num matches with cdm.patid and i2b2.encounter_num matches with cdm.encounterid
where (covid.pos_confirmed is not null or 
       exists (select 1 from eligb_icd
               where eligb_icd.patid = cdm.patid and eligb_icd.encounterid = cdm.encounterid)) and
      cdm.admit_date >= Date '2020-01-01' and
      cdm.enc_type in ('ED','EI','IP')
;
