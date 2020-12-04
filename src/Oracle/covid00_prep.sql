/* This script is for collecting additional information from source i2b2 datawarehouse
  1. Identifying patients/encounters that:
  - a. has been tested for COVID19; OR
  - b. has a COVID19 diagnosis;
  
  2. Extract additional data elements that may not be currently available in CDM
   - Start Date&Time and End Date&Time of ICU stay --> Primary outcome: ICU LOS
   - Start Date&Time and End Date&Time of invasive mechanical ventilation --> Primary outcome: IMV Exposure
   - Respiratory rate (breaths/min)
   - Temperature (C)
   - Hear Rate (/min)
   - Glasgow Coma Scale
   - SpO2,Pulse oximetry,Oxygen Saturation (%)
   - Non-invasive mechanival ventilation initiation
   - Non-invasive mechanival ventilation completion
   - Invasive Mechanical ventilation initiation
   - Invasive Mechanical ventilation completion
   - Invasive Mechanical ventilation mode (spontaneous, assisted, controlled)
   - PaO2, partial pressure of oxygen (mmHG) [only available with MV]
   - FiO2, fraction of inspired oxygen (%) [only available with MV]
   - PaO2/FiO2 [only available with MV]
*/

/*connect to most up-to-date i2b2 instance*/

whenever sqlerror continue;
drop table covid_enc;
whenever sqlerror exit;

create table covid_enc (
 patient_num varchar2(80),
 encounter_num varchar2(80),
 pos_confirmed integer,  --2=confirmed positive; 1=pending; 0=confirmed negative
 lab_int integer        --1=with lab; 0=without lab
);

/************************************************************************/
/*1. identify patients/encounters has tested for or diagnosis of COVID19*/
/************************************************************************/


/*identify patients/encounters that has a lab test (PCR, antibody, saliva) for COVID19*/
insert into covid_enc
with pos_cd as (
select distinct concept_cd
from "&&i2b2schema".concept_dimension
where    concept_path like '%ACT\UMLS_C0031437\SNOMED_3947185011\UMLS_C0022885\UMLS_C1335447%'              -- positive PCR lab
      or concept_path like '%ACT\UMLS_C0031437\SNOMED_3947185011\UMLS_C0022885\ACT_LOCAL_LAB_ANY_POSITIVE%' -- positive antibody lab
--      or concept_path like '...' -- add 1st local concept_path and uncomment it
--      or concept_path like '...' -- add 2nd local concept_path and uncomment it
)
select obs.patient_num
      ,obs.encounter_num
      ,2 
      ,1 
from "&&i2b2schema".observation_fact obs
join pos_cd
on obs.concept_cd = pos_cd.concept_cd
where obs.start_date >= '01-JAN-2020'
;
commit;


insert into covid_enc
with pos_cd as (
select distinct concept_cd
from "&&i2b2schema".concept_dimension
where    concept_path like '%ACT\UMLS_C0031437\SNOMED_3947185011\UMLS_C0022885\UMLS_C1611271%'                 -- pending PCR lab
      or concept_path like '%ACT\UMLS_C0031437\SNOMED_3947185011\UMLS_C0022885\UMLS_C4303880%'                 -- equivocal PCR lab
      or concept_path like '%ACT\UMLS_C0031437\SNOMED_3947185011\UMLS_C0022885\ACT_LOCAL_LAB_ANY_PENDING%'     -- pending antibody lab
      or concept_path like '%ACT\UMLS_C0031437\SNOMED_3947185011\UMLS_C0022885\ACT_LOCAL_LAB_ANY_EQUIVOCAL%'   -- equivocal antibody lab
--      or concept_path like '...' -- add 1st local concept_path and uncomment it
--      or concept_path like '...' -- add 2nd local concept_path and uncomment it
)
select obs.patient_num
      ,obs.encounter_num
      ,1 
      ,1 
from "&&i2b2schema".observation_fact obs
join pos_cd
on obs.concept_cd = pos_cd.concept_cd
where obs.start_date >= '01-JAN-2020'
;
commit;


insert into covid_enc
with pos_cd as (
select distinct concept_cd
from "&&i2b2schema".concept_dimension
where    concept_path like '%ACT\UMLS_C0031437\SNOMED_3947185011\UMLS_C0022885\UMLS_C1334932%'              -- negative PCR lab
      or concept_path like '%ACT\UMLS_C0031437\SNOMED_3947185011\UMLS_C0022885\ACT_LOCAL_LAB_ANY_NEGATIVE%' -- negative antibody lab
--      or concept_path like '...' -- add 1st local concept_path and uncomment it
--      or concept_path like '...' -- add 2nd local concept_path and uncomment it
)
select obs.patient_num
      ,obs.encounter_num
      ,0 
      ,1 
from "&&i2b2schema".observation_fact obs
join pos_cd
on obs.concept_cd = pos_cd.concept_cd
where obs.start_date >= '01-JAN-2020'
;
commit;



/*identify patients/encounters with ICD10 codes suggesting COVID19 (U07.1)*/
insert into covid_enc
with pos_cd as (
select distinct concept_cd
from "&&i2b2schema".concept_dimension
where    concept_path like '%ACT\UMLS_C0031437\SNOMED_3947185011\UMLS_C0037088\SNOMED_3947183016%' -- ACT ontology
      or concept_path like '%ICD10%U07.1%'                                                         -- ICD10 ontology
--      or concept_path like '...' -- add 1st local concept_path and uncomment it
--      or concept_path like '...' -- add 2nd local concept_path and uncomment it
)
select obs.patient_num
      ,obs.encounter_num
      ,2 
      ,0 
from "&&i2b2schema".observation_fact obs
join pos_cd
on obs.concept_cd = pos_cd.concept_cd
where obs.start_date >= '01-JAN-2020'
;
commit;


/*********************************************************************************/
/*2. identify additional data elements that may not currently be available in CDM*/
/*********************************************************************************/






