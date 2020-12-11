/* This script is for collecting additional information from source i2b2 datawarehouse
  1. Identifying patients/encounters that:
  - a. has been tested for COVID19; OR
  - b. has a COVID19 diagnosis;
  
  2. Extract additional data elements that may not be currently available in CDM
   - Start Date&Time and End Date&Time of ICU stay --> Primary outcome: ICU LOS
   - Respiratory rate (breaths/min)
   - Temperature (C)
   - Hear Rate (/min)
   - Glasgow Coma Scale
   - SpO2,Pulse oximetry,Oxygen Saturation (%)
   - Invasive Mechanical ventilation initiation  --> Primary outcome: IMV exposure  
   - Invasive Mechanical ventilation completion  --> Primary outcome: IMV exposure
   - Invasive Mechanical ventilation mode (spontaneous, assisted, controlled) --> Primary outcome: IMV exposure
   - PaO2, partial pressure of oxygen (mmHG) [only available with MV]
   - FiO2, fraction of inspired oxygen (%) [only available with MV]
   - PaO2/FiO2 [only available with MV]
*/

/*connect to most up-to-date i2b2 instance*/

whenever sqlerror continue;
drop table covid_enc;
drop table code_map_obsclinc purge;
whenever sqlerror exit;

create table covid_enc (
 patient_num varchar2(80),
 encounter_num varchar2(80),
 pos_confirmed integer,  --2=confirmed positive; 1=pending; 0=confirmed negative
 lab_int integer        --1=with lab; 0=without lab
);

create table code_map_obsclinc (
    RAW_OBSCLIN_NAME varchar(100) not null, 
    OBSCLIN_CODE varchar(20), 
    OBSCLIN_TYPE varchar(20), 
    i2b2_code varchar(100) not null,  
    i2b2_label varchar(400) not null -- i2b2 concept label (human-readable)
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
      and obs.patient_num not in (select patient_num from covid_enc)
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
       and obs.patient_num not in (select patient_num from covid_enc)
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
      and obs.patient_num not in (select patient_num from covid_enc)
;
commit;


-- create index for better efficiency
create index covid_enc_patenc_idx on covid_enc(patient_num, encounter_num);



/*********************************************************************************/
/*2. identify additional data elements that may not currently be available in CDM*/
/*********************************************************************************/
-- Invasive Mechanical Ventilation/Intubation
insert into code_map_obsclinc
    select t.name_raw
          ,t.code
          ,t.code_type
          ,t.i2b2_code
          ,t.i2b2_label
    from (
     select 'IMV_init' name_raw
           , null code
           ,'SCR' code_type
           ,'KUH|FLO_MEAS_ID:6687_SEL' i2b2_code  -- !!replace with local i2b2 concepts
           ,'ventilation initial' i2b2_label      
     from dual
     -- add more chunks as above if there are multiple concepts used for indicating ventilation initiation/completion
        union
     select 'IMV_mode'
           ,'20124-4'
           ,'LC'
           ,'KUH|FLO_MEAS_ID:3309'  -- !!replace with local i2b2 concepts
           ,'ventilation mode'      
    from dual
    -- add more chunks as above if there are multiple concepts used for indicating ventilator mode
        union 
     select 'IMV_TV'
           ,'20112-9'
           ,'LC'
           ,'KUH|FLO_MEAS_ID:3314'  -- !!replace with local i2b2 concepts
           ,'tidal volume setting' 
      from dual
      -- add more chunks as above if there are multiple concepts used for indicating tidal volume setting
        union
     select 'IMV_RR'
           ,'19834-1'
           ,'LC'
           ,'KUH|FLO_MEAS_ID:3318'   -- !!replace with local i2b2 concepts
           ,'respiratory rate setting' 
     from dual   
     -- add more chunks as above if there are multiple concepts used for indicating respiratory rate setting
         union
     select 'IMV_PEEP' name_raw
           ,'20077-4' code
           ,'LC' code_type
           ,'KUH|FLO_MEAS_ID:3326' i2b2_code
           ,'positive end expiratory pressure' i2b2_label 
     from dual
     -- add more chunks as above if there are multiple concepts used for indicating positive end expiratory pressure
         union
     select 'ETVS'
           ,'20107-9'
           ,'LC'
           ,'KUH|FLO_MEAS_ID:3315'  -- !!replace with local i2b2 concepts
           ,'expired tidal volume spontaneous' 
     from dual
     -- add more chunks as above if there are multiple concepts used for indicating expired tidal volume spontaneous
        union
     select 'INTUBE'
           ,'33443-3'
           ,'LC'
           ,'LDA|2118:700'   -- !!replace with local i2b2 concepts
           ,'endotracheal tube placement date' 
     from dual   
     -- add more chunks as above if there are multiple concepts used for indicating endotracheal tube placement 
        union
     select 'INTUBE'
           ,'33443-3'
           ,'LC'
           ,'LDA|4537:700'   -- !!replace with local i2b2 concepts
           ,'nasalendotracheal tube placement date' 
     from dual   
     -- add more chunks as above if there are multiple concepts used for indicating nasalendotracheal tube placement
        union
     select 'INTUBE'
           ,'33443-3'
           ,'LC'
           ,'LDA|2095:700'   -- !!replace with local i2b2 concepts
           ,'tracheostomy tube placement date' 
     from dual   
     -- add more chunks as above if there are multiple concepts used for indicating tracheostomy tube placement 
        union
     select 'INTUBE'
           ,'33443-3'
           ,'LC'
           ,'LDA|4567:700'   -- !!replace with local i2b2 concepts
           ,'oralendotracheal tube placement date' 
     from dual   
     -- add more chunks as above if there are multiple concepts used for indicating oralendotracheal tube placement 
    ) t;
commit; 


-- Respiratory rate (breaths/min)
-- Temperature (C)
-- Hear Rate (/min)
-- Glasgow Coma Scale
-- SpO2,Pulse oximetry,Oxygen Saturation (%)
-- PaO2, partial pressure of oxygen (mmHG) [only available with MV]
-- FiO2, fraction of inspired oxygen (%) [only available with MV]
-- PaO2/FiO2 [only available with MV]
insert into code_map_obsclinc 
	select t.name_raw
          ,t.code
          ,t.code_type
          ,t.i2b2_code
          ,t.i2b2_label
	from (
        select 'RR' name_raw
              ,'9279-1' code
              ,'LC' code_type
              ,'KUH|FLO_MEAS_ID:9' i2b2_code
              ,'respiratory rate' i2b2_label 
        from dual
            union
        select 'RR'
              ,'9279-1'
              ,'LC' 
              ,'KUH|FLO_MEAS_ID:1190' 
              ,'respiratory rate' 
        from dual
            union
        select 'TEMP'
              ,'8310-5'
              ,'LC'
              ,'KUH|FLO_MEAS_ID:6'
              ,'temperature' 
        from dual
           union 
        select 'HR'
              ,'8867-4'
              ,'LC'
              ,'KUH|FLO_MEAS_ID:8'
              ,'heart rate' 
        from dual   
           union 
        select 'GCS'
              ,'35088-4'
              ,'1005'
              ,'@'
              ,'Glascow Coma scale' 
        from dual
           union
        select 'SpO2'
              ,'2708-6'
              ,'LC'
              ,'KUH|FLO_MEAS_ID:10'
              ,'SpO2' 
        from dual
            union
		select 'FiO2'
              ,'19996-8'
              ,'LC'
              ,'KUH|FLO_MEAS_ID:10124'
              ,'FiO2' 
        from dual   
            union 
        select 'FiO2'
              ,'19996-8'
              ,'14447'
              ,'mmHg'
              ,'FiO2' 
        from dual 
            union
        select 'FiO2'
              ,'19996-8'
              ,'LC'
              ,'KUH|FLO_MEAS_ID:301550'
              ,'FiO2' 
        from dual
            union
        select 'PaO2'
              ,'2703-7'
              ,'9518'
              ,'mmHg'
              ,'PaO2' 
        from dual
            union
        select 'PaFiO2'
              ,'50983-6'
              ,'LC'
              ,'KUH|FLO_MEAS_ID:4916'
              ,'PaO2/FiO2' 
        from dual
	) t
;
commit;



