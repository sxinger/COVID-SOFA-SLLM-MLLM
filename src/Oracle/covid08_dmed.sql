/* Table 8 - Dispensing Table
   
   Note that: observation time window is within [admit_date - 365d, discharge_date + 30d]
*/

/*connect to most up-to-date CDM instance containing COVID patients*/

-- make sure that the eligible encounter table is accessible
select * from covid_eligible_enc;

whenever sqlerror continue;
drop table covid_dmed;
whenever sqlerror exit;

/*collect the demographic table*/
create table covid_dmed as
select enc.patid
      ,dmed.prescribingid
      ,dmed.dispensingid
      ,dmed.ndc
      ,dmed.dispense_date
      ,dmed.dispense_sup
      ,dmed.dispense_amt
      ,dmed.raw_ndc
from covid_eligible_enc enc
join "&&CDM_schema".DISPENSING dmed on dmed.patid = enc.patid and
      dmed.rx_start_date between enc.admit_date - 365 and enc.discharge_date + 30     
;