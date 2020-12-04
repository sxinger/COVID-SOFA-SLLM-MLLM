/* Table 7 - Prescribing Table
   
   Note that: observation time window is within [admit_date - 365d, discharge_date + 30d]
*/

/*connect to most up-to-date CDM instance containing COVID patients*/

-- make sure that the eligible encounter table is accessible
select * from covid_eligible_enc;

whenever sqlerror continue;
drop table covid_pmed;
whenever sqlerror exit;

/*collect the demographic table*/
create table covid_pmed as
select enc.patid
      ,pmed.encounterid
      ,pmed.prescribingid
      ,pmed.rxnorm_cui
      ,pmed.rx_order_date
      ,pmed.rx_start_date
      ,pmed.rx_days_supply
      ,pmed.rx_refills
      ,pmed.rx_basis
      ,pmed.raw_rx_med_name
from covid_eligible_enc enc
join "&&CDM_schema".PRESCRIBING pmed on pmed.patid = enc.patid and
      pmed.rx_start_date between enc.admit_date - 365 and enc.discharge_date + 30     
;