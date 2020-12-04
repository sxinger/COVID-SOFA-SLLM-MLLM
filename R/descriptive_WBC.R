rm(list=ls())
gc()


project_subject<-"COVID"
setwd(paste0("~/",project_subject))

source("./R/util.R")
require_libraries(c( "dplyr"
                    ,"tidyr"
                    ,"magrittr"
                    ,"stringr"
                    ,"ROracle"
                    ,"DBI"
                    ,"purrr"
                    ,"broom"
                    ,"tibble"
                    ,"ggplot2"))

config_file<-read.csv('./config_nh.csv')
conn<-connect_to_db("Oracle","OCI",config_file)

#--load data
data_at_enc<-dbGetQuery(conn,"select * from covid_pos_data")

####===preprocessing====####
#--identify WBC measures
wbc_cd<-read.csv("./ref/WBC.csv",stringsAsFactors = F) %>%
  mutate(COMPONENT_ID=as.character(COMPONENT_ID))

wbc<-data_at_enc %>%
  filter(grepl("KUH\\|COMPONENT_ID",CONCEPT_CD)) %>%
  mutate(COMPONENT_ID=gsub(".*\\:","",CONCEPT_CD)) %>%
  inner_join(wbc_cd,by="COMPONENT_ID")

wbc_num<-wbc %>% filter(TVAL_CHAR=="E")  

#-- data engineering
wbc_num %<>%
  mutate(UNITS_CD=toupper(UNITS_CD)) %>%
  filter(toupper(UNITS_CD)=="K/UL") %>% 
  dplyr::rename("UNITS"="UNITS_CD") %>%
  select(PATIENT_NUM, ENCOUNTER_NUM, WBC_TYPE, COMPONENT_ID, NVAL_NUM, UNITS, START_DATE, START_SINCE_LAB) %>% 
  unique

#calculated percentages
wbc_perc<-wbc_num %>%
  filter(WBC_TYPE != "WBC") %>% 
  inner_join(wbc_num %>% filter(WBC_TYPE == "WBC") %>%
               select(PATIENT_NUM, ENCOUNTER_NUM, START_SINCE_LAB, NVAL_NUM) %>%
               rename("NVAL_WBC"="NVAL_NUM"),
             by=c("PATIENT_NUM","ENCOUNTER_NUM","START_SINCE_LAB")) %>%
  mutate(NVAL_NUM=case_when(NVAL_NUM>NVAL_WBC ~ round(NVAL_NUM/NVAL_WBC,3)/10,
                            TRUE ~ round(NVAL_NUM/NVAL_WBC,3)*100),
         UNITS="%") %>%
  select(-NVAL_WBC) %>% unique


#X-to-lymphocyte ratio
wbc_lymph<-wbc_num %>%
  filter(!WBC_TYPE %in% c("WBC","Lymphocyte")) %>% 
  inner_join(wbc_num %>% filter(WBC_TYPE == "Lymphocyte") %>%
               select(PATIENT_NUM, ENCOUNTER_NUM, START_SINCE_LAB, NVAL_NUM) %>%
               rename("NVAL_LYMPH"="NVAL_NUM"),
             by=c("PATIENT_NUM","ENCOUNTER_NUM","START_SINCE_LAB")) %>%
  mutate(NVAL_NUM=case_when(NVAL_LYMPH > 0 ~ round(NVAL_NUM/NVAL_LYMPH,3),
                            TRUE ~ 0),
         WBC_TYPE=paste0(WBC_TYPE,"-to-Lymph"),
         UNITS="ratio") %>%
  select(-NVAL_LYMPH) %>% unique


#X-to-Neutrophil ratio
wbc_neut<-wbc_num %>%
  filter(!WBC_TYPE %in% c("WBC","Neutrophils")) %>% 
  inner_join(wbc_num %>% filter(WBC_TYPE == "Neutrophils") %>%
               select(PATIENT_NUM, ENCOUNTER_NUM, START_SINCE_LAB, NVAL_NUM) %>%
               rename("NVAL_NEUT"="NVAL_NUM"),
             by=c("PATIENT_NUM","ENCOUNTER_NUM","START_SINCE_LAB")) %>%
  mutate(NVAL_NUM=case_when(NVAL_NEUT > 0 ~ round(NVAL_NUM/NVAL_NEUT,3),
                            TRUE ~ 0),
         WBC_TYPE=paste0(WBC_TYPE,"-to-Neut"),
         UNITS="ratio") %>%
  select(-NVAL_NEUT) %>% unique


wbc_num %<>%
  inner_join(wbc_cd %>% select(COMPONENT_ID,NORMAL_LOW,NORMAL_HIGH),
             by="COMPONENT_ID") %>%
  mutate(NVAL_FAC=case_when(NVAL_NUM<NORMAL_LOW ~ 'low',
                            NVAL_NUM>NORMAL_HIGH ~ 'high',
                            TRUE ~ 'normal')) %>%
  select(-NORMAL_LOW,-NORMAL_HIGH) %>%
  bind_rows(wbc_perc %>%
              inner_join(wbc_cd %>% select(COMPONENT_ID,NORMAL_LOWP,NORMAL_HIGHP),
                         by="COMPONENT_ID") %>%
              mutate(NVAL_FAC=case_when(NVAL_NUM<NORMAL_LOWP ~ 'low',
                                        NVAL_NUM>NORMAL_HIGHP ~ 'high',
                                        TRUE ~ 'normal')) %>%
              select(-NORMAL_LOWP,-NORMAL_HIGHP)) %>%
  bind_rows(wbc_lymph) %>%
  bind_rows(wbc_neut)

saveRDS(wbc_num,file="./data/covid_pos_wbc_raw.rda")


####=======profile WBC========####
##-------initial values----------##
wbc_num_init<-wbc_num %>%
  group_by(PATIENT_NUM,ENCOUNTER_NUM,WBC_TYPE,COMPONENT_ID,UNITS) %>%
  arrange(desc(START_SINCE_LAB)) %>%
  dplyr::slice(1:1) %>% ungroup

length(unique(wbc_num_init$ENCOUNTER_NUM))

#--tabulation
wbc_num_init %>% 
  group_by(WBC_TYPE,UNITS) %>%
  dplyr::summarize(val_min=min(NVAL_NUM,na.rm=T),
                   val_5perc=quantile(NVAL_NUM,0.05,na.rm=T),
                   val_25perc=quantile(NVAL_NUM,0.25,na.rm=T),
                   val_50perc=quantile(NVAL_NUM,0.50,na.rm=T),
                   val_75perc=quantile(NVAL_NUM,0.75,na.rm=T),
                   val_95perc=quantile(NVAL_NUM,0.95,na.rm=T),
                   val_max=max(NVAL_NUM,na.rm=T)) %>%
  ungroup %>% View


