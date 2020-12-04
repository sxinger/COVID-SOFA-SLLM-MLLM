##======= overhead ====
rm(list=ls())
gc()

project_subject<-"COVID"
setwd(paste0("~/",project_subject))

source("./R/util.R")
require_libraries(c( "dplyr"
                     ,"tidyr"
                     ,"magrittr"
                     ,"stringr"
                     ,"purrr"
                     ,"broom"
                     ,"tibble"
                     ,"ggplot2"
                     ,"ggrepel"
))

##======= load data =======
enc<-readRDS("./data/raw/case_enc.rda") %>% 
  bind_rows(readRDS("./data/raw/ctrl_enc.rda") %>%
              mutate(INFECTION_START_DTTM=as.POSIXct('1900-01-01'),
                     AGE_AT_INF=NA_integer_))

dat<-readRDS("./data/raw/case_i2b2.rda") %>%
  bind_rows(readRDS("./data/raw/covid_ctrl_i2b2_0.rda")) %>%
  bind_rows(readRDS("./data/raw/covid_ctrl_i2b2_1.rda")) %>%
  bind_rows(readRDS("./data/raw/covid_ctrl_i2b2_2.rda")) %>%
  bind_rows(readRDS("./data/raw/covid_ctrl_i2b2_3.rda")) %>%
  bind_rows(readRDS("./data/raw/covid_ctrl_i2b2_4.rda")) %>%
  bind_rows(readRDS("./data/raw/covid_ctrl_i2b2_5.rda")) %>%
  bind_rows(readRDS("./data/raw/covid_ctrl_i2b2_6.rda")) %>%
  bind_rows(readRDS("./data/raw/covid_ctrl_i2b2_7.rda")) %>%
  bind_rows(readRDS("./data/raw/covid_ctrl_i2b2_8.rda")) %>%
  bind_rows(readRDS("./data/raw/covid_ctrl_i2b2_9.rda")) %>%
  semi_join(enc,by="PATIENT_NUM")

code<-readRDS("./data/raw/code_map_sel.rda")
sev_score_ref<-read.csv("./ref/SEV_SCORE.csv",stringsAsFactors = F) %>%
  filter(!is.na(SCORE))

##======= identify outcome - MV exposure ========
mv<-dat %>% 
  semi_join(enc,by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  inner_join(code %>% filter(CODE %in% c("Vent_init","Vent_mode")) %>% select(CODE,I2B2_CODE),
             by=c("CONCEPT_CD"="I2B2_CODE")) %>%
  filter(START_DATE>="2020-03-07") %>%
  filter(!(CODE=="Vent_mode"&grepl("spont",tolower(TVAL_CHAR)))) %>%
  left_join(enc %>% select(PATIENT_NUM,ENCOUNTER_NUM,ICU_STAY_START_DTTM,ICU_STAY_END_DTTM,ICU_LENGTH_OF_STAY_DAYS),
            by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  filter((START_DATE<=ICU_STAY_END_DTTM&START_DATE>=ICU_STAY_START_DTTM)|is.na(ICU_STAY_START_DTTM)) %>%
  group_by(PATIENT_NUM,ENCOUNTER_NUM,ICU_STAY_END_DTTM,CODE) %>%
  dplyr::summarise(min_dttm=min(START_DATE),
                   max_dttm=max(START_DATE)) %>% #dedup
  ungroup %>% gather(summ,val,-PATIENT_NUM,-ENCOUNTER_NUM,-ICU_STAY_END_DTTM,-CODE) %>%
  unite("code_summ",c("CODE","summ"),sep="_") %>%
  spread(code_summ,val) %>%
  mutate(MV_INIT=coalesce(Vent_init_min_dttm,Vent_mode_min_dttm),
         MV_DUR_HOUR=round(as.numeric(difftime(coalesce(Vent_mode_max_dttm,ICU_STAY_END_DTTM),MV_INIT,units="hour")),2)) %>%
  select(PATIENT_NUM,MV_INIT,MV_DUR_HOUR) %>% filter(!is.na(MV_DUR_HOUR)) %>% unique

#---!! issue: last mv record at 08/11, losing a full month of data


##======= feature engineering - SOFA ========
#--additional processing is needed for MAP
map<-dat %>% 
  semi_join(enc,by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  filter(START_DATE>="2020-03-07") %>%
  inner_join(code %>% filter(CODE %in% c("SBP","DBP","MAP")) %>% select(CODE,I2B2_CODE),
             by=c("CONCEPT_CD"="I2B2_CODE")) %>%
  select(PATIENT_NUM,ENCOUNTER_NUM,INSTANCE_NUM,CODE,START_DATE,NVAL_NUM) %>%
  unique %>% spread(CODE,NVAL_NUM) %>%
  mutate(MAP=case_when(!is.na(SBP)&!is.na(DBP)~round((2*DBP+SBP)/3),
                       TRUE ~ MAP)) %>%
  filter(!is.na(MAP))

#---last BP record at 08/12, losing a full month of data


sofa_all<-dat %>% 
  semi_join(enc,by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  filter(START_DATE>="2020-03-07") %>%
  select(PATIENT_NUM,ENCOUNTER_NUM,CONCEPT_CD,NVAL_NUM, START_DATE) %>%
  #identify concepts for calculating sofa score 
  inner_join(code %>% inner_join(sev_score_ref,by=c("CODE")), 
             by=c("CONCEPT_CD"="I2B2_CODE")) %>%
  #add preprocessed MAP values
  filter(!CODE %in% c("SBP","DBP","MAP")) %>%
  bind_rows(map %>% select(PATIENT_NUM,ENCOUNTER_NUM,START_DATE,MAP) %>%
              mutate(CODE="MAP") %>% inner_join(sev_score_ref, by="CODE") %>%
              dplyr::rename("NVAL_NUM"="MAP","CONCEPT_CD"="CODE")) %>%
  #assign organ-specific scores for all records
  mutate(pat_score=case_when(DIRECTION=="l"&NVAL_NUM<UB~SCORE,
                             DIRECTION=="le"&NVAL_NUM<=UB~SCORE,
                             DIRECTION=="g"&NVAL_NUM>LB~SCORE,
                             DIRECTION=="btw"&SCALE_TYPE=="c"&NVAL_NUM>=LB&NVAL_NUM<UB~SCORE,
                             DIRECTION=="btw"&SCALE_TYPE=="d"&NVAL_NUM>=LB&NVAL_NUM<=UB~SCORE
                             )) %>%
  filter(!is.na(pat_score)) %>% 
  #identify ICU sessions
  inner_join(enc %>% select(PATIENT_NUM,POS_CONFIRMED,HOSDEAD,AGE_AT_ADMIT,SEX_FEMALE,
                            HOSP_ADMSN_DTTM,ICU_STAY_START_DTTM,ICU_LENGTH_OF_STAY_DAYS),
             by=c("PATIENT_NUM")) %>%
  filter(START_DATE >= HOSP_ADMSN_DTTM-72*3600) %>%
  # filter(START_DATE >= "2020-01-01") %>%
  #calculate score time w.r.t. ICU stay
  mutate(hr_since_admin=round(as.numeric(difftime(START_DATE,HOSP_ADMSN_DTTM,units="hour"))),
         hr_since_icu=round(as.numeric(difftime(START_DATE,ICU_STAY_START_DTTM,units="hour"))),
         hr_since_icu=pmin(pmax(hr_since_icu,0,na.rm=T),round(ICU_LENGTH_OF_STAY_DAYS*24),na.rm=T)) %>%
  #attach MV duration (primary outcome)
  left_join(mv,by=c("PATIENT_NUM")) %>%
  filter(is.na(MV_INIT) | MV_INIT>=START_DATE-24*3600) %>%
  mutate(hr_since_mv=round(as.numeric(difftime(START_DATE,MV_INIT,units="hour"))),
         hr_since_mv=pmin(pmax(hr_since_mv,0,na.rm=T),round(MV_DUR_HOUR),na.rm=T))

#----- worst sofa score at intervals ----
sofa_by_hr<-sofa_all %>%
  group_by(PATIENT_NUM,ENCOUNTER_NUM,ORGAN,
           hr_since_admin,hr_since_icu,hr_since_mv) 

sofa_by_hr_admin<-c()
sofa_by_hr_icu<-c()
sofa_by_hr_mv<-c()
for(hr in seq(1,12)){
  sofa_by_hr_admin %<>%
    bind_rows(sofa_by_hr %>%
                filter(hr_since_admin<=hr) %>%
                arrange(desc(pat_score)) %>% dplyr::slice(1:1) %>% ungroup %>%
                group_by(PATIENT_NUM,ENCOUNTER_NUM,POS_CONFIRMED,HOSDEAD,AGE_AT_ADMIT,SEX_FEMALE,
                         hr_since_admin) %>%
                dplyr::summarise(SOFA=sum(pat_score),
                                 ORGAN_scored=length(unique(ORGAN))) %>% ungroup %>%
                arrange(PATIENT_NUM,ENCOUNTER_NUM) %>%
                mutate(hr_since_admin_cum = hr))
  
  sofa_by_hr_icu %<>%
    bind_rows(sofa_by_hr %>%
                filter(hr_since_icu<=hr) %>%
                arrange(desc(pat_score)) %>% dplyr::slice(1:1) %>% ungroup %>%
                group_by(PATIENT_NUM,ENCOUNTER_NUM,POS_CONFIRMED,HOSDEAD,AGE_AT_ADMIT,SEX_FEMALE,
                         hr_since_icu) %>%
                dplyr::summarise(SOFA=sum(pat_score),
                                 ORGAN_scored=length(unique(ORGAN))) %>% ungroup %>%
                arrange(PATIENT_NUM,ENCOUNTER_NUM) %>%
                mutate(hr_since_icu_cum = hr))
  
  sofa_by_hr_mv %<>%
    bind_rows(sofa_by_hr %>%
                filter(hr_since_mv<=hr) %>%
                arrange(desc(pat_score)) %>% dplyr::slice(1:1) %>% ungroup %>%
                group_by(PATIENT_NUM,ENCOUNTER_NUM,POS_CONFIRMED,HOSDEAD,AGE_AT_ADMIT,SEX_FEMALE,
                         hr_since_mv) %>%
                dplyr::summarise(SOFA=sum(pat_score),
                                 ORGAN_scored=length(unique(ORGAN))) %>% ungroup %>%
                arrange(PATIENT_NUM,ENCOUNTER_NUM) %>%
                mutate(hr_since_mv_cum = hr))
}





#----- sofa-score based linear models
#--- initial sofa score
sofa_init<-sofa_all %>%
  group_by(PATIENT_NUM,ENCOUNTER_NUM,ORGAN) %>%
  arrange(START_DATE) %>% dplyr::slice(1:1) %>% ungroup %>%
  group_by(PATIENT_NUM,ENCOUNTER_NUM,POS_CONFIRMED,HOSDEAD,AGE_AT_ADMIT,SEX_FEMALE,MV_DUR_HOUR) %>%
  dplyr::summarise(SOFA_init=sum(pat_score),
                   SOFA_init_since_admin=max(hr_since_admin),
                   SOFA_init_since_icu=max(hr_since_icu))

prop.table(table(sofa_init$HOSDEAD,sofa_init$POS_CONFIRMED),2)
prop.table(table(sofa_init$HOSDEAD,sofa_init$SOFA_init),2)
prop.table(table(sofa_init$HOSDEAD,sofa_init$SOFA_init),1)

quantile(sofa_init$SOFA_init_since_admin,c(0.5,0.8,0.9,0.95))
quantile(sofa_init$SOFA_init_since_icu,c(0.5,0.8,0.9,0.95),na.rm=T)

# SOFA score range: 0 - 24

#highest SOFA score may be
# SOFA > 12 -> 50% mortality, historically
# trend of SOFA scores 
# 1 point for organ dysfunction
### nurse reported (75%) 
# performance status (e.g. morbility, independence)


fit1<-glm(HOSDEAD ~ POS_CONFIRMED + SOFA_init,
          data=sofa_init,family="binomial")
summary(fit1)
get_perf_summ(fit1$fitted.values,fit1$y)

fit1p<-glm(HOSDEAD ~ SOFA_init,
          data=sofa_init %>% filter(POS_CONFIRMED=="covidpos"),
          family="binomial")

summary(fit1p)
get_perf_summ(fit1p$fitted.values,fit1p$y)

fit1n<-glm(HOSDEAD ~ SOFA_init,
           data=sofa_init %>% filter(POS_CONFIRMED=="covidneg"),
           family="binomial")
summary(fit1n)
get_perf_summ(fit1n$fitted.values,fit1n$y)


fit2<-glm(HOSDEAD ~ POS_CONFIRMED + AGE_AT_ADMIT + SOFA_init,
          data=sofa_init,family="binomial")
summary(fit2)
get_perf_summ(fit2$fitted.values,fit2$y)


fit3<-glm((MV_DUR_HOUR >= 96)  ~ POS_CONFIRMED + SOFA_init,
          data=sofa_init,family="binomial")
summary(fit3)
get_perf_summ(fit3$fitted.values,fit3$y)

fit4<-glm(MV_DUR_HOUR ~ POS_CONFIRMED + AGE_AT_ADMIT + SOFA_init,
          data=sofa_init,family="poisson")
summary(fit4)
get_perf_summ(fit4$fitted.values,as.numeric(fit4$y>=96))


sofa_hr_admin<-sofa_all 
sofa_hr_icu<-
  sofa_hr_mv<-




#TODO:saps_48hr
  

#TODO:apache_48hr





##======= pre-screening variables ========##





##======= feature pre-selection ========##










