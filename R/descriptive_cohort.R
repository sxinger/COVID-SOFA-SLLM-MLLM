##====== overhead
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

#========= load data =====
enc_case<-readRDS("./data/raw/case_enc.rda")
enc_ctrl<-readRDS("./data/raw/ctrl_enc.rda")


##======== temporal trends======
date_mt<-data.frame(date=seq(as.Date("2020/03/01"),as.Date("2020/09/20"),by="day")) %>%
  mutate(date_daily=dense_rank(date),
         date_week=dense_rank(strftime(date,format="%V")),
         date_biweek=floor(date_week/2)+1,
         date_month=as.numeric(format(date,"%m"))) %>%
  gather(date_scale,date_rk,-date)

enc_case %<>%
  mutate(hosp_los=round(as.numeric(difftime(HOSP_DISCH_DTTM,HOSP_ADMSN_DTTM,units="day"))),
         icu_ind=as.numeric(!is.na(ICU_STAY_START_DTTM))) %>%
  mutate(ADT_PAT_CLASS=case_when(ED_EPISODE==1&ADT_PAT_CLASS=='Inpatient'~'Emergency-Inpatient',
                                 TRUE ~ ADT_PAT_CLASS))

enc_ctrl %<>%
  mutate(hosp_los=round(as.numeric(difftime(HOSP_DISCH_DTTM,HOSP_ADMSN_DTTM,units="day"))),
         icu_ind=as.numeric(!is.na(ICU_STAY_START_DTTM))) %>%
  mutate(ADT_PAT_CLASS=case_when(ED_EPISODE==1&ADT_PAT_CLASS=='Inpatient'~'Emergency-Inpatient',
                                 TRUE ~ ADT_PAT_CLASS))

#----daily ip covid incidence----
date_scale_sel<-"date_week" #change aggregation specificity w.r.t. different date granularities
enc_case_date<-enc_case %>%
  mutate(date_to_match=as.Date(HOSP_ADMSN_DTTM,format="%Y-%m-%d")) %>%
  inner_join(date_mt %>% filter(date_scale==date_scale_sel),
             by=c("date_to_match"="date")) %>%
  select(ENCOUNTER_NUM,ADT_PAT_CLASS,date_to_match,date_rk) %>%  
  group_by(date_rk,ADT_PAT_CLASS) %>%
  dplyr::mutate(confirmed=length(unique(ENCOUNTER_NUM))) %>%
  select(-ENCOUNTER_NUM) %>% dplyr::slice(1:1) %>% ungroup

png("./fig&tbl/covid_hsp_incidence.png",
    width = 2100, height = 960,units = "px", pointsize = 12, bg = "white", res = NA)

ggplot(enc_case_date, aes(x=date_rk,y=confirmed,color=ADT_PAT_CLASS))+
  geom_point(size=10)+geom_line(size=2)+
  scale_x_continuous(breaks=unique(enc_case_date$date_rk),
                     label=enc_case_date$date_to_match[!duplicated(enc_case_date$date_rk)])+
  labs(x="Date",y="New Admissions", title="positive covid hospitalizations over time")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text=element_text(size=30,face="bold"))

dev.off()

#----daily covid ip prevalence---- 
date_scale_sel<-"date_daily" #change aggregation specificity w.r.t. different date granularities
#https://www.kansashealthsystem.com/-/media/Project/Website/PDFs-for-Download/COVID19/Kansas-Covid-Cases-by-the-Numbers.pdf 

png("./fig&tbl/covid_hsp_prevalence.png",
    width = 3000, height = 900,units = "px", pointsize = 12, bg = "white", res = NA)

hosp_los_max<-max(enc_case$hosp_los[enc_case$POS_CONFIRMED>0],na.rm=T)
data_ip_los<-enc_case %>% filter(POS_CONFIRMED>0&!is.na(hosp_los)) %>%
  select(ENCOUNTER_NUM, ADT_PAT_CLASS, HOSP_ADMSN_DTTM, hosp_los)

data_ip_daily<-data_ip_los[rep(row.names(data_ip_los),pmax(data_ip_los$hosp_los,1)),] %>%
  mutate(hosp_los = 1) %>% group_by(ENCOUNTER_NUM) %>%
  dplyr::mutate(hosp_los_cum=cumsum(hosp_los)) %>% ungroup %>%
  mutate(HOSP_DATE=HOSP_ADMSN_DTTM+hosp_los_cum*3600*24) %>%
  mutate(date_to_match=as.Date(HOSP_DATE,format="%Y-%m-%d")) %>%
  inner_join(date_mt %>% filter(date_scale==date_scale_sel),
             by=c("date_to_match"="date")) %>%
  select(ENCOUNTER_NUM,ADT_PAT_CLASS,date_to_match,date_rk) %>%  
  group_by(date_rk,ADT_PAT_CLASS) %>%
  dplyr::mutate(confirmed=length(unique(ENCOUNTER_NUM))) %>%
  select(-ENCOUNTER_NUM) %>% dplyr::slice(1:1) %>% ungroup

ggplot(data_ip_daily %>% filter(date_to_match>= "2020-03-07"),
       aes(x=date_rk,y=confirmed,color=ADT_PAT_CLASS))+
  geom_point(size=10)+geom_line(size=2)+
  scale_x_continuous(breaks=unique(data_ip_daily$date_rk),
                     label=data_ip_daily$date_to_match[!duplicated(data_ip_daily$date_rk)])+
  labs(x="Date",y="Inpatient Census", title="positive covid hospitalizations over time")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text=element_text(size=15,face="bold"))

dev.off()


#----daily ICU incidence ---- 
date_scale_sel<-"date_week" #change aggregation specificity w.r.t. different date granularities
icu_case_date<-enc_case %>%
  filter(icu_ind==1) %>%
  mutate(date_to_match=as.Date(ICU_STAY_START_DTTM,format="%Y-%m-%d")) %>%
  inner_join(date_mt %>% filter(date_scale==date_scale_sel),
             by=c("date_to_match"="date")) %>%
  select(ENCOUNTER_NUM,date_to_match,date_rk) %>%  
  group_by(date_rk) %>%
  dplyr::mutate(confirmed=length(unique(ENCOUNTER_NUM))) %>%
  select(-ENCOUNTER_NUM) %>% dplyr::slice(1:1) %>% ungroup

png("./fig&tbl/covid_icu_incidence.png",
    width = 2100, height = 960,units = "px", pointsize = 12, bg = "white", res = NA)

ggplot(icu_case_date, aes(x=date_rk,y=confirmed))+
  geom_point(size=10)+geom_line(size=2)+
  scale_x_continuous(breaks=unique(icu_case_date$date_rk),
                     label=icu_case_date$date_to_match[!duplicated(icu_case_date$date_rk)])+
  labs(x="Date",y="New ICU Admissions", title="positive covid ICU stays over time")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text=element_text(size=30,face="bold"))

dev.off()


#----daily ICU prevalence---- 
date_scale_sel<-"date_daily" #change aggregation specificity w.r.t. different date granularities

icu_case<-enc_case %>% filter(icu_ind==1)
png("./fig&tbl/covid_icu_prevalence.png",
    width = 3000, height = 900,units = "px", pointsize = 12, bg = "white", res = NA)

icu_los_max<-max(icu_case$ICU_LENGTH_OF_STAY_DAYS[icu_case$POS_CONFIRMED>0],na.rm=T)
data_icu_los<-icu_case %>% filter(POS_CONFIRMED>0&!is.na(ICU_LENGTH_OF_STAY_DAYS)) %>%
  select(ENCOUNTER_NUM, ADT_PAT_CLASS, ICU_STAY_START_DTTM, ICU_LENGTH_OF_STAY_DAYS)

data_icu_daily<-data_icu_los[rep(row.names(data_icu_los),pmax(data_icu_los$ICU_LENGTH_OF_STAY_DAYS,1)),] %>%
  mutate(ICU_LENGTH_OF_STAY_DAYS = 1) %>% group_by(ENCOUNTER_NUM) %>%
  dplyr::mutate(ICU_LENGTH_OF_STAY_DAYS_cum=cumsum(ICU_LENGTH_OF_STAY_DAYS)) %>% ungroup %>%
  mutate(ICU_DATE=ICU_STAY_START_DTTM+ICU_LENGTH_OF_STAY_DAYS_cum*3600*24) %>%
  mutate(date_to_match=as.Date(ICU_DATE,format="%Y-%m-%d")) %>%
  inner_join(date_mt %>% filter(date_scale==date_scale_sel),
             by=c("date_to_match"="date")) %>%
  select(ENCOUNTER_NUM,ADT_PAT_CLASS,date_to_match,date_rk) %>%  
  group_by(date_rk,ADT_PAT_CLASS) %>%
  dplyr::mutate(confirmed=length(unique(ENCOUNTER_NUM))) %>%
  select(-ENCOUNTER_NUM) %>% dplyr::slice(1:1) %>% ungroup

ggplot(data_icu_daily,
       aes(x=date_rk,y=confirmed))+
  geom_point(size=10)+geom_line(size=2)+
  scale_x_continuous(breaks=unique(data_icu_daily$date_rk),
                     label=data_icu_daily$date_to_match[!duplicated(data_icu_daily$date_rk)])+
  labs(x="Date",y="ICU Census", title="positive covid ICU admissions over time")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text=element_text(size=15,face="bold"))

dev.off()

#----LOS summaries----
los<-enc_case %>%
  filter((ADT_PAT_CLASS!="Emergency")&POS_CONFIRMED==2&!is.na(hosp_los)) %>%
  mutate(hosp_los=pmax(hosp_los,1)) %>%
  group_by(PATIENT_NUM,ENCOUNTER_NUM) %>%
  arrange(desc(hosp_los)) %>% dplyr::slice(1:1) %>% ungroup

min(los$HOSP_ADMSN_DTTM)
max(los$HOSP_ADMSN_DTTM)
summary(los$hosp_los)
sd(los$hosp_los)

los %>%
  mutate(LOS_grp=case_when(hosp_los<=4 ~ '1-4',
                           hosp_los>4&hosp_los<=28 ~ '5-28',
                           hosp_los>28 ~ '28up',
                           TRUE ~ NA_character_)) %>%
  dplyr::mutate(pat_N=length(unique(PATIENT_NUM)),
                enc_N=length(unique(ENCOUNTER_NUM))) %>%
  group_by(LOS_grp,pat_N,enc_N) %>%
  dplyr::summarise(pat_cnt=length(unique(PATIENT_NUM)),
                   enc_cnt=length(unique(ENCOUNTER_NUM))) %>%
  View

#----age getting younger over time?--not necessarily-----
date_scale_sel<-"date_biweek"
age_ts<-enc_case %>%
  mutate(date_to_match=as.Date(HOSP_ADMSN_DTTM,format="%Y-%m-%d")) %>%
  inner_join(date_mt %>% filter(date_scale==date_scale_sel),
             by=c("date_to_match"="date")) %>%
  group_by(date_rk,icu_ind) %>%
  dplyr::summarise(date_to_match=date_to_match[1],
                   pat_cnt=length(unique(PATIENT_NUM)),
                   age_median=median(AGE_AT_ADMIT,na.rm=T),
                   age_q1=quantile(AGE_AT_ADMIT,0.25,na.rm=T),
                   age_q3=quantile(AGE_AT_ADMIT,0.75,na.rm=T)) %>% ungroup %>%
  mutate(label=paste0(pat_cnt,",",age_median))
  # mutate(label=paste0(ifelse(pat_cnt<11,'<11',pat_cnt),",",age_median)) #cell size suppression

png("./fig&tbl/covid_pos_ip_age_ts.png",
    width = 960, height = 960,units = "px", pointsize = 12, bg = "white", res = NA)

ggplot(age_ts,aes(x=date_rk,y=age_median,color=as.factor(icu_ind))) +
  geom_point(aes(size=pat_cnt))+ 
  geom_errorbar(aes(ymin=age_q1, ymax=age_q3),linetype=2)+
  geom_line()+
  geom_label_repel(aes(label=label),size=10,fontface="bold")+
  scale_x_continuous(breaks=unique(age_ts$date_rk),
                     label=age_ts$date_to_match[!duplicated(age_ts$date_rk)])+
  labs(x="date",y="age",title="age trend overtime for confirmed COVID-19 inpatients") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text=element_text(size=20,face="bold"))
dev.off()


#======== demographic comparisons ======
demo_stk<-enc_case %>%
  filter(grepl("Inpatient",ADT_PAT_CLASS)) %>%
  select(PATIENT_NUM,ENCOUNTER_NUM,POS_CONFIRMED,
         DISCH_DISP,ADMSN_DX_GROUP,SEX_FEMALE,AGE_AT_ADMIT,ICU_LENGTH_OF_STAY_DAYS) %>%
  bind_rows(enc_ctrl %>%
              filter(grepl("Inpatient",ADT_PAT_CLASS)) %>%
              select(PATIENT_NUM,ENCOUNTER_NUM,POS_CONFIRMED,
                     DISCH_DISP,ADMSN_DX_GROUP,SEX_FEMALE,AGE_AT_ADMIT,ICU_LENGTH_OF_STAY_DAYS)) %>%
  replace_na(list(EXPIRED_IN_5_DAYS_BOOL=0,
                  EXPIRED_IN_5_DAYS_BOOL=0)) %>%
  #--- create additional variables that may be interested to compare
  mutate(POS_CONFIRMED=case_when(POS_CONFIRMED==2 ~ 'POS',
                                 POS_CONFIRMED==0 ~ 'NEG',
                                 POS_CONFIRMED==1 ~ 'PEND',
                                 TRUE ~ NA_character_),
         ICU_IND=as.numeric((!is.na(ICU_LENGTH_OF_STAY_DAYS))),
         DISCH_DISP_GRP=case_when(grepl("(nursing|hospice|rehap|long term care|home health)+",tolower(DISCH_DISP)) ~ "SNF-HOSP-REHAP-LTC-HHA",
                                  grepl("(home or self)+",tolower(DISCH_DISP)) ~ "HOME",
                                  grepl("(expired)+",tolower(DISCH_DISP)) ~ "DEATH",
                                  TRUE ~ "OT"),
         ADMSN_DX_GRP=case_when(ADMSN_DX_GROUP %in% c("GENERAL SYMPTOMS AND SIGNS",
                                                      "OTHER BACTERIAL DISEASES",
                                                      "SYMPTOMS AND SIGNS INVOLVING THE CIRCULATORY AND RESPIRATORY SYSTEMS",
                                                      "SYMPTOMS AND SIGNS INVOLVING THE DIGESTIVE SYSTEM AND ABDOMEN",
                                                      "INFLUENZA AND PNEUMONIA"
         ) ~ ADMSN_DX_GROUP,
         grepl("PERSONS WITH POTENTIAL HEALTH HAZARDS",ADMSN_DX_GROUP) ~ "PERSONS WITH POTENTIAL HEALTH HAZARDS",
         is.na(ADMSN_DX_GROUP) ~ NA_character_,
         TRUE ~ "OT")) %>%
  group_by(PATIENT_NUM,ENCOUNTER_NUM) %>% 
  arrange(desc(ICU_IND,ICU_LENGTH_OF_STAY_DAYS)) %>%
  dplyr::slice(1:1) %>% ungroup

basic_covid<-univar_analysis_mixed(id=demo_stk$PATIENT_NUM,
                                   X=demo_stk[,-c(1:5)],
                                   grp=demo_stk$POS_CONFIRMED,
                                   data_type = c("cat",rep("num",2),rep("cat",3)),
                                   pretty=T)

write.csv(basic_covid,file="./fig&tbl/covid_basic.csv",row.names = F)
