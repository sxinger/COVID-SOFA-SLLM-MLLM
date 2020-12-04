##====== overhead =====
rm(list=ls())
gc()


project_subject<-"COVID"
setwd(paste0("~/",project_subject))

source("./R/util.R")
require_libraries(c(  "ROracle"
                     ,"DBI"
))

config_file<-read.csv('./config_nh.csv')
conn<-connect_to_db("Oracle","OCI",config_file[2,])


##======= load data ======
#--- case
enc_case<-dbGetQuery(conn,"select * from eligible_enc_case") 
dat_case<-dbGetQuery(conn,"select * from covid_case_i2b2")

#--- control
enc_ctrl<-dbGetQuery(conn,"select * from eligible_enc_ctrl")
#---since control set is pretty large, we will load it in in random chunks
# dat_ctrl<-dbGetQuery(conn,"select * from covid_ctrl_i2b2") # may take forever
chunk_load(conn,
           dataset="covid_ctrl_i2b2",
           by_row = F,
           sample_by="PATIENT_NUM",
           download_chunk=T)

#--- pre-select variables
code_map_man<-dbGetQuery(conn,"select * from code_map")


##====== save raw data
saveRDS(enc_case,file="./data/raw/case_enc.rda")
saveRDS(enc_ctrl,file="./data/raw/ctrl_enc.rda")

saveRDS(dat_case,file="./data/raw/case_i2b2.rda")
saveRDS(dat_ctrl,file="./data/raw/ctrl_i2b2.rda")

saveRDS(code_map_man,file="./data/raw/code_map_sel.rda")


