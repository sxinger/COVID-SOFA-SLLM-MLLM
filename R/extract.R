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
                     ,"ROracle"
                     ,"DBI"
))

config_file<-read.csv('./config_nh.csv')
conn<-connect_to_db("Oracle","OCI",config_file[2,])


##===== TODO
