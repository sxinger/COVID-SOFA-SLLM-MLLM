setwd("~/#Projects/COVID")

rm(list=ls()); gc()
install.packages(c("ggthemes",
                   "zipcode",
                   "maps"))

library(dplyr)
library(tidyr)
library(magrittr)
library(stringr)
library(ggplot2)
library(zipcode)
library(maps)
library(gridExtra)
library(grid)
library(ggthemes)
library(scales)
library(ggrepel)

#load data
gpc_pat_cnt<-read.csv("./data/gpc_covid_overview.csv",stringsAsFactors = F) %>%
  mutate(state=tolower(state))

## plot
us<-map_data("state")
long_lim<-c(min(us$long),max(us$long))
lat_lim<-c(min(us$lat),max(us$lat))
stannote<-data.frame(
  long=state.center$x,
  lat=state.center$y,
  state_abb=state.abb)

out_us<-gpc_pat_cnt %>%
  filter(site_lon < long_lim[1] | site_lon > long_lim[2]) %>%
  filter(site_lat < lat_lim[1] | site_lat > lat_lim[2]) %>%
  arrange(desc(test))

gpc_pat_cnt %<>%
  filter(site_lon >= long_lim[1] & site_lon <= long_lim[2]) %>%
  filter(site_lat >= lat_lim[1] & site_lat <= lat_lim[2])

us_dat<-us %>%
  left_join(gpc_pat_cnt %>% select(state,state_pos) %>% unique, 
            by=c("region"="state"))

ggplot(us_dat,aes(x=long,y=lat))+
  geom_polygon(aes(fill=state_pos,group=group),color='grey')+
  scale_fill_continuous("State Area (1000 miles)",
                        breaks=seq(5,25,by=5)*1e4,
                        labels=seq(5,25,by=5)*10,
                        trans = "reverse",
                        guide_legend(direction = "horizontal"))+
  geom_point(data=gpc_pat_cnt %>% filter(!is.na(test)),
             aes(x=site_lon,y=site_lat,size=test),alpha=0.5,color="orange")+
  geom_point(data=gpc_pat_cnt %>% filter(!is.na(pos)),
             aes(x=site_lon,y=site_lat,size=pos),alpha=0.5,color="green")+
  geom_label_repel(data=gpc_pat_cnt %>% filter(!is.na(test)) %>% 
                     select(site_lon,site_lat,site_short, test, pos) %>% unique %>%
                     mutate(pos=case_when(pos < 11 ~ "< 11",
                                          TRUE ~ as.character(pos))),
                   aes(x=site_lon,y=site_lat,label=paste0(site_short,":",test,"(",pos,")")),
                   label.size = NA, 
                   fontface="bold",
                   alpha = 0.2, 
                   size=5,
                   label.padding=.1, 
                   na.rm=TRUE,
                   seed = 1234)+
  geom_label_repel(data=gpc_pat_cnt %>% filter(!is.na(test)) %>% 
                     select(site_lon,site_lat,site_short, test, pos) %>% unique %>%
                     mutate(pos=case_when(pos < 11 ~ "< 11",
                                          TRUE ~ as.character(pos))),
                   aes(x=site_lon,y=site_lat,label=paste0(site_short,":",test,"(",pos,")")),
                   label.size = NA,
                   color="red",
                   fontface="bold",
                   alpha = 1,
                   size=5,
                   label.padding=.1,
                   na.rm=TRUE,
                   fill = NA,
                   seed = 1234) +
  scale_size_continuous(range=c(5,30))+
  guides(size=FALSE, alpha=FALSE)+
  labs(subtitle = "GPC site: # of tested cases (# of positive PCR)",
       caption = "Estimated as of 04/20/2020")+
  theme(text=element_text(face="bold",size=20),
        panel.background = element_rect(fill = NA, colour = NA),
        panel.grid.major = element_line(colour = NA))

