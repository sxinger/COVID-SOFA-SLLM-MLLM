require_libraries<-function(package_list,verb=T){
  for (lib in package_list) {
    chk_install<-!(lib %in% installed.packages()[,"Package"])
    if(chk_install){
      install.packages(lib,repos = "http://cran.us.r-project.org")
    }
    library(lib, character.only=TRUE,lib.loc=.libPaths())
    if(verb){
      cat("\n", lib, " loaded.", sep="") 
    }
  }
}


unzip_redcap<-function(req_nm){
  #unzip and save
  zipF<-paste0("./",req_nm,"-raw.zip")
  unzip(zipF)
  
  #load date
  out<-list(pat_tbl=read.csv(paste0("./",req_nm,"/",req_nm,"-patient.csv"),stringsAsFactors = F,na.strings = c(""," ")),
            data_tbl=read.csv(paste0("./",req_nm,"/",req_nm,"-data.csv"),stringsAsFactors = F,na.strings = c(""," ")),
            code_info=read.csv(paste0("./",req_nm,"/",req_nm,"-code.info.csv"),stringsAsFactors = F,na.strings = c(""," ")))

  return(out)  
}


#exact matching with coarsening, with/without replacement
matched_sample<-function(case_ref,
                         ctrl_pool,
                         id_col,
                         exact=c(),
                         coarse=c(),
                         coarse_range=c(),
                         other_cov=c(),
                         weighting_scheme=c("weighted","hiearchical"),
                         wt=c(1),
                         match_rt=match_rt,
                         replace=FALSE,
                         verbose=TRUE){
  #only keep relavant columns
  case_ref %<>% select(c(id_col,exact,coarse,other_cov))
  ctrl_pool %<>% select(c(id_col,exact,coarse,other_cov))
  
  #--exact matching with coarsening (relative range)
  filter_cond_vec<-c()
  for(cond_i in seq_along(coarse)){
    filter_cond_vec<-c(filter_cond_vec,
                       paste0(coarse[cond_i],".y",">=",coarse[cond_i],".x-",coarse_range[cond_i],"&",
                              coarse[cond_i],".y","<=",coarse[cond_i],".x+",coarse_range[cond_i]))
  }
  filter_cond<-paste0("(",paste(filter_cond_vec,collapse = ")&("),")")
  
  #--apply exact matching conditions
  ctrl_match<-case_ref %>% unique %>%
    dplyr::rename("id"=id_col) %>% #for convenience
    inner_join(ctrl_pool,by=exact)
  
  #--apply relative/coarsening conditions
  if(length(coarse) > 0){
    ctrl_match %<>% filter(eval(parse(text=filter_cond)))
  }
  
  #checkpoint--if no additional matched samples can be found, break out
  if(nrow(ctrl_match)==0){
    stop("no matched sample can be found!")
  }
  
  #--rank multiple matched controls based on similarity metrics
  similarity_cond_vec<-c()
  if(weighting_scheme=="hiearchical"){
    wt<-rank(wt)
    for(cond_i in seq_along(coarse)){
      similarity_cond_vec<-c(similarity_cond_vec,
                             paste0("abs((",coarse[cond_i],".x","-",coarse[cond_i],".y",")*",10^(wt[cond_i]-1),")"))
    }
    similarity_formula<-paste0("rank(",paste(similarity_cond_vec,collapse = "+"),",ties.method=\"random\")")
    
  }else if(weighting_scheme=="weighted"){
    for(cond_i in seq_along(coarse)){
      similarity_cond_vec<-c(similarity_cond_vec,
                             paste0("abs((",coarse[cond_i],".x","-",coarse[cond_i],".y",")*",wt[cond_i],")"))
    }
    similarity_formula<-paste(similarity_cond_vec,collapse = "+")
    
  }else{
    stop("similarity measuring method between case and control is not currently supported!")
  }
  
  ctrl_match %<>%
    group_by(id) %>%
    mutate(similarity_rank=eval(parse(text=similarity_formula))) %>%
    ungroup 
  
  #sample without replacement
  max_rk<-max(ctrl_match$similarity_rank)
  case_n<-length(unique(ctrl_match$id))
  ctrl_match_undup<-c()
  
  match_rd<-1
  while(match_rd<=match_rt){
    #--identify a new batch of matched samples
    ctrl_match_undup_rd<-ctrl_match %>%
      filter(similarity_rank<=1)

    if(!replace){
      #--matched samples could be the same over different cases
      ctrl_match_undup_rd %<>%
        distinct(mrn,.keep_all=TRUE)
      
      ctrl_match %<>% 
        anti_join(ctrl_match_undup_rd,by=id_col) %>%
        group_by(id) %>%
        dplyr::mutate(similarity_rank=rank(similarity_rank,ties.method = "random")) %>% #move 2nd matched sample up
        ungroup
      
      #--go over the pool of matched samples until there is no duplicates
      matched_n<-length(unique(ctrl_match_undup_rd$id))
      while(matched_n<case_n&match_rd<=match_rt&match_rd<=max_rk) {
        case_unmatched<-ctrl_match %>%
          anti_join(ctrl_match_undup_rd,by="id") %>%
          filter(similarity_rank > 1) %>% #first matched sample already been picked
          group_by(id) %>%
          dplyr::mutate(similarity_rank=rank(similarity_rank)) %>% #move 2nd matched sample up
          ungroup
        
        #--when no more cases can be matched
        if(nrow(case_unmatched)==0) break

        ctrl_match_undup_rd %<>%
          bind_rows(case_unmatched %>%
                      filter(similarity_rank<=1))
        
        #--end-of-inner-loop updates
        matched_n<-length(unique(ctrl_match_undup_rd$id))
        ctrl_match %<>% 
          anti_join(ctrl_match_undup_rd,by=id_col) %>%
          group_by(id) %>%
          dplyr::mutate(similarity_rank=rank(similarity_rank,ties.method = "random")) %>% #move 2nd matched sample up
          ungroup
      }
    }

    #--end-of-outer-loop updates
    ctrl_match_undup %<>% 
      bind_rows(ctrl_match_undup_rd %>% mutate(similarity_rank=match_rd))
    
    if(verbose){
      cat("match round:",match_rd,"; ","matched samples:",matched_n,"\n")
    }
    
    match_rd<-match_rd+1
  }
  
  id_ctrl<-paste0(id_col,"_ctrl")
  ctrl_match_undup %<>%
    dplyr::rename(!!sym(id_ctrl) := id_col) %>%
    dplyr::rename(!!sym(id_col) := "id")
  
  
  return(ctrl_match_undup)
}

#one-hot coding is required!
#require (RANN, data.table)
strata_sample<-function(ref_dat, #reference dataset
                        match_dat, #matching dataset
                        keep_col="patient_num",
                        compare_metric=c("age","sex"), #matching criteria
                        boots=5,
                        nnk=boots+1, #number of candidate neighbors, recommend:nnk>=boots
                        searchtype=c("standard", "priority", "radius"),
                        replace=F,
                        verb=T){
  #attach row_id
  ref_dat$row_id<-1:nrow(ref_dat)
  match_dat$row_id<-(nrow(ref_dat)+1):(nrow(ref_dat)+nrow(match_dat))
  
  boots_samp<-c()
  for(k in 1:boots){
    start_k<-Sys.time()
    if(verb){
      cat("bootstrapped sample:",k,"\n")
    }
    
    start_kt<-Sys.time()
    #identify k-nearest-neighbour
    sample_pos<-ref_dat$row_id
    sample_neg<-nn2(match_dat[,compare_metric],
                    ref_dat[,compare_metric],
                    k=nnk,
                    searchtype=searchtype)$nn.idx[,sample(seq_len(nnk),1)] #inject randomness
    sample_neg<-match_dat[sample_neg,]$row_id
    
    #reconstruct stratified samples
    idx_lst<-c(sample_pos,sample_neg)
    idx_lst<-idx_lst[order(idx_lst)]
    
    if(verb){
      cat("...reconstruct stratified sample of size ",length(idx_lst),
          " in ",Sys.time()-start_kt,units(Sys.time()-start_kt),"\n")
    }
    
    start_kt<-Sys.time()
    idx_map<-as.data.frame(table(idx_lst))
    sample_reconst0<-as.data.table(rbind(ref_dat[,c(compare_metric,"row_id",keep_col)],
                                         match_dat[,c(compare_metric,"row_id",keep_col)]))[(row_id %in% idx_map$idx_lst)]
    sample_reconst<-sample_reconst0[rep(seq_len(nrow(sample_reconst0)),idx_map$Freq)]
    sample_reconst[,boots_rnd:=k]
    
    boots_samp<-rbind(boots_samp,sample_reconst)
    
    if(verb){
      cat(".....rebuild the balanced sample in ",
          Sys.time()-start_kt,units(Sys.time()-start_kt),"\n")
      
      cat("Finish bootstrap sample ",k," in ",
          Sys.time()-start_k,units(Sys.time()-start_k),"\n")
    }
    
    if(!replace){
      match_dat<-match_dat[!(match_dat$row_id %in% unique(sample_neg)),]
    }
  }
  return(boots_samp)
}

parse_med_ont_va<-function(path_vec){
  if (!is.null(dim(path_vec))||path_vec[1]=="")
    stop("input has to be a non-empty vector!")
  
  n<-length(path_vec)
  path_init<-data.frame(ROW_ID=seq(1,n,by=1),
                        PATH=path_vec,
                        stringsAsFactors = F)
  
  # initial string clean-up
  path_init %<>% 
    mutate(PATH = gsub("\\\\i2b2\\\\Medications","",PATH)) %>% # remove common pre-fix
    mutate(PATH = gsub(" / ","/",PATH)) # remove redundant spaces
  
  # calculate depths of overall PATH and VA segment
  path_init %<>%
    mutate(NLEVEL=stringr::str_count(PATH,"\\\\"),
           VA_NLEVEL=stringr::str_count(PATH,"\\\\\\["))
  
  #--concept level
  path_parse<-path_init %>% mutate(CONCEPT_LEV=6)
  
  #--SCDF or SCBF level
  path_add<-path_init %>%
    mutate(PATH = ifelse(NLEVEL==(VA_NLEVEL+1),PATH,
                         ifelse(VA_NLEVEL==0,
                                stringr::str_extract(PATH,paste0("^([^\\\\]*\\\\){",pmin(3,NLEVEL),"}")),
                                stringr::str_extract(PATH,paste0("^([^\\\\]*\\\\){",(VA_NLEVEL+2),"}")))))
  
  path_parse %<>% bind_rows(path_add %>% mutate(CONCEPT_LEV=5))
  
  #--extract generic/brand names from SCDF/SCBF concepts (usually first word)
  path_add %<>%
    mutate(scdf = gsub("[^\\\\]*\\\\","",gsub("\\\\$","",PATH)),
           generic = case_when(grepl("\\/",scdf) ~ stringr::str_extract(scdf,".*\\/+[^ ]* {1}"),
                               TRUE ~ gsub(" .*","",scdf))) %>%
    mutate(PATH = stringr::str_replace(PATH,scdf,generic)) %>%
    dplyr::select(ROW_ID,PATH,NLEVEL,VA_NLEVEL)
  
  path_parse %<>% bind_rows(path_add %>% mutate(CONCEPT_LEV=4))
  
  #--VA class levels
  va_max<-max(path_init$VA_NLEVEL)
  for(lev in 1:va_max){
    path_add<-path_init %>%
      mutate(PATH=ifelse(VA_NLEVEL==0, 
                         stringr::str_extract(PATH,paste0("^([^\\\\]*\\\\){2}")),
                         stringr::str_extract(PATH,paste0("^([^\\\\]*\\\\){",
                                                          pmin((VA_NLEVEL+1),(lev+1)),
                                                          "}"))))
    
    path_parse %<>% bind_rows(path_add %>% mutate(CONCEPT_LEV=lev))
  }
  
  path_parse %<>% 
    spread(CONCEPT_LEV,PATH,sep="") %>%
    arrange(ROW_ID)
  
  return(path_parse)
}


# multiclass y is not supported yet!
# data_type should be a vector of "cat" or "num"
univar_analysis_mixed<-function(id,grp,X,data_type,pretty=F){
  if(ncol(X)!=length(data_type)){
    stop("data types of X need to be specified")
  }
  
  #TODO: when there is only 1 category

    # anova
  df_num<-data.frame(cbind(id,grp,X[,(data_type=="num"),drop=F]),stringsAsFactors=F) %>%
    gather(var,val,-grp,-id) %>%
    mutate(grp=as.factor(grp)) %>%
    mutate(val=as.numeric(val))
  
  out_num<-df_num %>%
    group_by(var,grp) %>%
    dplyr::summarise(n=length(unique(id)),
                     val_miss=sum(is.na(val)),
                     val_mean=mean(val,na.rm=T),
                     val_sd=sd(val,na.rm=T),
                     val_med=median(val,na.rm=T),
                     val_q1=quantile(val,0.25,na.rm=T),
                     val_q3=quantile(val,0.75,na.rm=T),
                     val_min=min(val,na.rm=T),
                     val_max=max(val,na.rm=T)) %>% 
    ungroup %>%
    left_join(df_num %>%
                nest(-var) %>%
                mutate(fit=map(data, ~ aov(val~grp,data=.x)),
                       tidied=map(fit,tidy)) %>%
                unnest(tidied) %>% 
                filter(!is.na(p.value)) %>%
                select(var,p.value),
              by="var") %>%
    mutate(label=paste0(n,"; ",
                        # round(val_miss/n,2),"; ", #missing rate
                        round(val_mean,1),"(",round(val_sd,2),"); ",
                        val_med,"(",val_q1,",",val_q3,")"))
  
  
  # chi-sq
  df_cat<-data.frame(cbind(id,grp,X[,(data_type=="cat")]),stringsAsFactors=F) %>%
    gather(var,val,-grp,-id) %>%
    mutate(grp=as.factor(grp),val=as.factor(val))
  
  out_cat<-df_cat %>%
    group_by(grp) %>%
    dplyr::mutate(tot=length(unique(id))) %>%
    ungroup %>%
    group_by(var) %>%
    dplyr::mutate(val_miss=sum(is.na(val))) %>%
    ungroup %>% filter(!is.na(val)) %>%
    group_by(var,grp,tot,val_miss,val) %>%
    dplyr::summarise(n=length(unique(id))) %>%
    ungroup %>%
    mutate(prop=round(n/tot,4)) %>%
    left_join(df_cat %>%
                group_by(var) %>%
                dplyr::summarise(p.value=chisq.test(val,grp,simulate.p.value=T)$p.value) %>%
                ungroup,
              by="var") %>%
    mutate(label=paste0(n,"; ",
                        # round(val_miss/n,2),"; ", #missing rate
                        "(",prop*100,"%)"))
  
  #output
  if(pretty){
    out<-out_num %>% 
      select(n,grp) %>% unique %>%
      gather(var,val,-grp) %>% 
      mutate(val=as.character(val)) %>% 
      spread(grp,val) %>%
      bind_rows(out_num %>%
                  mutate(label2=paste0(round(val_mean,1)," (",round(val_sd,1),")"," [",round(val_miss/n,2),"]")) %>%
              dplyr::select(var,grp,p.value,label2) %>% spread(grp,label2)) %>%
      bind_rows(out_cat %>%
                  unite("var",c("var","val"),sep="=") %>%
                  mutate(label2=paste0(n," (",round(prop*100,1),"%)"," [",round(val_miss/n,2),"]")) %>%
                  dplyr::select(var,grp,p.value,label2) %>% spread(grp,label2)) %>%
      mutate(p.value=round(p.value,4)) %>%
      separate("var",c("var","cat"),sep="=",extra="merge",fill="right") %>%
      mutate(cat=case_when(var=="n" ~ "",
                           is.na(cat) ~ "mean(sd) [miss]",
                           TRUE ~ paste0(cat,",n(%) [miss]")))

  }else{
    out<-list(out_num=out_num,
              out_cat=out_cat)
  }

  return(out)
}


