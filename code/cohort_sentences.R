
get_domain_tbl<-function(cohort, domain, r1, r2, incident=FALSE, washout_months=18){
  print("Making domain table for:")
  print(domain)
  a=Sys.time()
  if (domain=="condition"){
    domain_tbl=cdm_tbl('condition_occurrence') %>% 
      inner_join(vocabulary_tbl('concept') %>% 
                   dplyr::select(concept_id, vocabulary_id) %>% 
                   filter(vocabulary_id=='ICD10CM'), 
                 by=c('condition_source_concept_id'='concept_id')) %>%
      rename(domain_date=condition_start_date, domain_concept_id=condition_source_concept_id) %>%
      mutate(domain_concept_id=as.character(domain_concept_id),
             domain_concept_name=condition_source_concept_name) %>%
      dplyr::select(person_id, site, domain_concept_id, domain_concept_name, domain_date)
  }else if (domain=='drug'){
    scdf_xwalk<-vocabulary_tbl('concept') %>% 
      filter(vocabulary_id=="RxNorm", concept_code!="No matching concept", domain_id=="Drug", concept_class_id=="Clinical Drug Form") %>%
      dplyr::select(concept_id) %>%
      inner_join(vocabulary_tbl('concept_ancestor'), by=c('concept_id'='ancestor_concept_id')) %>%
      rename(ancestor_concept_id=concept_id) %>% 
      distinct(ancestor_concept_id, descendant_concept_id) %>% 
      inner_join(vocabulary_tbl('concept') %>% dplyr::select(concept_id, ancestor_concept_name=concept_name), by=c('ancestor_concept_id'='concept_id')) %>%
      compute_new(indexes=list(c('ancestor_concept_id', 'descendant_concept_id')))
    
    domain_tbl=cdm_tbl('drug_exposure') %>%
      inner_join(scdf_xwalk, by=c('drug_concept_id'='descendant_concept_id')) %>%
      mutate(domain_date=drug_exposure_start_date, domain_concept_id=ancestor_concept_id,
             domain_concept_name=ancestor_concept_name) %>%
      mutate(domain_concept_id=as.character(domain_concept_id)) %>% 
      dplyr::select(person_id, site, domain_concept_id, domain_concept_name, domain_date)
  }else if (domain=='measurement_labs'){
    lab_groups<-load_codeset('lab_groupings_final')
    
    keep_value_as_concept_ids<-c(9189L, 9190L, 9191L, 9192L, 4126681L, 4181412L, 
                                 4124457L, 4115446L, 4124462L, 4115447L, 4128647L, 
                                 4116858L, 4125548L, 4309202L,
                                 3197320L, 4165520L, 4162849L, 
                                 4053347L, 4339292L, 763703L, 4128645L, 
                                 4211692L, 45912109L,  45935782L, 
                                 3221924L, 4132135L, 4146067L, 45919864L, 
                                 4267416L, 44814649L, 763964L, 45880913L, 4125559L)
    
    keep_value_as_concept_ids_b<-c(4126681L, 9191L, 4181412L)
    domain_tbl_temp=cdm_tbl('measurement_labs') %>%
      inner_join(lab_groups %>% dplyr::select(concept_id, group_id, group_name=group, lab_val_type), by=c('measurement_concept_id'='concept_id')) %>% 
      mutate(lab_val=case_when(
        lab_val_type=='num' & (is.na(range_low) | is.na(range_high) | is.na(value_as_number)) ~ 'na',
        lab_val_type=='cat' & is.na(value_as_concept_id)~'na',
        lab_val_type=='num' & value_as_number<=range_low~'low',
        lab_val_type=='num'& value_as_number>range_low & value_as_number<range_high ~ 'normal',
        lab_val_type=='num' & value_as_number>=range_high ~ 'high',
        lab_val_type=='cat' & value_as_concept_id %in% keep_value_as_concept_ids_b ~ value_as_concept_name,
        lab_val_type=='pres'~'no_result_available',
        TRUE~'error'
      )) %>%
      filter(lab_val!='na', lab_val!='error', lab_val!="no_result_available", lab_val!="normal") %>% 
      mutate(domain_concept_id=paste(group_id, lab_val, sep="_"),
             domain_concept_name=paste(group_name, lab_val, sep="_")) %>%
      rename(domain_date=measurement_date) %>%
      dplyr::select(person_id, site, domain_concept_id, domain_concept_name, domain_date)
    
    pcr<-cdm_tbl('observation_derivation_recover') %>%
      filter(observation_concept_id==2000001530L) %>%
      mutate(lab_val=case_when(
        value_as_concept_id %in% c(2000001526L, 9191L) ~'Positive',
        value_as_concept_id ==9189L~'Negative',
        TRUE~'Unknown'
      )) %>%
      filter(lab_val!="Unknown", lab_val!='Negative') %>% 
      mutate(group_id='PCR_test') %>%
      mutate(domain_concept_id=paste(group_id, lab_val, sep="_")) %>%
      rename(domain_date=observation_date)%>%
      mutate(domain_concept_name=domain_concept_id) %>% 
      dplyr::select(person_id, site, domain_concept_id, domain_concept_name, domain_date)
    
    
    antigen<-cdm_tbl('observation_derivation_recover') %>%
      filter(observation_concept_id==2000001529L) %>%
      mutate(lab_val=case_when(
        value_as_concept_id %in% c(2000001526L, 9191L) ~'Positive',
        value_as_concept_id ==9189L~'Negative',
        TRUE~'Unknown'
      )) %>%
      filter(lab_val!="Unknown", lab_val!='Negative') %>% 
      mutate(group_id='antigen_test') %>%
      mutate(domain_concept_id=paste(group_id, lab_val, sep="_")) %>%
      rename(domain_date=observation_date)%>%
      mutate(domain_concept_name=domain_concept_id) %>% 
      dplyr::select(person_id, site, domain_concept_id, domain_concept_name, domain_date)
    
    serology<-cdm_tbl('observation_derivation_recover') %>%
      filter(observation_concept_id==2000001528L) %>%
      mutate(lab_val=case_when(
        value_as_concept_id %in% c(2000001526L, 9191L) ~'Positive',
        value_as_concept_id ==9189L~'Negative',
        TRUE~'Unknown'
      )) %>%
      filter(lab_val!="Unknown", lab_val!='Negative') %>% 
      mutate(group_id='serology_test') %>%
      mutate(domain_concept_id=paste(group_id, lab_val, sep="_")) %>%
      rename(domain_date=observation_date)%>%
      mutate(domain_concept_name=domain_concept_id) %>% 
      dplyr::select(person_id, site, domain_concept_id, domain_concept_name, domain_date)
    
    domain_tbl=domain_tbl_temp %>%
      dplyr::union(pcr) %>%
      dplyr::union(antigen) %>%
      dplyr::union(serology)
    
    
    
  }else if (domain=='procedure'){
    procs<-load_codeset('procs')
    domain_tbl=cdm_tbl('procedure_occurrence') %>%
      inner_join(procs %>% dplyr::select(concept_id, concept_name), by=c('procedure_concept_id'='concept_id')) %>%
      rename(domain_concept_id=procedure_concept_id,
             domain_concept_name=concept_name,
             domain_date=procedure_date) %>%
      mutate(domain_concept_id=as.character(domain_concept_id)) %>% 
      dplyr::select(person_id, site, domain_concept_id, domain_concept_name, domain_date)
  }else if (domain=='vitals'){
    if (is.na(cohort)){
      print('error')
      return()
    }else{
      
    }
    domain_tbl_temp=cdm_tbl('measurement_vitals') %>%
      inner_join(cohort %>% dplyr::select(person_id), by='person_id') %>%
      compute_new(indexes=list(c('person_id')))
    
    domain_tbl=domain_tbl_temp %>%
      filter(measurement_concept_id==3020891L | measurement_concept_id==40762499L) %>% 
      mutate(vital_val=case_when(
        (is.na(range_low) | is.na(range_high) | is.na(value_as_number)) ~ 'na',
        value_as_number<=range_low~'low',
        value_as_number>range_low & value_as_number<range_high ~ 'normal',
        value_as_number>=range_high ~ 'high',
        TRUE~'error'
      )) %>%
      filter(vital_val!='na', vital_val!='error', vital_val!='normal') %>% 
      mutate(domain_concept_id=paste(measurement_concept_id, vital_val, sep="_")) %>%
      mutate(domain_concept_name=paste(measurement_concept_name, vital_val, sep="_")) %>%
      rename(domain_date=measurement_date) %>%
      dplyr::select(person_id, site, domain_concept_id, domain_concept_name, domain_date) 
  }else{
    return()
  }
  
  
  ## domain_tbl needs
  ## - person_id col
  ## - domain_date_col
  ## -domain_concept_id_col
  ## (can filter for required vocabulary ids above and remove inner join to concept below)
  
  if (incident==TRUE){
    print('washout')
    tbl_washout<-domain_tbl %>%
      inner_join(cohort, by=c('person_id', 'site')) %>% 
      filter(domain_date<observation_date-days(7), domain_date>observation_date-months(washout_months)) %>% 
      #inner_join(vocabulary_tbl("concept") %>% filter(concept_code!="No matching concept", vocabulary_id%in% vocabulary_ids) %>%
      #             dplyr::select(concept_id), by=setNames("concept_id", domain_concept_id_col)) %>%
      rename(concept_id=domain_concept_id,
             concept_name=domain_concept_name) %>% 
      distinct(person_id, site, concept_id, concept_name) %>% 
      compute_new(indexes=list(c('person_id', 'site', 'concept_id')))
    
    print('anti-joining')
    tbl_final<-domain_tbl %>% 
      inner_join(cohort, by=c('person_id', 'site')) %>% 
      filter(domain_date>observation_date+days(r1),domain_date<=observation_date+days(r2)) %>%
      rename(concept_id=domain_concept_id,
             concept_name=domain_concept_name) %>% 
      distinct(person_id, site, concept_id, concept_name) %>% 
      anti_join(tbl_washout, by=c("person_id", "concept_id")) %>% 
      compute_new(indexes=list(c('person_id', 'site', 'concept_id')))
  }else{
    print('no washout')
    tbl_final<-domain_tbl %>%
      inner_join(cohort, by=c('person_id', 'site')) %>% 
      filter(domain_date>observation_date+days(r1),domain_date<=observation_date+days(r2)) %>%
      rename(concept_id=domain_concept_id,
             concept_name=domain_concept_name) %>% 
      distinct(person_id, site, concept_id, concept_name) %>% 
      #      anti_join(tbl_washout, by=c("person_id", "concept_id")) %>% 
      compute_new(indexes=list(c('person_id', 'site', 'concept_id')))
  }
  print(Sys.time()-a)
  return(tbl_final)
}



get_cohort_sentences<-function(cohort, r1=-15, r2=15, incident=FALSE, washout_months=18, domains=c('condition', 'drug', 'measurement_labs', 'procedure', 'vitals'))
{   
  #  cond_codes<-get_domain_tbl(cohort, domain='condition', r1, r2, incident, washout_months)
  code_tbl<-setNames(data.frame(matrix(ncol=4, nrow=0)), c('person_id', 'site', 'concept_id', 'concept_name')) %>%
    mutate(person_id=as.integer(person_id), 
           site=as.character(site),
           concept_id=as.character(concept_id), 
           concept_name=as.character(concept_name)) %>% 
    output_tbl("code_tbl", temp=TRUE)
  for (domain in domains){
    domain_tbl<-get_domain_tbl(cohort, domain, r1, r2, incident, washout_months)
    code_tbl<-code_tbl %>%
      dplyr::union(domain_tbl) %>%
      compute_new(indexes=list(c('person_id', 'site', 'concept_id')))
  }
  
  domain_rename<-function(x){
    y=case_when(
      x=='condition'~'Condition',
      x=='measurement_labs'~'Measurement',
      x=='drug'~'Drug',
      x=='procedure'~'Procedure',
      x=='vitals'~'Measurement'
    )
  }
  
  #domains_clean=domain_rename(domains)
  
  #code_tbl<-code_tbl %>%
  #  inner_join(
  #    vocabulary_tbl('concept') %>% dplyr::select(concept_id, domain_id) %>% filter(domain_id %in% domains_clean),
  #    by='concept_id'
  #  ) %>%
  #  dplyr::select(-domain_id) %>%
  #  compute_new(index='person_id')
  
  n_codes=code_tbl %>% 
    group_by(person_id) %>%
    mutate(n=n()) %>%
    ungroup %>%
    filter(n>1) %>% summarize(n=n()) %>% pull(n)
  #    n_codes=n
  
  sentences<-code_tbl %>% 
    group_by(person_id) %>%
    mutate(n_codes=n()) %>%
    ungroup %>%
    filter(n_codes>1) %>%
    group_by(person_id) %>% 
    slice_sample(n=n_codes) %>%  
    summarize(codes=str_flatten(as.character(concept_id), collapse=" ")) %>% 
    compute_new(index=c("person_id"))
  
  sentences_names<-code_tbl
  
  return_list<-list(sentences, sentences_names)
  return(return_list)
}




impute_date<-function(cohort, low=28, high=90){
  date_window=low:high
  shifts<-data.frame(date_shift=date_window) %>%
    output_tbl("shifts", temp=TRUE)
  
  cohort %>%
    merge(shifts) %>%
    group_by(person_id) %>%
    slice_sample(n=1) %>%
    ungroup %>% 
    mutate(cohort_entry_date=cohort_entry_date-days(date_shift)) %>%
    output_tbl("cohort_imputed", temp=TRUE)
  
}

