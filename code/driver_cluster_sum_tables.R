# Vector of additional packages to load before executing the request
config_append('extra_packages', c("tidyverse"))

#' Execute the request
#'
#' This function presumes the environment has been set up, and executes the
#' steps of the request.
#'
#' In addition to performing queries and analyses, the execution path in this
#' function should include periodic progress messages to the user, and logging
#' of intermediate totals and timing data through [append_sum()].
#'
#' @return The return value is dependent on the content of the request, but is
#'   typically a structure pointing to some or all of the retrieved data or
#'   analysis results.  The value is not used by the framework itself.
#' @md
.run  <- function() {
  
  message('Starting execution with framework version ',
          config('framework_version'))
  
  # Set up the step log with as many attrition columns as you need.
  # For example, this call sets up the log with a `persons` count that will be
  # required at each step.
  init_sum(cohort = 'Start', persons = 0)
  
  # By convention, accumulate execution results in a list rather than as
  # independent variables, in order to make returning the entire set easier
  rslt <- list()
  
  
  TAG<-'_320'
  
  supercluster_df_train<-data.frame(
    c('Cardiorespiratory\n symptoms', 'Cardiorespiratory\n symptoms', 
      'Cardiorespiratory\n symptoms', 'Cardiorespiratory\n symptoms', 
      'Cardiorespiratory\n symptoms', 
      'Musculoskeletal\n pain',
      'Behavioral health', 'Fatigue', 'Headache',
      'Gastrointestinal\n symptoms', 'Behavioral health'),
    c('0', '2', '3', '4', '5', '1', '9', '6', '8', '7', '10'))
  colnames(supercluster_df_train)=c('supercluster', 'cluster')
  
  supercluster_df_train<-output_tbl(supercluster_df_train, paste('supercluster_df_train', TAG, sep=''))
  
  
  supercluster_df_test<-data.frame(
    c('Cardiorespiratory\n symptoms', 'Cardiorespiratory\n symptoms', 
      'Cardiorespiratory\n symptoms', 'Cardiorespiratory\n symptoms', 
      'Cardiorespiratory\n symptoms', 
      'Musculoskeletal\n pain','Musculoskeletal\n pain',
      'Behavioral health', 'Fatigue', 'Headache',
      'Gastrointestinal\n symptoms', 'Cardiorespiratory\n symptoms'),
    c('0', '1', '6', '7', '8', '2', '3', '10', '4', '11', '9', '5'))
  colnames(supercluster_df_test)=c('supercluster', 'cluster')
  
  supercluster_df_test<-output_tbl(supercluster_df_test, paste('supercluster_df_test', TAG, sep=''))
  
  
#  split_a_superclusters=data.frame(
#    cluster=c(4L, 10L, 11L, 
#              3L, 
#              5L, 
#              2L, 6L,
#              8L, 9L,
#              0L, 1L, 7L),
#    supercluster=c("respiratory", "respiratory", "respiratory", 
#                   "musculoskeletal", 
#                   'cardiopulmonary', 
#                   'neurologic', 'neurologic', 
#                   'GI', 'GI',
#                   'other', 'other', 'other'
#                   )
#  )
#  
###  split_b_superclusters=data.frame(
#    cluster=c(1L, 2L,
#              0L, 
#              3L, 
#              7L, 9L,
#              5L, 8L,
#              4L, 6L),
#    supercluster=c("respiratory", "respiratory", 
#                   "musculoskeletal", 
#                   'cardiopulmonary', 
#                   'neurologic', 'neurologic', 
#                   'GI', 'GI',
#                   'other', 'other'
#    )
#  )
 
  
  ####### Grid param search
  
  
  results_tbl("cp_embedded_df_aws_v3_train_try_60", results_tag="_pasc_78") %>%
    impute_clusters() %>% 
    dplyr::select(-index) %>% #rename(x='0', y='1') %>% 
    inner_join(results_tbl("cp_cohort_incident_noncomplex_v2"), by="person_id") %>%
    collect %>%
    mutate(cluster=as.factor(cluster))%>%
    output_tbl(paste("cp_opt_try_60_train", TAG, sep=''), index="person_id")
  
  
  results_tbl("cp_embedded_df_aws_v3_test_try_60", results_tag="_pasc_78") %>%
    impute_clusters() %>% 
    dplyr::select(-index) %>% #rename(x='0', y='1') %>% 
    inner_join(results_tbl("cp_cohort_incident_noncomplex_v2"), by="person_id") %>%
    collect %>%
    mutate(cluster=as.factor(cluster))%>%
    output_tbl(paste("cp_opt_try_60_test", TAG, sep=''), index="person_id")
  
  results_tbl("cp_embedded_df_26_opt_5", results_tag="_pasc_78") %>%
    impute_clusters() %>% 
    dplyr::select(-index) %>% #rename(x='0', y='1') %>% 
    inner_join(results_tbl("cp_cohort_incident_noncomplex_v2"), by="person_id") %>%
    collect %>%
    mutate(cluster=as.factor(cluster))%>%
    output_tbl(paste("cp_opt1", TAG, sep=''), index="person_id")
  
  results_tbl("cp_embedded_df_26_opt_75", results_tag="_pasc_78") %>%
    impute_clusters() %>% 
    dplyr::select(-index) %>% #rename(x='0', y='1') %>% 
    inner_join(results_tbl("cp_cohort_incident_noncomplex_v2"), by="person_id") %>%
    collect %>%
    mutate(cluster=as.factor(cluster))%>%
    output_tbl(paste("cp_opt2", TAG, sep=''), index="person_id")
  
  results_tbl("cp_embedded_df_26_opt_235", results_tag="_pasc_78") %>%
    impute_clusters() %>% 
    dplyr::select(-index) %>% #rename(x='0', y='1') %>% 
    inner_join(results_tbl("cp_cohort_incident_noncomplex_v2"), by="person_id") %>%
    collect %>%
    mutate(cluster=as.factor(cluster))%>%
    output_tbl(paste("cp_opt3", TAG, sep=''), index="person_id")

  
   results_tbl("cp_embedded_df_26_opt_355", results_tag="_pasc_78") %>%
    impute_clusters() %>% 
    dplyr::select(-index) %>% #rename(x='0', y='1') %>% 
    inner_join(results_tbl("cp_cohort_incident_noncomplex_v2"), by="person_id") %>%
    collect %>%
    mutate(cluster=as.factor(cluster))%>%
    output_tbl(paste("cp_opt4", TAG, sep=''), index="person_id")
  
   cohort_clustered_opt_try_60_train<-make_cohort_clustered(cohort=results_tbl(paste("cp_opt_try_60_train", TAG, sep=''), results_tag='_pasc_78') %>% collect %>% 
                                                  # rename(x="0", y="1") %>%
                                                  mutate(cluster=as.factor(cluster)),
                                                sentence_names=results_tbl("cp_cohort_sentence_names_fixed_aws_v2_train")) %>%
     output_tbl(paste('cohort_clustered_opt_try_60_train', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
   
   cohort_clustered_opt_try_60_test<-make_cohort_clustered(cohort=results_tbl(paste("cp_opt_try_60_test", TAG, sep=''), results_tag='_pasc_78') %>% collect %>% 
                                                              # rename(x="0", y="1") %>%
                                                              mutate(cluster=as.factor(cluster)),
                                                            sentence_names=results_tbl("cp_cohort_sentence_names_fixed_aws_v2_test")) %>%
     output_tbl(paste('cohort_clustered_opt_try_60_test', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
   
   
   
 
   cohort_clustered_opt1<-make_cohort_clustered(cohort=results_tbl(paste("cp_opt1", TAG, sep=''), results_tag='_pasc_78') %>% collect %>% 
                                              # rename(x="0", y="1") %>%
                                              mutate(cluster=as.factor(cluster)),
                                            sentence_names=results_tbl("cp_cohort_sentence_names_fixed_aws_v2_train")) %>%
    output_tbl(paste('cohort_clustered_opt1', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
  
   cohort_clustered_opt2<-make_cohort_clustered(cohort=results_tbl(paste("cp_opt2", TAG, sep=''), results_tag='_pasc_78') %>% collect %>% 
                                              # rename(x="0", y="1") %>%
                                              mutate(cluster=as.factor(cluster)),
                                            sentence_names=results_tbl("cp_cohort_sentence_names_fixed_aws_v2_train")) %>%
    output_tbl(paste('cohort_clustered_opt2', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
   cohort_clustered_opt3<-make_cohort_clustered(cohort=results_tbl(paste("cp_opt3", TAG, sep=''), results_tag='_pasc_78') %>% collect %>% 
                                              # rename(x="0", y="1") %>%
                                              mutate(cluster=as.factor(cluster)),
                                            sentence_names=results_tbl("cp_cohort_sentence_names_fixed_aws_v2_train")) %>%
    output_tbl(paste('cohort_clustered_opt3', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
    cohort_clustered_opt4<-make_cohort_clustered(cohort=results_tbl(paste("cp_opt4", TAG, sep=''), results_tag='_pasc_78') %>% collect %>% 
                                              # rename(x="0", y="1") %>%
                                              mutate(cluster=as.factor(cluster)),
                                            sentence_names=results_tbl("cp_cohort_sentence_names_fixed_aws_v2_train")) %>%
    output_tbl(paste('cohort_clustered_opt4', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
  
    cohort_dx_clusters_opt_try_60_train<-make_dx_clusters(cohort_clustered_opt_try_60_train) %>% collect %>% 
      output_tbl(paste('cohort_dx_clusters_opt_try_60_train', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
    
     cohort_dx_clusters_opt_try_60_test<-make_dx_clusters(cohort_clustered_opt_try_60_test) %>% collect %>% 
      output_tbl(paste('cohort_dx_clusters_opt_try_60_test', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
    
    
   cohort_dx_clusters_opt1<-make_dx_clusters(cohort_clustered_opt1) %>%
    output_tbl(paste('cohort_dx_clusters_opt1', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
    cohort_dx_clusters_opt2<-make_dx_clusters(cohort_clustered_opt2) %>%
    output_tbl(paste('cohort_dx_clusters_opt2', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
    cohort_dx_clusters_opt3<-make_dx_clusters(cohort_clustered_opt3) %>%
    output_tbl(paste('cohort_dx_clusters_opt3', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
   cohort_dx_clusters_opt4<-make_dx_clusters(cohort_clustered_opt4) %>%
    output_tbl(paste('cohort_dx_clusters_opt4', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
  
   
   cluster_sum_opt_try_60_train<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_opt_try_60_train %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
     output_tbl(paste('cluster_sum_opt_try_60_train', TAG, sep='')) 
   cld_df_opt_try_60_train<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_opt_try_60_train %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
   cld_long_opt_try_60_train<-cld_df_opt_try_60_train %>% pivot_longer(cols=setdiff(colnames(cld_df_opt_try_60_train), 'dx_group')) %>%
     rename(Subphenotype=name, cld_letter=value)
   heatmap_df_opt_try_60_train<-make_heatmap_df(cluster_sum=cluster_sum_opt_try_60_train, cld_df_long=cld_long_opt_try_60_train) %>%
     output_tbl(paste('heatmap_df_opt_try_60_train', TAG, sep=''))
   top_codes_opt_try_60_train<-get_top_codes(cohort_clustered_opt_try_60_train) %>%
     output_tbl(paste('top_codes_opt_try_60_train', TAG, sep=''))
   
    
   cluster_sum_opt_try_60_test<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_opt_try_60_test %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
     output_tbl(paste('cluster_sum_opt_try_60_test', TAG, sep='')) 
   cld_df_opt_try_60_test<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_opt_try_60_test %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
   cld_long_opt_try_60_test<-cld_df_opt_try_60_test %>% pivot_longer(cols=setdiff(colnames(cld_df_opt_try_60_test), 'dx_group')) %>%
     rename(Subphenotype=name, cld_letter=value)
   heatmap_df_opt_try_60_test<-make_heatmap_df(cluster_sum=cluster_sum_opt_try_60_test, cld_df_long=cld_long_opt_try_60_test) %>%
     output_tbl(paste('heatmap_df_opt_try_60_test', TAG, sep=''))
   top_codes_opt_try_60_test<-get_top_codes(cohort_clustered_opt_try_60_test) %>%
     output_tbl(paste('top_codes_opt_try_60_test', TAG, sep=''))
   
   
   
   
   
   
   cluster_sum_opt1<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_opt1 %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
    output_tbl(paste('cluster_sum_opt1', TAG, sep='')) 
    cld_df_opt1<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_opt1 %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
    cld_long_opt1<-cld_df_opt1 %>% pivot_longer(cols=setdiff(colnames(cld_df_opt1), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
    heatmap_df_opt1<-make_heatmap_df(cluster_sum=cluster_sum_opt1, cld_df_long=cld_long_opt1) %>%
      output_tbl(paste('heatmap_df_opt1', TAG, sep=''))
   top_codes_opt1<-get_top_codes(cohort_clustered_opt1) %>%
      output_tbl(paste('top_codes_opt1', TAG, sep=''))
 
      cluster_sum_opt2<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_opt2 %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
    output_tbl(paste('cluster_sum_opt2', TAG, sep='')) 
    cld_df_opt2<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_opt2 %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
    cld_long_opt2<-cld_df_opt2 %>% pivot_longer(cols=setdiff(colnames(cld_df_opt2), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
    heatmap_df_opt2<-make_heatmap_df(cluster_sum=cluster_sum_opt2, cld_df_long=cld_long_opt2) %>%
      output_tbl(paste('heatmap_df_opt2', TAG, sep=''))
   top_codes_opt2<-get_top_codes(cohort_clustered_opt2) %>%
      output_tbl(paste('top_codes_opt2', TAG, sep=''))

   
   cluster_sum_opt3<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_opt3 %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
    output_tbl(paste('cluster_sum_opt3', TAG, sep='')) 
    cld_df_opt3<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_opt3 %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
    cld_long_opt3<-cld_df_opt3 %>% pivot_longer(cols=setdiff(colnames(cld_df_opt3), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
    heatmap_df_opt3<-make_heatmap_df(cluster_sum=cluster_sum_opt3, cld_df_long=cld_long_opt3) %>%
      output_tbl(paste('heatmap_df_opt3', TAG, sep=''))
   top_codes_opt3<-get_top_codes(cohort_clustered_opt3) %>%
      output_tbl(paste('top_codes_opt3', TAG, sep=''))

   
   cluster_sum_opt4<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_opt4 %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
    output_tbl(paste('cluster_sum_opt4', TAG, sep='')) 
    cld_df_opt4<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_opt4 %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
    cld_long_opt4<-cld_df_opt4 %>% pivot_longer(cols=setdiff(colnames(cld_df_opt4), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
    heatmap_df_opt4<-make_heatmap_df(cluster_sum=cluster_sum_opt4, cld_df_long=cld_long_opt4) %>%
      output_tbl(paste('heatmap_df_opt4', TAG, sep=''))
   top_codes_opt4<-get_top_codes(cohort_clustered_opt4) %>%
      output_tbl(paste('top_codes_opt4', TAG, sep=''))

   
  # No evidence control cohort 
   
     
  results_tbl("cp_embedded_df_aws_v3_no_evidence", results_tag="_pasc_78") %>%
    impute_clusters() %>% 
    dplyr::select(-index) %>% #rename(x='0', y='1') %>% 
    inner_join(results_tbl("cohort_no_evidence_matched"), by="person_id") %>%
    collect %>%
    mutate(cluster=as.factor(cluster))%>%
    output_tbl(paste("cp_no_evidence", TAG, sep=''), index="person_id")
  
  
 
   cohort_clustered_no_evidence<-make_cohort_clustered(cohort=results_tbl(paste("cp_no_evidence", TAG, sep=''), results_tag='_pasc_78') %>% collect %>% 
                                              # rename(x="0", y="1") %>%
                                              mutate(cluster=as.factor(cluster)),
                                            sentence_names=results_tbl("cohort_no_evidence_sentence_names_aws_v2")) %>%
    output_tbl(paste('cohort_clustered_no_evidence', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
  
  require(multcomp)
   cohort_dx_clusters_no_evidence<-make_dx_clusters(cohort_clustered_no_evidence) %>%
     collect %>% 
    output_tbl(paste('cohort_dx_clusters_no_evidence', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
   
   
   cluster_sum_no_evidence<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_no_evidence %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
    output_tbl(paste('cluster_sum_no_evidence', TAG, sep='')) 
    cld_df_no_evidence<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_no_evidence %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
    cld_long_no_evidence<-cld_df_no_evidence %>% pivot_longer(cols=setdiff(colnames(cld_df_no_evidence), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
    heatmap_df_no_evidence<-make_heatmap_df(cluster_sum=cluster_sum_no_evidence, cld_df_long=cld_long_no_evidence) %>%
      output_tbl(paste('heatmap_df_no_evidence', TAG, sep=''))
   top_codes_no_evidence<-get_top_codes(cohort_clustered_no_evidence) %>%
      output_tbl(paste('top_codes_no_evidence', TAG, sep=''))
 
    cohort_dx_clusters_master_no_evidence<-make_dx_clusters(cohort_clustered_no_evidence, cluster_master_pasc=load_codeset('cluster_master') %>% rename(dx_group=cluster)) %>%
     collect %>% 
    output_tbl(paste('cohort_dx_clusters_master_no_evidence', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
   
    
    # no evidence imputed
    cohort_dx_clusters_no_evidence<-make_dx_clusters(results_tbl(paste('cohort_clustered_no_evidence', TAG, sep='')) %>%
                                                       mutate(cluster=as.character(imputed_cluster))) %>%
      collect %>% 
      output_tbl(paste('cohort_dx_imp_clusters_no_evidence', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
    
    
    cluster_sum_no_evidence<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_no_evidence %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
      output_tbl(paste('imp_cluster_sum_no_evidence', TAG, sep='')) 
    cld_df_no_evidence<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_no_evidence %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
    cld_long_no_evidence<-cld_df_no_evidence %>% pivot_longer(cols=setdiff(colnames(cld_df_no_evidence), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
    library(multcomp)
    detach("package:multcomp", unload = TRUE)
    detach("package:TH.data", unload = TRUE)
    detach("package:MASS", unload = TRUE)
    heatmap_df_no_evidence<-make_heatmap_df(cluster_sum=cluster_sum_no_evidence, cld_df_long=cld_long_no_evidence) %>%
      output_tbl(paste('imp_heatmap_df_no_evidence', TAG, sep=''))
    top_codes_no_evidence<-get_top_codes(results_tbl(paste('cohort_clustered_no_evidence', TAG, sep='')) %>%
                                           mutate(cluster=as.character(imputed_cluster))) %>%
      output_tbl(paste('imp_top_codes_no_evidence', TAG, sep=''))
    
    
   
   cluster_master_sum_no_evidence<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_master_no_evidence %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
    output_tbl(paste('cluster_master_sum_no_evidence', TAG, sep='')) 
    cld_df_no_evidence<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_master_no_evidence %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
    cld_long_no_evidence<-cld_df_no_evidence %>% pivot_longer(cols=setdiff(colnames(cld_df_no_evidence), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
    heatmap_df_no_evidence<-make_heatmap_df(cluster_sum=cluster_master_sum_no_evidence, cld_df_long=cld_long_no_evidence) %>%
      output_tbl(paste('heatmap_df_master_no_evidence', TAG, sep=''))   
    
    
    
    
     cohort_dx_clusters_master_no_evidence<-make_dx_clusters(
      cohort_clustered_no_evidence, 
      cluster_master_pasc=load_codeset('cluster_master') %>% rename(dx_group=cluster)) %>%
    output_tbl(paste('cohort_dx_clusters_master_v3_no_evidence', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
  
    cohort_dx_clusters_master_no_evidence_wide<-results_tbl(paste('cohort_dx_clusters_master_v3_no_evidence', TAG, sep='')) %>% 
      distinct(person_id, cluster, imputed_cluster, dx_group) %>% mutate(val=1) %>%  
      collect %>% 
      pivot_wider(names_from='dx_group', values_from='val', values_fill=0)%>%
      dplyr::select(-"NA") %>%
      output_tbl(paste('cohort_dx_clusters_master_v3_wide_no_evidence', TAG, sep=''), indexes=list(c('person_id')))
    
    
    
     
   cluster_master_sum_no_evidence_temp<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_master_no_evidence %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
    output_tbl(paste('cluster_master_sum_v3_no_evidence_temp', TAG, sep=''), temp=TRUE) 
   cluster_master_keep <-cluster_master_sum_no_evidence_temp %>% filter(prop>0.2) %>% distinct(dx_group) %>% pull(dx_group)
   cluster_master_keep_pasc<-results_tbl(paste('cluster_master_sum_v3_train_temp', TAG, sep='')) %>% filter(prop>0.2) %>% distinct(dx_group) %>% pull(dx_group)
   cluster_master_keep_total<-setdiff(union(cluster_master_keep, cluster_master_keep_pasc),c('administrative encounter', 'other specified status', 'personal/family history of disease', 'general signs and symptoms'))
  cluster_master_sum_no_evidence<-cluster_master_sum_no_evidence_temp %>%
    filter(dx_group %in% cluster_master_keep_total) %>%
    collect %>% 
    output_tbl(paste('cluster_master_sum_v3_no_evidence', TAG, sep=''), temp=TRUE) 
       cld_df_master_no_evidence<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_master_no_evidence %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
    cld_long_master_no_evidence<-cld_df_master_no_evidence %>% pivot_longer(cols=setdiff(colnames(cld_df_master_no_evidence), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
    heatmap_df_master_no_evidence<-make_heatmap_df(cluster_sum=cluster_master_sum_no_evidence, cld_df_long=cld_long_master_no_evidence) %>%
      output_tbl(paste('heatmap_df_master_v3_no_evidence', TAG, sep=''))
    
    
    
    ### Combined df
    
    combined_temp<-results_tbl("embedded_df_aws_v3_combined", results_tag="_pasc_78") %>% dplyr::select(person_id, x, y)
    no_evidence<-results_tbl(paste("cp_no_evidence", TAG, sep='')) %>% 
      dplyr::select(person_id, cluster, imputed_cluster) %>% mutate(cohort='no_evidence') %>%
      mutate(supercluster=as.character(imputed_cluster), imputed_cluster=as.character(imputed_cluster))
    pasc_train<-results_tbl(paste("cp_aws_v3_train_temp", TAG, sep='')) %>% 
      dplyr::select(person_id, cluster, imputed_cluster) %>% 
      mutate(imputed_cluster=as.character(imputed_cluster)) %>% 
      mutate(cohort='pasc_train') %>%
      inner_join(results_tbl(paste('supercluster_df_train', TAG, sep='')), by=c('imputed_cluster'='cluster'))
    pasc_test<-results_tbl(paste("cp_aws_v3_test_temp", TAG, sep='')) %>% 
      dplyr::select(person_id, cluster, imputed_cluster) %>% 
      mutate(imputed_cluster=as.character(imputed_cluster)) %>% 
      mutate(cohort='pasc_test') %>%
      inner_join(results_tbl(paste('supercluster_df_test', TAG, sep='')), by=c('imputed_cluster'='cluster'))
    both<-no_evidence %>% dplyr::union(pasc_train) %>% dplyr::union(pasc_test) %>%  
      compute_new(indexes=list(c('person_id')))
    
    combined<-combined_temp %>% inner_join(both , by='person_id') %>% 
      output_tbl(paste('df_aws_v3_combined', TAG, sep=''))
   


   
    
  ####
   
  #### New splits
   
   cohort_train_temp<-results_tbl("cp_embedded_df_aws_v3_train", results_tag="_pasc_78") %>%
    impute_clusters() %>% 
    dplyr::select(-index) %>% 
    inner_join(results_tbl("cp_cohort_incident_noncomplex_v2"), by="person_id") %>%
    collect %>%
    mutate(cluster=as.factor(cluster))%>%
    output_tbl(paste("cp_aws_v3_train_temp", TAG, sep=''), indexes=list(c("person_id")))
  
   ## get hosp
   
   cohort_hosp_train<-cohort_train_temp %>% rename(observation_date=cohort_entry_date) %>% 
     get_hosp(cohort_tbl=., days_start=28, days_end=179) %>%
     rename(cohort_entry_date=observation_date) %>% 
     output_tbl('cohort_hosp_train', temp=TRUE, indexes=list(c('person_id')))
   
   
   ## get icu
   
   cohort_icu_train<-cohort_hosp_train %>% rename(observation_date=cohort_entry_date) %>% 
     get_icu(cohort_tbl=., days_start=28, days_end=179) %>%
     rename(cohort_entry_date=observation_date) %>% 
     output_tbl('cohort_icu_train', temp=TRUE, indexes=list(c('person_id')))
   
   
   
   ## get severity
   
   
   cohort_visits<-results_tbl("cohort_icu_train") %>%
     rename(observation_date=cohort_entry_date) %>% 
     dplyr::select(person_id, observation_date) %>%
     mutate(test_result='positive') %>% 
     get_visits() %>%
     compute_new(indexes=list('person_id',
                              'visit_type',
                              'observation_date',
                              'visit_occurrence_id'))
   
   output_tbl(cohort_visits, "visit_tbl_train", indexes=list('person_id',
                                                     'visit_type',
                                                     'observation_date',
                                                     'visit_occurrence_id'))
   
   
   message('Computing Flag 1')
   cohort_flag_1 <- compute_flag1(visit_cohort = cohort_visits %>% filter(test_result=="positive")) %>% 
     output_tbl('flag_1_train',
                indexes=list('person_id',
                             'condition_concept_id',
                             'visit_occurrence_id'))
   #append_sum(cohort = 'Flag 1 Assignment', persons = distinct_ct(cohort_flag_1))
   
   message('Computing Flag 2')
   cohort_flag_2 <- compute_flag2(visit_cohort = cohort_visits %>% filter(test_result=="positive")) %>%
     output_tbl('flag_2_train',
                indexes=list('person_id',
                             'concept_id',
                             'visit_occurrence_id'))
   #append_sum(cohort = 'Flag 2 Assignment', persons = distinct_ct(cohort_flag_2))
   
   
   message('Computing Flag 3')
   cohort_flag_3 <- compute_flag3(visit_cohort = cohort_visits %>% filter(test_result=="positive")) %>%
     output_tbl('flag_3_train',
                indexes=list('person_id',
                             'concept_id',
                             'visit_occurrence_id'))
   
   
  
   message('Computing Visit Level Severity')
   cohort_visit_level_severity <- compute_visit_level_severity(
     visit_cohort = cohort_visits,
     flag_1_tbl=results_tbl('flag_1_train'),
     flag_2_tbl=results_tbl('flag_2_train'),
     flag_3_tbl=results_tbl('flag_3_train')
     ) %>%
     output_tbl('visit_level_severity_train',
                indexes=list('person_id'))
   
  
   message('Computing Person Level Severity')
   cohort_person_level_severity <- compute_person_level_severity(visit_severity_tbl = cohort_visit_level_severity) %>%
     output_tbl('person_level_severity_train',
                indexes=list('person_id'))
   
  results_tbl('cohort_icu_train') %>% left_join(results_tbl('person_level_severity_train'), by='person_id') %>%
    output_tbl('cohort_severity_train', indexes=list('person_id'))
   ## get util
   
   cohort_util<-results_tbl('cohort_severity_train') %>%
     get_util() %>%
      output_tbl(paste("cp_aws_v3_train", TAG, sep=''), indexes=list(c("person_id")))
   
   results_tbl(paste("cp_aws_v3_train", TAG, sep='')) %>%
     rename(hosp_post_acute_flag=hosp_flag, icu_post_acute_flag=icu_flag,
            observation_date=cohort_entry_date) %>%
     get_hosp(cohort_tbl=., days_start=0, days_end=29) %>%
     get_icu(cohort_tbl=., days_start=0, days_end=29) %>%
     rename(hosp_acute_flag=hosp_flag, icu_acute_flag=icu_flag,
            cohort_entry_date=observation_date) %>%
     compute_new(indexes=list(c('person_id')))%>%
     output_tbl(paste("cp_aws_v3_train", TAG, sep=''), indexes=list(c("person_id")))
   
   
   cohort_provider_trajectory_train<-results_tbl(paste("cp_aws_v3_train", TAG, sep='')) %>%
     get_provider_trajectory(days_start=0, days_end = 365) %>%
     output_tbl(paste('cohort_provider_trajectory_train', TAG, sep=''))
   
    cohort_visit_trajectory_train<-results_tbl(paste("cp_aws_v3_train", TAG, sep='')) %>%
     get_visit_trajectory(days_start=0, days_end = 365) %>%
     output_tbl(paste('cohort_visit_trajectory_train', TAG, sep=''))

    cohort_body_system_trajectory_train<-results_tbl(paste("cp_aws_v3_train", TAG, sep='')) %>%
     get_body_system_trajectory(days_start=0, days_end = 365) %>%
     output_tbl(paste('cohort_body_system_trajectory_train', TAG, sep=''))
    
    cohort_provider_trajectory_train<-results_tbl(paste("cp_aws_v3_train", TAG, sep='')) %>%
      mutate(cluster=as.character(imputed_cluster)) %>% 
      get_provider_trajectory(days_start=0, days_end = 365) %>%
      output_tbl(paste('cohort_provider_trajectory_train_imp', TAG, sep=''))
    
    cohort_visit_trajectory_train<-results_tbl(paste("cp_aws_v3_train", TAG, sep='')) %>%
      mutate(cluster=as.character(imputed_cluster)) %>% 
      get_visit_trajectory(days_start=0, days_end = 365) %>%
      output_tbl(paste('cohort_visit_trajectory_train_imp', TAG, sep=''))
    
    cohort_body_system_trajectory_train<-results_tbl(paste("cp_aws_v3_train", TAG, sep='')) %>%
      mutate(cluster=as.character(imputed_cluster)) %>% 
      get_body_system_trajectory(days_start=0, days_end = 365) %>%
      output_tbl(paste('cohort_body_system_trajectory_train_imp', TAG, sep=''))
    
   
   site_clust_sum_train<-results_tbl(paste('cp_aws_v3_train', TAG, sep='')) %>%
     inner_join(cdm_tbl('person') %>% dplyr::select('person_id', 'site'), by='person_id') %>%
     group_by(site) %>% mutate(n_total=n()) %>%
     ungroup %>%
     group_by(cluster, site, n_total) %>%
     summarize(n_clust=n()) %>%
     ungroup %>%
     collect %>%
     mutate(prop=n_clust/n_total) %>%
#     pivot_wider(id_cols=site, names_from=cluster, values_from=prop, values_fill=0) %>%
     output_tbl(paste('site_clust_sum_train', TAG, sep=''))
   
  
   cohort_clustered_train<-make_cohort_clustered(cohort=results_tbl(paste("cp_aws_v3_train", TAG, sep=''), results_tag='_pasc_78') %>% collect %>% 
                                              # rename(x="0", y="1") %>%
                                              mutate(cluster=as.factor(cluster)),
                                            sentence_names=results_tbl("cp_cohort_sentence_names_fixed_aws_v2_train")) %>%
    output_tbl(paste('cohort_clustered_v3_train', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
  
   
   cohort_dx_clusters_train<-make_dx_clusters(cohort_clustered_train) %>%
    output_tbl(paste('cohort_dx_clusters_v3_train', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
   
   cohort_dx_clusters_wide<-results_tbl(paste('cohort_dx_clusters_v3_train', TAG, sep='')) %>% 
     distinct(person_id, cluster, imputed_cluster, dx_group) %>% mutate(val=1) %>%  
     collect %>% 
     pivot_wider(names_from='dx_group', values_from='val', values_fill=0)%>%
     dplyr::select(-"NA") %>%
     inner_join(supercluster_df_train %>% 
                  rbind(c('None', -1L)), by=c('cluster')) %>% 
     output_tbl(paste('cohort_dx_clusters_v3_wide_train', TAG, sep=''), indexes=list(c('person_id')))
   
   
   
   cluster_sum_train<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_train %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
    output_tbl(paste('cluster_sum_v3_train', TAG, sep='')) 
    cld_df_train<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_train %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
    cld_long_train<-cld_df_train %>% pivot_longer(cols=setdiff(colnames(cld_df_train), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
    heatmap_df_train<-make_heatmap_df(cluster_sum=cluster_sum_train, cld_df_long=cld_long_train) %>%
      output_tbl(paste('heatmap_df_v3_train', TAG, sep=''))
    
    
 
      
    
   top_codes_train<-get_top_codes(cohort_clustered_train) %>%
      output_tbl(paste('top_codes_v3_train', TAG, sep=''))
   
   top_codes_expanded_train<-get_top_codes(results_tbl(paste('cohort_clustered_v3_train', TAG, sep='')), n=50) %>%
     output_tbl(paste('top_codes_expanded_v3_train', TAG, sep=''))
   
   results_tbl(paste('top_codes_expanded_v3_train', TAG, sep='')) %>%
     filter(domain_id=='Condition') %>%
     group_by(cluster) %>%
     slice_max(prop, n=20, with_ties=FALSE) %>%
     dplyr::select(cluster, concept_name, prop) %>%
     collect %>%
     write.csv('./specs/top_20_codes_train.csv', row.names=FALSE)
   
   top_codes_ids_keep<-results_tbl(paste('top_codes_expanded_v3_train', TAG, sep='')) %>%
     group_by(cluster, domain_id) %>%
     slice_max(prop, n=30, with_ties=FALSE) %>%
     ungroup %>% 
     distinct(concept_id) %>%
     inner_join(vocabulary_tbl('concept'), by='concept_id') %>%
     distinct(concept_id, concept_name) %>% 
     mutate(name_id=paste(concept_id, concept_name, sep=": ")) %>% 
     compute_new(indexes=list(c('concept_id')))
  
  cohort_clusters_top_v3_wide_train<-results_tbl(paste('cohort_clustered_v3_train', TAG, sep='')) %>%
    dplyr::select(-concept_name) %>% 
    left_join(top_codes_ids_keep, by='concept_id') %>% 
     distinct(person_id, cluster, imputed_cluster, name_id) %>% mutate(val=1) %>%  
     collect %>% 
     pivot_wider(names_from='name_id', values_from='val', values_fill=0)%>%
     dplyr::select(-"NA") %>%
     inner_join(supercluster_df_train %>% 
                  rbind(c('None', -1L)), by=c('cluster')) %>% 
     output_tbl(paste('cohort_clusters_top_v3_wide_train_temp4', TAG, sep=''), indexes=list(c('person_id')))
  
  
     cohort_dx_clusters_train<-make_dx_clusters(results_tbl(paste('cohort_clustered_v3_train', TAG, sep='')) %>% 
                                                  mutate(cluster=as.character(imputed_cluster))) %>%
    output_tbl(paste('cohort_dx_imp_clusters_v3_train', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
   cluster_sum_train<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_train, cluster_by='cluster') %>%
    output_tbl(paste('imp_cluster_sum_v3_train', TAG, sep='')) 
    cld_df_train<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_train %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
    cld_long_train<-cld_df_train %>% pivot_longer(cols=setdiff(colnames(cld_df_train), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
      library(multcomp)
  detach("package:multcomp", unload = TRUE)
  detach("package:TH.data", unload = TRUE)
  detach("package:MASS", unload = TRUE)
    heatmap_df_train<-make_heatmap_df(cluster_sum=cluster_sum_train %>% mutate(supercluster=cluster), cld_df_long=cld_long_train) %>%
      output_tbl(paste('imp_heatmap_df_v3_train', TAG, sep=''))
    
   top_codes_train<-get_top_codes(results_tbl(paste('cohort_clustered_v3_train', TAG, sep='')) %>% 
                                    mutate(cluster=as.character(imputed_cluster))) %>%
      output_tbl(paste('imp_top_codes_v3_train', TAG, sep=''))
 
   
   
  
  
 cohort_dx_superclusters_train<- results_tbl(paste('cohort_dx_clusters_v3_train', TAG, sep='')) %>%
   mutate(imputed_cluster=as.character(imputed_cluster))%>%
   inner_join(supercluster_df_train, by=c('imputed_cluster'='cluster')) %>%
   output_tbl(paste('cohort_dx_superclusters_v3_train', TAG, sep=''))
 
 supercluster_sum_train<-make_cluster_sum(cohort_dx_clusters=cohort_dx_superclusters_train, cluster_by='supercluster') %>%
   output_tbl(paste('supercluster_sum_v3_train', TAG, sep='')) 
  
  
  supercluster_cld_df_train<-get_cld_df(cohort_dx_clusters=cohort_dx_superclusters_train) %>% rename(dx_group=cluster)
  supercluster_cld_long_train<-supercluster_cld_df_train %>% pivot_longer(cols=setdiff(colnames(supercluster_cld_df_train), 'dx_group')) %>%
    rename(Subphenotype=name, cld_letter=value)
  supercluster_heatmap_df_train<-make_heatmap_df(cluster_sum=supercluster_sum_train, cld_df_long=supercluster_cld_long_train) %>%
    output_tbl(paste('supercluster_heatmap_df_v3_train', TAG, sep=''))
  cohort_superclustered_train <- results_tbl(paste('cohort_clustered_v3_train', TAG, sep='')) %>%
    mutate(imputed_cluster=as.character(imputed_cluster)) %>% 
    inner_join(results_tbl(paste('supercluster_df_train', TAG, sep='')), by=c('imputed_cluster'='cluster')) %>% 
    collect %>% 
    output_tbl('cohort_superclustered_train', temp=TRUE, indexes=list(c('person_id')))
  library(multcomp)
  detach("package:multcomp", unload = TRUE)
  detach("package:TH.data", unload = TRUE)
  detach("package:MASS", unload = TRUE)
  supercluster_top_codes_train<-cohort_superclustered_train %>%
    get_top_codes(cluster_by='supercluster') %>%
      output_tbl(paste('supercluster_top_codes_v3_train', TAG, sep=''))
   
    ### Continue from here
   
    cohort_dx_clusters_master_train<-make_dx_clusters(
      cohort_clustered_train, 
      cluster_master_pasc=load_codeset('cluster_master') %>% rename(dx_group=cluster)) %>%
    output_tbl(paste('cohort_dx_clusters_master_v3_train', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
  
    cohort_dx_clusters_master_wide<-results_tbl(paste('cohort_dx_clusters_master_v3_train', TAG, sep='')) %>% 
      distinct(person_id, cluster, imputed_cluster, dx_group) %>% mutate(val=1) %>%  
      collect %>% 
      pivot_wider(names_from='dx_group', values_from='val', values_fill=0)%>%
      dplyr::select(-"NA") %>%
      inner_join(supercluster_df_train %>% 
                   rbind(c('None', -1L)), by=c('cluster')) %>% 
      output_tbl(paste('cohort_dx_clusters_master_v3_wide_train', TAG, sep=''), indexes=list(c('person_id')))
    
    
    
     
   cluster_master_sum_train_temp<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_master_train %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
    output_tbl(paste('cluster_master_sum_v3_train_temp', TAG, sep=''), temp=TRUE) 
#   cluster_master_keep <-cluster_master_sum_train_temp %>% filter(prop>0.2) %>% distinct(dx_group) %>% pull(dx_group)
   cluster_master_keep<-cluster_master_keep_total
  cluster_master_sum_train<-cluster_master_sum_train_temp %>%
    filter(dx_group %in% cluster_master_keep) %>%
    collect %>% 
    output_tbl(paste('cluster_master_sum_v3_train', TAG, sep=''), temp=TRUE) 
       cld_df_master_train<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_master_train %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
    cld_long_master_train<-cld_df_master_train %>% pivot_longer(cols=setdiff(colnames(cld_df_master_train), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
    heatmap_df_master_train<-make_heatmap_df(cluster_sum=cluster_master_sum_train, cld_df_long=cld_long_master_train) %>%
      output_tbl(paste('heatmap_df_master_v3_train', TAG, sep=''))
    
  
   overlap_df_train<-results_tbl('cp_embedded_df_aws_v3_train') %>%
     dplyr::select(-index, -x, -y) %>%
     make_overlap_df() %>%
    output_tbl(paste('rel_int_long_train', TAG, sep=''))
   
   
   
   
   
   
   ### supplementary sensitivity analysis
   
   site_clust_sum_train_sup<-results_tbl(paste('cp_aws_v3_train', TAG, sep='')) %>%
     #     inner_join(cdm_tbl('person') %>% dplyr::select('person_id', 'site'), by='person_id') %>%
     group_by(site) %>% mutate(n_total=n()) %>%
     ungroup %>%
     mutate(cluster=imputed_cluster) %>% 
     group_by(cluster, site, n_total) %>%
     summarize(n_clust=n()) %>%
     ungroup %>%
     collect %>%
     mutate(prop=n_clust/n_total) %>%
     #     pivot_wider(id_cols=site, names_from=cluster, values_from=prop, values_fill=0) %>%
     output_tbl(paste('site_clust_sum_train_sup', TAG, sep=''))
   
   cohort_clustered_train_sup<-make_cohort_clustered(cohort=results_tbl(paste("cp_aws_v3_train", TAG, sep=''), results_tag='_pasc_78') %>% collect %>% 
                                                       # rename(x="0", y="1") %>%
                                                       mutate(cluster=as.factor(imputed_cluster)),
                                                     sentence_names=results_tbl("cp_cohort_sentence_names_fixed_aws_v2_train")) %>%
     output_tbl(paste('cohort_clustered_v3_train_sup', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
   
   cohort_dx_clusters_train_sup<-make_dx_clusters(cohort_clustered_train_sup) %>%
     output_tbl(paste('cohort_dx_clusters_v3_train_sup', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
   
   supercluster_df_train_sup<-data.frame(
     c('Cardiorespiratory\n symptoms', 'Cardiorespiratory\n symptoms', 
       'Cardiorespiratory\n symptoms', 'Cardiorespiratory\n symptoms', 
       'Pain and\n muscle weakness', 'Pain and\n muscle weakness', 
       'Mood disorders\n and symptoms', 'Headache', 
       'Gastrointestinal\n symptoms', 'Other'),
     c('0', '3', '6', '7', '1', '2', '8', '5', '4', '9'))
   colnames(supercluster_df_train_sup)=c('supercluster', 'cluster')
   
   cohort_dx_clusters_wide_sup<-results_tbl(paste('cohort_dx_clusters_v3_train_sup', TAG, sep='')) %>% 
     distinct(person_id, cluster, imputed_cluster, dx_group) %>% mutate(val=1) %>%  
     collect %>% 
     pivot_wider(names_from='dx_group', values_from='val', values_fill=0)%>%
     dplyr::select(-"NA") %>%
     inner_join(supercluster_df_train %>% 
                  rbind(c('None', -1L)), by=c('cluster')) %>% 
     output_tbl(paste('cohort_dx_clusters_v3_wide_train_sup', TAG, sep=''), indexes=list(c('person_id')))
   
   
   
   cluster_sum_train_sup<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_train_sup %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
     output_tbl(paste('cluster_sum_v3_train_sup', TAG, sep='')) 
   cld_df_train_sup<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_train_sup %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
   cld_long_train_sup<-cld_df_train_sup %>% pivot_longer(cols=setdiff(colnames(cld_df_train_sup), 'dx_group')) %>%
     rename(Subphenotype=name, cld_letter=value)
   heatmap_df_train_sup<-make_heatmap_df(cluster_sum=cluster_sum_train_sup, cld_df_long=cld_long_train_sup) %>%
     output_tbl(paste('heatmap_df_v3_train_sup', TAG, sep=''))
   top_codes_train_sup<-get_top_codes(cohort_clustered_train_sup) %>%
     output_tbl(paste('top_codes_v3_train_sup', TAG, sep=''))
   
   top_codes_expanded_train_sup<-get_top_codes(results_tbl(paste('cohort_clustered_v3_train_sup', TAG, sep='')), n=50) %>%
     output_tbl(paste('top_codes_expanded_v3_train_sup', TAG, sep=''))
   
   results_tbl(paste('top_codes_expanded_v3_train_sup', TAG, sep='')) %>%
     filter(domain_id=='Condition') %>%
     group_by(cluster) %>%
     slice_max(prop, n=20, with_ties=FALSE) %>%
     dplyr::select(cluster, concept_name, prop) %>%
     collect %>%
     write.csv('./specs/top_20_codes_train_sup.csv', row.names=FALSE)
   
   top_codes_ids_keep_sup<-results_tbl(paste('top_codes_expanded_v3_train_sup', TAG, sep='')) %>%
     group_by(cluster, domain_id) %>%
     slice_max(prop, n=30, with_ties=FALSE) %>%
     ungroup %>% 
     distinct(concept_id) %>%
     inner_join(vocabulary_tbl('concept'), by='concept_id') %>%
     distinct(concept_id, concept_name) %>% 
     mutate(name_id=paste(concept_id, concept_name, sep=": ")) %>% 
     compute_new(indexes=list(c('concept_id')))
   
   cohort_clusters_top_v3_wide_train_sup<-results_tbl(paste('cohort_clustered_v3_train_sup', TAG, sep='')) %>%
     dplyr::select(-concept_name) %>% 
     left_join(top_codes_ids_keep, by='concept_id') %>% 
     distinct(person_id, cluster, imputed_cluster, name_id) %>% mutate(val=1) %>%  
     collect %>% 
     pivot_wider(names_from='name_id', values_from='val', values_fill=0)%>%
     dplyr::select(-"NA") %>%
     inner_join(supercluster_df_train_sup %>% 
                  rbind(c('None', -1L)), by=c('cluster')) %>% 
     output_tbl(paste('cohort_clusters_top_v3_wide_train_temp_sup', TAG, sep=''), indexes=list(c('person_id')))
   
   
   
   
   
   
   cohort_dx_clusters_master_train_sup<-make_dx_clusters(
     cohort_clustered_train_sup, 
     cluster_master_pasc=load_codeset('cluster_master') %>% rename(dx_group=cluster)) %>%
     output_tbl(paste('cohort_dx_clusters_master_v3_train_sup', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
   
   cohort_dx_clusters_master_wide_sup<-results_tbl(paste('cohort_dx_clusters_master_v3_train_sup', TAG, sep='')) %>% 
     distinct(person_id, cluster, imputed_cluster, dx_group) %>% mutate(val=1) %>%  
     collect %>% 
     pivot_wider(names_from='dx_group', values_from='val', values_fill=0)%>%
     dplyr::select(-"NA") %>%
     inner_join(supercluster_df_train_sup %>% 
                  rbind(c('None', -1L)), by=c('cluster')) %>% 
     output_tbl(paste('cohort_dx_clusters_master_v3_wide_train_sup', TAG, sep=''), indexes=list(c('person_id')))
   
   
   
   
   cluster_master_sum_train_temp_sup<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_master_train_sup %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
     output_tbl(paste('cluster_master_sum_v3_train_temp_sup', TAG, sep=''), temp=TRUE) 
   cluster_master_keep_sup <-cluster_master_sum_train_temp_sup %>% filter(prop>0.2) %>% distinct(dx_group) %>% pull(dx_group)
   cluster_master_sum_train_sup<-cluster_master_sum_train_temp_sup %>%
     filter(dx_group %in% cluster_master_keep_sup) %>%
     output_tbl(paste('cluster_master_sum_v3_train_sup', TAG, sep=''), temp=TRUE) 
   cld_df_master_train_sup<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_master_train_sup %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
   cld_long_master_train_sup<-cld_df_master_train_sup %>% pivot_longer(cols=setdiff(colnames(cld_df_master_train_sup), 'dx_group')) %>%
     rename(Subphenotype=name, cld_letter=value)
   heatmap_df_master_train_sup<-make_heatmap_df(cluster_sum=cluster_master_sum_train_sup, cld_df_long=cld_long_master_train_sup) %>%
     output_tbl(paste('heatmap_df_master_v3_train_sup', TAG, sep=''))
   
   
   overlap_df_train_sup<-results_tbl('cp_embedded_df_aws_v3_train') %>%
     dplyr::select(-index, -x, -y) %>%
     make_overlap_df() %>%
     output_tbl(paste('rel_int_long_train_sup', TAG, sep=''))
   
   
   
   
   
   
   
   
   
   
   #   library(igraph)
#   g<-graph_from_adjacency_matrix(as.matrix(relative_intersection_matrix), weighted=TRUE,                                       mode='plus', diag=FALSE)
#   E(g)$weight[E(g)$weight<0.23]=0.001
#   E(g)$width <- 5*E(g)$weight+min(E(g)$weight)+1 # offset=1
#   plot(g)
#   
#       
 
     cohort_test_temp<-results_tbl("cp_embedded_df_aws_v3_test", results_tag="_pasc_78") %>%
    impute_clusters() %>% 
    dplyr::select(-index) %>% 
    inner_join(results_tbl("cp_cohort_incident_noncomplex_v2"), by="person_id") %>%
    collect %>%
    mutate(cluster=as.factor(cluster))%>%
    output_tbl(paste("cp_aws_v3_test_temp", TAG, sep=''), indexes=list(c("person_id")))
  
   ## get hosp
   
   cohort_hosp_test<-cohort_test_temp %>% rename(observation_date=cohort_entry_date) %>% 
     get_hosp(cohort_tbl=., days_start=28, days_end=179) %>%
     rename(cohort_entry_date=observation_date) %>% 
     output_tbl('cohort_hosp_test', temp=TRUE, indexes=list(c('person_id')))
   
   
   ## get icu
   
   cohort_icu_test<-cohort_hosp_test %>% rename(observation_date=cohort_entry_date) %>% 
     get_icu(cohort_tbl=., days_start=28, days_end=179) %>%
     rename(cohort_entry_date=observation_date) %>% 
     output_tbl('cohort_icu_test', temp=TRUE, indexes=list(c('person_id')))
   
   
   
   ## get severity
   
   
   cohort_visits<-results_tbl("cohort_icu_test") %>%
     rename(observation_date=cohort_entry_date) %>% 
     dplyr::select(person_id, observation_date) %>%
     mutate(test_result='positive') %>% 
     get_visits() %>%
     compute_new(indexes=list('person_id',
                              'visit_type',
                              'observation_date',
                              'visit_occurrence_id'))
   
   output_tbl(cohort_visits, "visit_tbl_test", indexes=list('person_id',
                                                     'visit_type',
                                                     'observation_date',
                                                     'visit_occurrence_id'))
   
   
   message('Computing Flag 1')
   cohort_flag_1 <- compute_flag1(visit_cohort = cohort_visits %>% filter(test_result=="positive")) %>% 
     output_tbl('flag_1_test',
                indexes=list('person_id',
                             'condition_concept_id',
                             'visit_occurrence_id'))
   #append_sum(cohort = 'Flag 1 Assignment', persons = distinct_ct(cohort_flag_1))
   
   message('Computing Flag 2')
   cohort_flag_2 <- compute_flag2(visit_cohort = cohort_visits %>% filter(test_result=="positive")) %>%
     output_tbl('flag_2_test',
                indexes=list('person_id',
                             'concept_id',
                             'visit_occurrence_id'))
   #append_sum(cohort = 'Flag 2 Assignment', persons = distinct_ct(cohort_flag_2))
   
   
   message('Computing Flag 3')
   cohort_flag_3 <- compute_flag3(visit_cohort = cohort_visits %>% filter(test_result=="positive")) %>%
     output_tbl('flag_3_test',
                indexes=list('person_id',
                             'concept_id',
                             'visit_occurrence_id'))
   
   
  
   message('Computing Visit Level Severity')
   cohort_visit_level_severity <- compute_visit_level_severity(
     visit_cohort = cohort_visits,
     flag_1_tbl=results_tbl('flag_1_test'),
     flag_2_tbl=results_tbl('flag_2_test'),
     flag_3_tbl=results_tbl('flag_3_test')
     ) %>%
     output_tbl('visit_level_severity_test',
                indexes=list('person_id'))
   
  
   message('Computing Person Level Severity')
   cohort_person_level_severity <- compute_person_level_severity(visit_severity_tbl = cohort_visit_level_severity) %>%
     output_tbl('person_level_severity_test',
                indexes=list('person_id'))
   
  results_tbl('cohort_icu_test') %>% left_join(results_tbl('person_level_severity_test'), by='person_id') %>%
    output_tbl('cohort_severity_test', indexes=list('person_id'))
   ## get util
   
   cohort_util<-results_tbl('cohort_severity_test') %>%
     get_util() %>%
      output_tbl(paste("cp_aws_v3_test", TAG, sep=''), indexes=list(c("person_id")))
   
    results_tbl(paste("cp_aws_v3_test", TAG, sep='')) %>%
     rename(hosp_post_acute_flag=hosp_flag, icu_post_acute_flag=icu_flag,
            observation_date=cohort_entry_date) %>%
     get_hosp(cohort_tbl=., days_start=0, days_end=29) %>%
     get_icu(cohort_tbl=., days_start=0, days_end=29) %>%
     rename(hosp_acute_flag=hosp_flag, icu_acute_flag=icu_flag,
            cohort_entry_date=observation_date) %>%
     compute_new(indexes=list(c('person_id')))%>%
     output_tbl(paste("cp_aws_v3_test", TAG, sep=''), indexes=list(c("person_id")))
   
   cohort_test<-results_tbl(paste("cp_aws_v3_test", TAG, sep='')) %>%
     mutate(imputed_cluster=as.character(imputed_cluster)) %>% 
     inner_join(results_tbl(paste('supercluster_df_test', TAG, sep='')), by=c('imputed_cluster'='cluster')) %>%
     compute_new(indexes=list(c('person_id')))
    
      cohort_provider_trajectory_test_supercluster<-cohort_test %>%
     get_provider_trajectory(days_start=0, days_end = 365) %>%
     output_tbl(paste('cohort_provider_trajectory_test_supercluster', TAG, sep=''))
   
    cohort_visit_trajectory_test_supercluster<-cohort_test %>%
     get_visit_trajectory(days_start=0, days_end = 365) %>%
     output_tbl(paste('cohort_visit_trajectory_test_supercluster', TAG, sep=''))

    cohort_body_system_trajectory_test_supercluster<-cohort_test %>%
     get_body_system_trajectory(days_start=0, days_end = 365) %>%
     output_tbl(paste('cohort_body_system_trajectory_test_supercluster', TAG, sep=''))
 
   
   
   cohort_provider_trajectory_test<-results_tbl(paste("cp_aws_v3_test", TAG, sep='')) %>%
     get_provider_trajectory(days_start=0, days_end = 365) %>%
     output_tbl(paste('cohort_provider_trajectory_test', TAG, sep=''))
   
    cohort_visit_trajectory_test<-results_tbl(paste("cp_aws_v3_test", TAG, sep='')) %>%
     get_visit_trajectory(days_start=0, days_end = 365) %>%
     output_tbl(paste('cohort_visit_trajectory_test', TAG, sep=''))

    cohort_body_system_trajectory_test<-results_tbl(paste("cp_aws_v3_test", TAG, sep='')) %>%
     get_body_system_trajectory(days_start=0, days_end = 365) %>%
     output_tbl(paste('cohort_body_system_trajectory_test', TAG, sep=''))
    
 
  
   
  site_clust_sum_test<-results_tbl(paste('cp_aws_v3_test', TAG, sep='')) %>%
     inner_join(cdm_tbl('person') %>% dplyr::select('person_id', 'site'), by='person_id') %>%
     group_by(site) %>% mutate(n_total=n()) %>%
     ungroup %>%
     group_by(cluster, site, n_total) %>%
     summarize(n_clust=n()) %>%
     ungroup %>%
     collect %>%
     mutate(prop=n_clust/n_total) %>%
#     pivot_wider(id_cols=site, names_from=cluster, values_from=prop, values_fill=0) %>%
     output_tbl(paste('site_clust_sum_test', TAG, sep=''))
  
   cohort_clustered_test<-make_cohort_clustered(cohort=results_tbl(paste("cp_aws_v3_test", TAG, sep=''), results_tag='_pasc_78') %>% collect %>% 
                                              # rename(x="0", y="1") %>%
                                              mutate(cluster=as.factor(cluster)),
                                            sentence_names=results_tbl("cp_cohort_sentence_names_fixed_aws_v2_test")) %>%
    output_tbl(paste('cohort_clustered_v3_test', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
  
   
   cohort_dx_clusters_test<-make_dx_clusters(cohort_clustered_test) %>%
    output_tbl(paste('cohort_dx_clusters_v3_test', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
   
   cohort_dx_clusters_wide<-results_tbl(paste('cohort_dx_clusters_v3_test', TAG, sep='')) %>% 
     distinct(person_id, cluster, dx_group) %>% mutate(val=1) %>%  
     collect %>% 
     pivot_wider(names_from='dx_group', values_from='val', values_fill=0)%>%
     dplyr::select(-"NA") %>%
     output_tbl(paste('cohort_dx_clusters_v3_wide_test', TAG, sep=''), indexes=list(c('person_id')))
   
   
   
   cluster_sum_test<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_test %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
    output_tbl(paste('cluster_sum_v3_test', TAG, sep='')) 
    cld_df_test<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_test %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
    cld_long_test<-cld_df_test %>% pivot_longer(cols=setdiff(colnames(cld_df_test), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
    heatmap_df_test<-make_heatmap_df(cluster_sum=cluster_sum_test, cld_df_long=cld_long_test) %>%
      output_tbl(paste('heatmap_df_v3_test', TAG, sep=''))
   top_codes_test<-get_top_codes(cohort_clustered_test) %>%
      output_tbl(paste('top_codes_v3_test', TAG, sep=''))  

   
   
   
   
   
        cohort_dx_clusters_master_test<-make_dx_clusters(
      cohort_clustered_test, 
      cluster_master_pasc=load_codeset('cluster_master') %>% rename(dx_group=cluster)) %>%
    output_tbl(paste('cohort_dx_clusters_master_v3_test', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
   
   cluster_master_sum_test_temp<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_master_test %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
    output_tbl(paste('cluster_master_sum_v3_test_temp', TAG, sep=''), temp=TRUE) 
   cluster_master_keep <-cluster_master_sum_test_temp %>% filter(prop>0.2) %>% distinct(dx_group) %>% pull(dx_group)
  cluster_master_sum_test<-cluster_master_sum_test_temp %>%
    filter(dx_group %in% cluster_master_keep) %>%
    output_tbl(paste('cluster_master_sum_v3_test', TAG, sep=''), temp=TRUE) 
       cld_df_master_test<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_master_test %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
    cld_long_master_test<-cld_df_master_test %>% pivot_longer(cols=setdiff(colnames(cld_df_master_test), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
    heatmap_df_master_test<-make_heatmap_df(cluster_sum=cluster_master_sum_test, cld_df_long=cld_long_master_test) %>%
      output_tbl(paste('heatmap_df_master_v3_test', TAG, sep=''))
    
    overlap_df_test<-results_tbl('cp_embedded_df_aws_v3_test') %>%
     dplyr::select(-index, -x, -y) %>%
     make_overlap_df() %>%
    output_tbl(paste('rel_int_long_test', TAG, sep=''))
    
    cohort_dx_clusters_test<-make_dx_clusters(results_tbl(paste('cohort_clustered_v3_test', TAG, sep='')) %>% 
                                                 mutate(cluster=as.character(imputed_cluster))) %>%
      output_tbl(paste('cohort_dx_imp_clusters_v3_test', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
    cluster_sum_test<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_test, cluster_by='cluster') %>%
      output_tbl(paste('imp_cluster_sum_v3_test', TAG, sep='')) 
    cld_df_test<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_test %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
    cld_long_test<-cld_df_test %>% pivot_longer(cols=setdiff(colnames(cld_df_test), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
    library(multcomp)
    detach("package:multcomp", unload = TRUE)
    detach("package:TH.data", unload = TRUE)
    detach("package:MASS", unload = TRUE)
    heatmap_df_test<-make_heatmap_df(cluster_sum=cluster_sum_test %>% mutate(supercluster=cluster), cld_df_long=cld_long_test) %>%
      output_tbl(paste('imp_heatmap_df_v3_test', TAG, sep=''))
    
    top_codes_test<-get_top_codes(results_tbl(paste('cohort_clustered_v3_test', TAG, sep='')) %>% 
                                     mutate(cluster=as.character(imputed_cluster))) %>%
      output_tbl(paste('imp_top_codes_v3_test', TAG, sep=''))
    
    

    cohort_dx_superclusters_test<- results_tbl(paste('cohort_dx_clusters_v3_test', TAG, sep='')) %>%
      mutate(imputed_cluster=as.character(imputed_cluster))%>%
      inner_join(supercluster_df_test, by=c('imputed_cluster'='cluster')) %>%
      output_tbl(paste('cohort_dx_superclusters_v3_test', TAG, sep=''))
    
    supercluster_sum_test<-make_cluster_sum(cohort_dx_clusters=cohort_dx_superclusters_test, cluster_by='supercluster') %>%
      output_tbl(paste('supercluster_sum_v3_test', TAG, sep='')) 
    
    
    supercluster_cld_df_test<-get_cld_df(cohort_dx_clusters=cohort_dx_superclusters_test) %>% rename(dx_group=cluster)
    supercluster_cld_long_test<-supercluster_cld_df_test %>% pivot_longer(cols=setdiff(colnames(supercluster_cld_df_test), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
    supercluster_heatmap_df_test<-make_heatmap_df(cluster_sum=supercluster_sum_test, cld_df_long=supercluster_cld_long_test) %>%
      output_tbl(paste('supercluster_heatmap_df_v3_test', TAG, sep=''))
    cohort_superclustered_test <- results_tbl(paste('cohort_clustered_v3_test', TAG, sep='')) %>%
      mutate(imputed_cluster=as.character(imputed_cluster)) %>% 
      inner_join(results_tbl(paste('supercluster_df_test', TAG, sep='')), by=c('imputed_cluster'='cluster')) %>%
      collect %>% 
      output_tbl('cohort_superclustered_test', temp=TRUE, indexes=list(c('person_id')))
    library(multcomp)
    detach("package:multcomp", unload = TRUE)
    detach("package:TH.data", unload = TRUE)
    detach("package:MASS", unload = TRUE)
    supercluster_top_codes_test<-cohort_superclustered_test %>%
      get_top_codes(cluster_by='supercluster') %>%
      output_tbl(paste('supercluster_top_codes_v3_test', TAG, sep='')) 
    ### Continue from here
    
    
    
    
    ### supplementary sensitivity analysis
    
    site_clust_sum_test_sup<-results_tbl(paste('cp_aws_v3_test', TAG, sep='')) %>%
      #     inner_join(cdm_tbl('person') %>% dplyr::select('person_id', 'site'), by='person_id') %>%
      group_by(site) %>% mutate(n_total=n()) %>%
      ungroup %>%
      mutate(cluster=imputed_cluster) %>% 
      group_by(cluster, site, n_total) %>%
      summarize(n_clust=n()) %>%
      ungroup %>%
      collect %>%
      mutate(prop=n_clust/n_total) %>%
      #     pivot_wider(id_cols=site, names_from=cluster, values_from=prop, values_fill=0) %>%
      output_tbl(paste('site_clust_sum_test_sup', TAG, sep=''))
    
    cohort_clustered_test_sup<-make_cohort_clustered(cohort=results_tbl(paste("cp_aws_v3_test", TAG, sep=''), results_tag='_pasc_78') %>% collect %>% 
                                                        # rename(x="0", y="1") %>%
                                                        mutate(cluster=as.factor(imputed_cluster)),
                                                      sentence_names=results_tbl("cp_cohort_sentence_names_fixed_aws_v2_test")) %>%
      output_tbl(paste('cohort_clustered_v3_test_sup', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
    
    cohort_dx_clusters_test_sup<-make_dx_clusters(cohort_clustered_test_sup) %>%
      output_tbl(paste('cohort_dx_clusters_v3_test_sup', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
    
#    supercluster_df_test_sup<-data.frame(
#      c('Cardiorespiratory\n symptoms', 'Cardiorespiratory\n symptoms', 
#        'Cardiorespiratory\n symptoms', 'Cardiorespiratory\n symptoms', 
#        'Pain and\n muscle weakness', 'Pain and\n muscle weakness', 
#        'Mood disorders\n and symptoms', 'Headache', 
#        'Gastrointestinal\n symptoms', 'Other'),
#      c('0', '3', '6', '7', '1', '2', '8', '5', '4', '9'))
#    colnames(supercluster_df_train_sup)=c('supercluster', 'cluster')
    
    cohort_dx_clusters_wide_test_sup<-results_tbl(paste('cohort_dx_clusters_v3_test_sup', TAG, sep='')) %>% 
      distinct(person_id, cluster, imputed_cluster, dx_group) %>% mutate(val=1) %>%  
      collect %>% 
      pivot_wider(names_from='dx_group', values_from='val', values_fill=0)%>%
      dplyr::select(-"NA") %>%
#      inner_join(supercluster_df_test_sup %>% 
#                   rbind(c('None', -1L)), by=c('cluster')) %>% 
      output_tbl(paste('cohort_dx_clusters_v3_wide_test_sup', TAG, sep=''), indexes=list(c('person_id')))
    
    
    
    cluster_sum_test_sup<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_test_sup %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
      output_tbl(paste('cluster_sum_v3_test_sup', TAG, sep='')) 
    cld_df_test_sup<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_test_sup %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
    cld_long_test_sup<-cld_df_test_sup %>% pivot_longer(cols=setdiff(colnames(cld_df_test_sup), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
    heatmap_df_test_sup<-make_heatmap_df(cluster_sum=cluster_sum_test_sup, cld_df_long=cld_long_test_sup) %>%
      output_tbl(paste('heatmap_df_v3_test_sup', TAG, sep=''))
    top_codes_test_sup<-get_top_codes(cohort_clustered_test_sup) %>%
      output_tbl(paste('top_codes_v3_test_sup', TAG, sep=''))
    
    top_codes_expanded_test_sup<-get_top_codes(results_tbl(paste('cohort_clustered_v3_test_sup', TAG, sep='')), n=50) %>%
      output_tbl(paste('top_codes_expanded_v3_test_sup', TAG, sep=''))
    
    results_tbl(paste('top_codes_expanded_v3_test_sup', TAG, sep='')) %>%
      filter(domain_id=='Condition') %>%
      group_by(cluster) %>%
      slice_max(prop, n=20, with_ties=FALSE) %>%
      dplyr::select(cluster, concept_name, prop) %>%
      collect %>%
      write.csv('./specs/top_20_codes_test_sup.csv', row.names=FALSE)
    
    top_codes_ids_keep_sup<-results_tbl(paste('top_codes_expanded_v3_test_sup', TAG, sep='')) %>%
      group_by(cluster, domain_id) %>%
      slice_max(prop, n=30, with_ties=FALSE) %>%
      ungroup %>% 
      distinct(concept_id) %>%
      inner_join(vocabulary_tbl('concept'), by='concept_id') %>%
      distinct(concept_id, concept_name) %>% 
      mutate(name_id=paste(concept_id, concept_name, sep=": ")) %>% 
      compute_new(indexes=list(c('concept_id')))
    
    cohort_clusters_top_v3_wide_test_sup<-results_tbl(paste('cohort_clustered_v3_test_sup', TAG, sep='')) %>%
      dplyr::select(-concept_name) %>% 
      left_join(top_codes_ids_keep, by='concept_id') %>% 
      distinct(person_id, cluster, imputed_cluster, name_id) %>% mutate(val=1) %>%  
      collect %>% 
      pivot_wider(names_from='name_id', values_from='val', values_fill=0)%>%
      dplyr::select(-"NA") %>%
#      inner_join(supercluster_df_test_sup %>% 
#                   rbind(c('None', -1L)), by=c('cluster')) %>% 
      output_tbl(paste('cohort_clusters_top_v3_wide_test_temp_sup', TAG, sep=''), indexes=list(c('person_id')))
    
    
    
    
    
    
    cohort_dx_clusters_master_test_sup<-make_dx_clusters(
      cohort_clustered_test_sup, 
      cluster_master_pasc=load_codeset('cluster_master') %>% rename(dx_group=cluster)) %>%
      output_tbl(paste('cohort_dx_clusters_master_v3_test_sup', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
    
    cohort_dx_clusters_master_wide_test_sup<-results_tbl(paste('cohort_dx_clusters_master_v3_test_sup', TAG, sep='')) %>% 
      distinct(person_id, cluster, imputed_cluster, dx_group) %>% mutate(val=1) %>%  
      collect %>% 
      pivot_wider(names_from='dx_group', values_from='val', values_fill=0)%>%
      dplyr::select(-"NA") %>%
#      inner_join(supercluster_df_test_sup %>% 
#                   rbind(c('None', -1L)), by=c('cluster')) %>% 
      output_tbl(paste('cohort_dx_clusters_master_v3_wide_test_sup', TAG, sep=''), indexes=list(c('person_id')))
    
    
    
    
    cluster_master_sum_test_temp_sup<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_master_test_sup %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
      output_tbl(paste('cluster_master_sum_v3_test_temp_sup', TAG, sep=''), temp=TRUE) 
    cluster_master_keep_sup <-cluster_master_sum_test_temp_sup %>% filter(prop>0.2) %>% distinct(dx_group) %>% pull(dx_group)
    cluster_master_sum_test_sup<-cluster_master_sum_test_temp_sup %>%
      filter(dx_group %in% cluster_master_keep_sup) %>%
      output_tbl(paste('cluster_master_sum_v3_test_sup', TAG, sep=''), temp=TRUE) 
    cld_df_master_test_sup<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_master_test_sup %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
    cld_long_master_test_sup<-cld_df_master_test_sup %>% pivot_longer(cols=setdiff(colnames(cld_df_master_test_sup), 'dx_group')) %>%
      rename(Subphenotype=name, cld_letter=value)
    heatmap_df_master_test_sup<-make_heatmap_df(cluster_sum=cluster_master_sum_test_sup, cld_df_long=cld_long_master_test_sup) %>%
      output_tbl(paste('heatmap_df_master_v3_test_sup', TAG, sep=''))
    
    
    overlap_df_test_sup<-results_tbl('cp_embedded_df_aws_v3_test') %>%
      dplyr::select(-index, -x, -y) %>%
      make_overlap_df() %>%
      output_tbl(paste('rel_int_long_test_sup', TAG, sep=''))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
   
    
######  
  
#   
#  
#  cp_embedded_df<-results_tbl("cp_embedded_df_aws_many_a", results_tag="_pasc_78") %>%
#    impute_clusters() %>%
#    #add_superclusters() %>% 
#    dplyr::select(-index) %>% rename(x='0', y='1') %>% 
#    inner_join(results_tbl("cp_cohort_incident_noncomplex"), by="person_id") %>%
#    collect %>%
#    mutate(cluster=as.factor(cluster)) %>%
#    left_join(split_a_superclusters, by=c('imputed_cluster'='cluster'))
#  
#  
#  
#  output_tbl(cp_embedded_df, paste("cp_v1_a", TAG, sep=''), index="person_id")
#  
#  cp_embedded_df<-results_tbl("cp_embedded_df_aws_many_b", results_tag="_pasc_78") %>%
#    impute_clusters() %>%
#    dplyr::select(-index) %>% rename(x='0', y='1') %>% 
#    inner_join(results_tbl("cp_cohort_incident_noncomplex"), by="person_id") %>%
#    collect %>%
#    mutate(cluster=as.factor(cluster)) %>%
#    left_join(split_b_superclusters, by=c('imputed_cluster'='cluster'))
#  
#  
#  
#  output_tbl(cp_embedded_df, paste("cp_v1_b", TAG, sep=''), index="person_id")
#  
#  
#  
#  cohort_clustered_a<-make_cohort_clustered(cohort=results_tbl(paste("cp_v1_a", TAG, sep=''), results_tag='_pasc_78') %>% collect %>% 
#                                              # rename(x="0", y="1") %>%
#                                              mutate(cluster=as.factor(imputed_cluster)),
#                                            sentence_names=results_tbl("cp_cohort_incident_noncomplex_sentence_names_aws_a")) %>%
#    output_tbl(paste('cohort_clustered_a', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
#  
#  cohort_clustered_b<-make_cohort_clustered(cohort=results_tbl(paste("cp_v1_b", TAG, sep=''), results_tag='_pasc_78') %>% collect %>% 
#                                              # rename(x="0", y="1") %>%
#                                              mutate(cluster=as.factor(imputed_cluster)),
#                                            sentence_names=results_tbl("cp_cohort_incident_noncomplex_sentence_names_aws_b")) %>%
#    output_tbl(paste('cohort_clustered_b', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
#  
#  
#  cohort_dx_clusters_a<-make_dx_clusters(cohort_clustered_a) %>%
#    output_tbl(paste('cohort_dx_clusters_a', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
#  
#  cohort_dx_clusters_b<-make_dx_clusters(cohort_clustered_b) %>%
#    output_tbl(paste('cohort_dx_clusters_b', TAG, sep=''), indexes=list(c('person_id', 'concept_id')))
#  
#  cluster_sum_a<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_a, cluster_by='supercluster') %>%
#    output_tbl(paste('cluster_sum_a', TAG, sep=''))
#  cluster_sum_b<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_b, cluster_by='supercluster') %>%
#    output_tbl(paste('cluster_sum_b', TAG, sep=''))
#  
#
#  
#  cld_df_a<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_a) %>% rename(dx_group=cluster)
#  cld_long_a<-cld_df_a %>% pivot_longer(cols=setdiff(colnames(cld_df_a), 'dx_group')) %>%
#    rename(Subphenotype=name, cld_letter=value)
#  
#  heatmap_df_a<-make_heatmap_df(cluster_sum=cluster_sum_a, cld_df_long=cld_long_a) %>%
#    output_tbl(paste('heatmap_df_a', TAG, sep=''))
#  
#  cld_df_b<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_b) %>% rename(dx_group=cluster)
#  cld_long_b<-cld_df_b %>% pivot_longer(cols=setdiff(colnames(cld_df_b), 'dx_group')) %>%
#    rename(Subphenotype=name, cld_letter=value)
#  
#  heatmap_df_b<-make_heatmap_df(cluster_sum=cluster_sum_b, cld_df_long=cld_long_b) %>%
#    output_tbl(paste('heatmap_df_b', TAG, sep=''))
#
#  
#  top_codes_a<-get_top_codes(cohort_clustered_a) %>%
#    output_tbl(paste('top_codes_a', TAG, sep=''))
#  
#  top_codes_b<-get_top_codes(cohort_clustered_b) %>%
#    output_tbl(paste('top_codes_b', TAG, sep=''))
# 
#   top_codes_a_supercluster<-get_top_codes(cohort_clustered_a, cluster_by='supercluster') %>%
#    output_tbl(paste('top_codes_a_supercluster', TAG, sep=''))
#  
#  top_codes_b_supercluster<-get_top_codes(cohort_clustered_b, cluster_by='supercluster') %>%
#    output_tbl(paste('top_codes_b_supercluster', TAG, sep='')) 
# 
#  
#  
#  
# ## Positives 
#   TAG_NEW='_1113'
#   results_tbl("cp_embedded_df_positives_aws_v2_a", results_tag="_pasc_78") %>%
#    impute_clusters() %>% 
#    dplyr::select(-index) %>% rename(x='0', y='1') %>% 
#    inner_join(results_tbl("cp_cohort_positives_v2"), by="person_id") %>%
#    collect %>%
#    mutate(cluster=as.factor(cluster))%>%
#    output_tbl(paste("cp_positives_aws_v2_a", TAG_NEW, sep=''), index="person_id")
#   
#   
#   site_clust_sum_a<-results_tbl(paste('cp_positives_aws_v2_a', TAG_NEW, sep='')) %>%
#     inner_join(cdm_tbl('person') %>% dplyr::select('person_id', 'site'), by='person_id') %>%
#     group_by(site) %>% mutate(n_total=n()) %>%
#     ungroup %>%
#     group_by(cluster, site, n_total) %>%
#     summarize(n_clust=n()) %>%
#     ungroup %>%
#     collect %>%
#     mutate(prop=n_clust/n_total) %>%
##     pivot_wider(id_cols=site, names_from=cluster, values_from=prop, values_fill=0) %>%
#     output_tbl(paste('site_clust_sum_positives_a', TAG_NEW, sep=''))
#  
#  
#   cohort_clustered_a<-make_cohort_clustered(cohort=results_tbl(paste("cp_positives_aws_v2_a", TAG_NEW, sep=''), results_tag='_pasc_78') %>% collect %>% 
#                                              # rename(x="0", y="1") %>%
#                                              mutate(cluster=as.factor(cluster)),
#                                            sentence_names=results_tbl("cp_cohort_positives_sentence_names_aws_v2_a")) %>%
#    output_tbl(paste('cohort_clustered_positives_v2_a', TAG_NEW, sep=''), indexes=list(c('person_id', 'concept_id')))
#  
#   
#   cohort_dx_clusters_a<-make_dx_clusters(cohort_clustered_a) %>%
#    output_tbl(paste('cohort_dx_clusters_positives_v2_a', TAG_NEW, sep=''), indexes=list(c('person_id', 'concept_id')))
#   
#   cluster_sum_a<-make_cluster_sum(cohort_dx_clusters=cohort_dx_clusters_a %>% rename(supercluster=cluster), cluster_by='supercluster') %>%
#    output_tbl(paste('cluster_sum_positives_v2_a', TAG_NEW, sep='')) 
#    cld_df_a<-get_cld_df(cohort_dx_clusters=cohort_dx_clusters_a %>% filter(cluster!="-1") %>% rename(supercluster=cluster)) %>% rename(dx_group=cluster)
#    cld_long_a<-cld_df_a %>% pivot_longer(cols=setdiff(colnames(cld_df_a), 'dx_group')) %>%
#      rename(Subphenotype=name, cld_letter=value)
#    heatmap_df_a<-make_heatmap_df(cluster_sum=cluster_sum_a %>% filter(cluster!="-1"), cld_df_long=cld_long_a) %>%
#      output_tbl(paste('heatmap_df_positives_v2_a', TAG_NEW, sep=''))
#   top_codes_a<-get_top_codes(cohort_clustered_a) %>%
#      output_tbl(paste('top_codes_positives_v2_a', TAG_NEW, sep=''))
#
#  # top codes overall in each cohort (split A)
#   cohort_positives=results_tbl(paste('cohort_clustered_positives_v2_a',TAG_NEW, sep=''))
#   cohort_pasc=results_tbl(paste('cohort_clustered_v2_a', TAG, sep=''))
#   
#   ## non-pasc positives
#   top_codes_overall_positives<-cohort_positives %>%
#     mutate(temp='temp') %>%
#     get_top_codes(cluster_by='temp') %>%
#     output_tbl(paste('top_codes_v2_overall_positives_a', TAG_NEW, sep=''))
#   
#   ## PASC
#    top_codes_overall_pasc<-cohort_pasc %>%
#     mutate(temp='temp') %>%
#     get_top_codes(cluster_by='temp') %>%
#     output_tbl(paste('top_codes_v2_overall_pasc_a', TAG_NEW, sep=''))
   
   
   
   
   
    
  message('Done.')
  
  invisible(rslt)
  
}

  
  
  
  
  
  
  
  
  
  
  