
impute_clusters<-function(cohort, method='centroid'){
  centroids<-cohort %>%
#    dplyr::select(-index) %>% 
    mutate(cluster=as.integer(cluster)) %>%
    filter(cluster!=-1) %>% 
#    rename(x='0', y='1') %>%
    group_by(cluster) %>%
    summarize(mean_x=mean(x), mean_y=mean(y)) %>%
    collect %>%
    output_tbl('centroids', temp=TRUE)
  
    
  cohort_unclustered<-cohort %>%
#    rename(x='0', y='1') %>% 
    filter(cluster==-1) %>% 
    dplyr::select(-cluster) %>% 
    merge(centroids) %>%
    rename(imputed_cluster=cluster) %>% 
    mutate(dist_x=x-mean_x, dist_y=y-mean_y) %>%
    mutate(dist=sqrt(dist_x^2+dist_y^2)) %>%
    group_by(person_id) %>%
    slice_min(dist, with_ties=FALSE) %>%
    distinct(person_id, imputed_cluster) %>% 
    mutate(imputed_flag=1L) %>% 
    output_tbl('centroids_temp', temp=TRUE, indexes=list(c('person_id')))
  
  cohort %>%
    left_join(cohort_unclustered, by='person_id') %>%
    mutate(imputed_cluster=case_when(
      is.na(imputed_cluster)~cluster,
      TRUE~imputed_cluster
    )) %>%
    mutate(imputed_flag=case_when(
      is.na(imputed_flag)~0L,
      TRUE~imputed_flag
    )) %>%
    collect %>% 
    output_tbl('cohort_centroids_temp', temp=TRUE, indexes=list(c('person_id'))) %>%
    return()
  
}


make_cohort_clustered<-function(cohort, sentence_names){
  cohort_clustered<-cohort %>% 
#    dplyr::select(person_id, cluster) %>% 
#$    rename(cohort=cluster) %>% 
    inner_join(sentence_names %>% collect, by="person_id")  %>%
    mutate(concept_id=as.integer(concept_id)) %>% 
    return()
}


make_dx_clusters<-function(cohort_clustered, cluster_master_pasc=load_codeset('cluster_master_pasc_mod') %>% rename(dx_group=cluster)){
  cohort_clustered %>%
    mutate(concept_id=as.integer(concept_id)) %>%
    filter(!is.na(concept_id)) %>% 
    #filter(domain_id=="Condition") %>% 
    left_join(cluster_master_pasc %>% dplyr::select(concept_id, dx_group), by="concept_id") %>%
    return()
  #compute_new(index="person_id")
}

make_cluster_sum<-function(cohort_dx_clusters, cluster_by='supercluster'){
  
  denoms<-cohort_dx_clusters %>% group_by(!!sym(cluster_by)) %>%
    summarize(denom=n_distinct(person_id))
  
  
  cohort_cluster_sum<-cohort_dx_clusters %>% 
#    dplyr::select(-cluster) %>% 
#    inner_join(cohort %>% dplyr::select(person_id, !!sym(cluster_by)), by='person_id') %>% 
    filter(!is.na(dx_group)) %>% 
    group_by_at(c('dx_group', cluster_by)) %>%
    summarize(n_persons=n_distinct(person_id)) %>%
    inner_join(denoms, by=cluster_by) %>% 
    collect %>% 
    mutate(prop=n_persons/denom)     %>%
    group_by(dx_group) %>%
    mutate(max_prop=max(prop)) %>%
    ungroup %>%
    return()
}



get_cld_df<-function(cohort_dx_clusters, cluster_by='supercluster'){
  subphenotype_clust_tbl<-cohort_dx_clusters %>%
    distinct(person_id, supercluster, dx_group) %>%
    filter(!is.na(dx_group))
  
  
  
  subphenotypes<-subphenotype_clust_tbl %>% 
    distinct(!!sym(cluster_by)) %>% pull(!!sym(cluster_by))
  dx_groups<-subphenotype_clust_tbl %>%
    distinct(dx_group) %>% pull(dx_group)
  #  chi_sq_df<-data.frame(matrix(nrow=0, ncol=3))
  
  library(multcomp)
  cld_df<-data.frame(matrix(nrow=0, ncol=length(subphenotypes)+1))
  colnames(cld_df)=c("cluster", setdiff(subphenotypes, -1) %>% as.character())
  for (clust in dx_groups){
    clust_df<-cohort_dx_clusters %>% 
      rename(cohort=!!sym(cluster_by)) %>% 
      mutate(val=case_when(dx_group==clust~1, TRUE~0))  %>% 
      distinct(person_id, cohort, val) %>%
      collect %>% 
      mutate(cohort=as.factor(cohort)) %>%
      mutate(val=as.factor(val))
    
    model<-glm(val~cohort, data=clust_df, family='binomial')
    #glht.mod <- glht(model, mcp(cohort = "tukey"))
    mult<- summary(glht(model, linfct = mcp(cohort = "Tukey")), test = adjusted("bonferroni"))
    mod.cld <- cld(mult)
    temp<-data.frame(matrix(nrow=0, ncol=length(subphenotypes)+1))
    colnames(temp)=c('cluster', names(mod.cld$mcletters$Letters))
    temp[1, ]<-c(clust, as.character(mod.cld$mcletters$Letters))
    cld_df<-cld_df %>% rbind(temp)
    
  }
  detach('package:multcomp')
  return(cld_df)
}





make_heatmap_df<-function(cluster_sum, cld_df_long){
  cluster_sum %>%  collect %>% rename(Subphenotype=supercluster) %>% 
    #filter(cluster!="pots") %>%
    complete(dx_group, Subphenotype) %>%
    replace_na(list(prop=0)) %>% 
    left_join(cld_df_long, by=c("Subphenotype", "dx_group")) %>% 
    arrange(Subphenotype) %>% 
    rename(Condition=dx_group, Proportion=prop) %>%
    mutate(Condition=str_to_title(gsub("_", " ", Condition))) %>% 
    return()
}





get_domains<-function(cohort_clustered){
  cohort_clustered %>% 
    mutate(id_as_int=as.integer(concept_id)) %>%
    compute_new(indexes=list(c('id_as_int'))) %>% 
#    collect %>% 
#    output_tbl('temp', temp=TRUE, indexes=list('id_as_int')) %>% 
    left_join(vocabulary_tbl('concept') %>% dplyr::select(concept_id, domain_id), by=c('id_as_int'='concept_id')) %>%
    mutate(domain_id=case_when(
      is.na(domain_id)~'Lab/vital',
      TRUE~domain_id
    )) %>%
    return()
}



get_top_codes<-function(cohort_clustered, cluster_by='cluster', n=10){
  cohort_pop<-cohort_clustered %>% group_by_at(cluster_by) %>%
    summarize(n_persons=n_distinct(person_id)) %>% ungroup %>%
    compute_new()
  cohort_domains<-cohort_clustered %>% get_domains() %>%
    compute_new()
  
  top_codes<-cohort_domains %>% 
#    filter(domain_id %in% c('Condition', 'Drug', 'Procedure')) %>%
    #    filter(cohort!=-1) %>% 
    group_by_at(c(cluster_by, 'concept_id', 'concept_name', 'domain_id')) %>%
    summarize(n=n_distinct(person_id)) %>% ungroup %>%
    inner_join(cohort_pop , by=cluster_by) %>% 
    collect %>% 
    mutate(prop=round(n/n_persons,4)) %>%
    group_by_at(c(cluster_by, 'domain_id')) %>%
    slice_max(prop, n=n, with_ties=FALSE) %>%
    collect %>% 
    arrange(!!sym(cluster_by))
  
  return(top_codes)
}



make_overlap_df<-function(cluster_probs){
  prob_matrix=cluster_probs %>% dplyr::select(-person_id, -cluster) %>% collect %>%  as.matrix()
  prod=t(prob_matrix) %*% prob_matrix
  
  clusts=cluster_probs %>% filter(cluster!=-1) %>% distinct(cluster) %>% pull(cluster) %>% as.character
  overlap_matrix=data.frame(matrix(nrow=length(clusts), ncol=length(clusts)))
  union_matrix=data.frame(matrix(nrow=length(clusts), ncol=length(clusts)))
  rownames(overlap_matrix)=clusts
  colnames(overlap_matrix)=clusts
  rownames(union_matrix)=clusts
  colnames(union_matrix)=clusts
  
  for (x in clusts){
    for (y in clusts){
      #       print(x)
      #       print(y)
      vec_x=cluster_probs %>% pull(!!x)
      vec_y=cluster_probs %>% pull(!!y)
      min=pmin(vec_x, vec_y)
      max=pmax(vec_x, vec_y)
      overlap_matrix[x, y]=sum(min)
      union_matrix[x, y]=sum(max)
    }
  }
  
  relative_intersection_matrix=overlap_matrix/union_matrix
  relative_intersection_matrix['cluster']=rownames(relative_intersection_matrix) 
  for (i in 1:(dim(relative_intersection_matrix)[2]-1)){
    relative_intersection_matrix[i, i]=0
  }
  rel_int_long=relative_intersection_matrix %>%
    pivot_longer(cols=setdiff(colnames(relative_intersection_matrix), 'cluster'))%>%
    rename(cluster1=cluster, cluster2=name)
  
  #   rel_int_long[rel_int_long$value==1.0, ]$value=0
  
  rel_int_long %>% rename(prop=value) %>%
    return
}

#' Function to find any ICU event in relation to an observation_date
#' @param adt_tbl table with adt_occurrences, defaulting to the cdm table adt_occurrence
#' @param cohort_tbl table with all person_ids for cohort, including their observation_date
#' @return table with NICU, PICU, CICU occurrences of any kind along with adt_type_concept_id and adt_date
get_icu <- function(adt_tbl = cdm_tbl('adt_occurrence'),
                     cohort_tbl,
                     days_start,
                     days_end) {
  cohort_adt<-cohort_tbl %>% dplyr::select(person_id, site, observation_date) %>%
    inner_join(adt_tbl, by = c('person_id', 'site')) %>%
    filter(service_concept_id %in% c(2000000079L,2000000080L,2000000078L)) %>%
    filter(adt_date>=observation_date+days(!!days_start), adt_date<observation_date+days(!!days_end)) %>% 
    distinct(person_id, site) %>%
    mutate(icu_flag=1L) %>% 
    compute_new(indexes=list(c('person_id', 'site')))  
  
  cohort_tbl %>%
    left_join(cohort_adt, by=c('person_id', 'site')) %>%
    mutate(icu_flag=case_when(is.na(icu_flag)~0L, TRUE~icu_flag)) %>%
    compute_new(indexes=list(c('person_id', 'site'))) %>%
    return
 
}

#' Function to find hospitalizations in relation to observation_date
#' @param visit_tbl table with visit_occurrences, defaulting to the cdm table visit_occurrence
#' @param cohort_tbl table with person_ids for cohort, including their observation_date
#' @param hosp_visit_types a list of visit_concept_ids to classify as hospitalizations
#' @return table with hospitalizations for the cohort at any time along with visit_start_date and visit_concept_id
get_hosp <- function(visit_tbl = cdm_tbl('visit_occurrence'),
                      cohort_tbl,
                      hosp_visit_types = c(9201L,
                                           2000000048L,
                                           2000000088L),
                      days_start, days_end) {
    print('test')
    cohort_hosp<-cohort_tbl %>% dplyr::select(person_id, site, observation_date) %>%
    inner_join(visit_tbl, by = c('person_id', 'site')) %>%
    filter(visit_concept_id %in% !!hosp_visit_types) %>%
    filter(visit_start_date>=observation_date+days(!!days_start), visit_start_date<observation_date+days(!!days_end)) %>%
    distinct(person_id, site) %>% 
    mutate(hosp_flag=1L) %>% 
    compute_new(indexes=list(c('person_id', 'site'))) 
 print('test') 
  cohort_tbl %>%
    left_join(cohort_hosp, by=c('person_id', 'site')) %>%
    mutate(hosp_flag=case_when(is.na(hosp_flag)~0L, TRUE~hosp_flag)) %>%
    compute_new(indexes=list(c('person_id', 'site'))) %>%
    return
  
}


get_util<-function(cohort, 
                   visit_tbl=cdm_tbl('visit_occurrence'),
                   visit_types=c('inpatient', 'outpatient_office', 'ed', 'outpatient_test_only', 'other_unknown')){
  cohort_visits<-cohort %>% 
    inner_join(visit_tbl, by='person_id') %>%
    dplyr::select(person_id, cohort_entry_date, visit_start_date, visit_concept_id) %>% 
    filter(visit_start_date>cohort_entry_date-days(365), visit_start_date<cohort_entry_date+days(180)) %>%
    mutate(visit_period=case_when(
      visit_start_date<cohort_entry_date~'prior',
      visit_start_date<cohort_entry_date+days(28)~'acute',
      TRUE~'post_acute'
    )) %>%
    mutate(
      visit_loc =case_when(
        visit_concept_id %in% c(9201L,2000000088L,2000000048L) ~ 'inpatient',
        visit_concept_id %in% c(9202L,581399L) ~ 'outpatient_office',
        visit_concept_id %in% c(9203L) ~ 'ed',
        visit_concept_id %in% c(2000000469L,44814711L) ~ 'outpatient_test_only',
        TRUE ~ 'other_unknown'
      )
    )%>%
    filter(visit_loc %in% visit_types) %>% 
    group_by(person_id, visit_period, visit_loc) %>%
    summarize(n_visits=as.numeric(n_distinct(visit_start_date))) %>% ungroup %>% collect %>% 
    mutate(n_visits_per=case_when(
       visit_period=='prior'~n_visits/12.0,
       visit_period=='acute'~n_visits,
       visit_period=='post_acute'~n_visits/5.0
    )) %>% 
    ungroup %>% collect %>% 
    pivot_wider(id_cols='person_id', names_from=c('visit_loc', 'visit_period'), values_from=n_visits_per, values_fill=0) %>%
    output_tbl('wide_temp', temp=TRUE, indexes=list('person_id'))
  
  replace_cols=setdiff(colnames(cohort_visits), 'person_id')
  replace_list=as.list(rep(0, length(replace_cols)))
  names(replace_list)=replace_cols
  cohort %>% 
    left_join(cohort_visits, by='person_id') %>%
    replace_na(replace=replace_list) %>%
    compute_new(indexes=list('person_id')) %>%
    return()
    
  
  
    
}
  
get_provider_trajectory<-function(cohort, days_start=0, days_end=365){
  cohort_providers<-cohort %>% 
    dplyr::select(person_id, cluster, cohort_entry_date) %>%
    inner_join(cdm_tbl("visit_occurrence"), by=c("person_id")) %>%
    filter(visit_start_date>cohort_entry_date+days(days_start), visit_start_date<cohort_entry_date+days(days_end)) %>%
    mutate(visit_month=ceiling((visit_start_date-cohort_entry_date)/30)) %>%
    #mutate(visit_month=case_when(visit_month==0~1, TRUE~visit_month)) %>% 
    group_by(person_id, cluster, visit_month) %>%
    summarize(n_providers=n_distinct(provider_id)) %>%
    group_by(cluster, visit_month) %>%
    summarize(mean_n_providers=mean(n_providers)) %>% 
    compute_new()
}
  
get_visit_trajectory<-function(cohort, days_start=0, days_end=365){
  cohort_visits<-cohort %>% 
    dplyr::select(person_id, cluster, cohort_entry_date) %>%
    inner_join(cdm_tbl("visit_occurrence"), by=c("person_id")) %>%
    filter(visit_start_date>cohort_entry_date+days(days_start), visit_start_date<cohort_entry_date+days(days_end)) %>%
    mutate(visit_month=ceiling((visit_start_date-cohort_entry_date)/30)) %>%
    #mutate(visit_month=case_when(visit_month==0~1, TRUE~visit_month)) %>% 
    group_by(person_id, cluster, visit_month) %>%
    summarize(n_visits=n_distinct(visit_start_date)) %>%
    group_by(cluster, visit_month) %>%
    summarize(mean_n_visits=mean(n_visits)) %>% 
    compute_new()
}


get_body_system_trajectory<-function(cohort, days_start=0, days_end=365){
  cohort_pmca_post_acute<-cohort %>% 
    get_pmca_post_acute(days_min=days_start, days_max=days_end)  %>%
    dplyr::select(person_id, cluster, cohort_entry_date, visit_start_date, body_system) %>% 
    mutate(visit_month=ceiling((visit_start_date-cohort_entry_date)/30)) %>%
    #mutate(visit_month=case_when(visit_month==0~1, TRUE~visit_month))  %>% 
    compute_new()
  
  #png(paste0(output_dir,"/n_body_systems_temporal.png", sep=""),width=15, height=15, units="in", res=1000)
  cohort_post_acute<-cohort_pmca_post_acute %>% 
    group_by(person_id, cluster, visit_month) %>% 
    summarize(n_body_systems=n_distinct(body_system)) %>% 
    group_by(cluster, visit_month) %>% 
    summarize(mean_n_body_systems=mean(n_body_systems)) %>% compute_new()
  
} 
  

