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
 
  
  
  cond_tbl<-cdm_tbl("condition_occurrence")
  drug_tbl<-cdm_tbl("drug_exposure")
  lab_tbl<-cdm_tbl("measurement_labs")
  vitals_tbl<-cdm_tbl('measurement_vitals')
  proc_tbl<-cdm_tbl("procedure_occurrence")
  
  
  cond_codes<-cond_tbl %>% 
    mutate(month_year=paste(month(condition_start_date), year(condition_start_date)), sep="_") %>% 
    inner_join(vocabulary_tbl("concept") %>% filter(concept_code!="No matching concept", vocabulary_id=="ICD10CM") %>% dplyr::select(concept_id), by=c("condition_source_concept_id"="concept_id")) %>%
    rename(concept_id=condition_source_concept_id) %>% 
    distinct(person_id, month_year, concept_id, site) %>% 
    dplyr::select(person_id, month_year, concept_id, site) %>% 
    output_tbl('cond_codes', indexes=list(c("person_id", "month_year", "site")))
  
  scdf_xwalk<-vocabulary_tbl('concept') %>% 
    filter(vocabulary_id=="RxNorm", concept_code!="No matching concept", domain_id=="Drug", concept_class_id=="Clinical Drug Form") %>%
    dplyr::select(concept_id) %>%
    inner_join(vocabulary_tbl('concept_ancestor'), by=c('concept_id'='ancestor_concept_id')) %>%
    rename(ancestor_concept_id=concept_id) %>% 
    compute_new(indexes=list(c('ancestor_concept_id', 'descendant_concept_id')))
  
  drug_codes<-drug_tbl %>% 
    mutate(month_year=paste(month(drug_exposure_start_date), year(drug_exposure_start_date)), sep="_") %>% 
    inner_join(vocabulary_tbl("concept") %>% filter(concept_code!="No matching concept", vocabulary_id=="RxNorm", domain_id=="Drug") %>% dplyr::select(concept_id), by=c("drug_concept_id"="concept_id")) %>%
    inner_join(scdf_xwalk, by=c('drug_concept_id'='descendant_concept_id')) %>%
    rename(concept_id=ancestor_concept_id) %>% 
    distinct(person_id, month_year, concept_id, site) %>% 
    dplyr::select(person_id, month_year, concept_id, site) %>% 
    output_tbl('drug_codes', indexes=list(c("person_id", "month_year", "site")))
  
  
    proc_codes<-proc_tbl %>% 
      mutate(month_year=paste(month(procedure_date), year(procedure_date)), sep="_") %>% 
      inner_join(vocabulary_tbl("concept") %>% filter(concept_code!="No matching concept", vocabulary_id %in% c("ICD10PCS", "HCPCS", "CPT4"), domain_id=="Procedure") %>% dplyr::select(concept_id), by=c("procedure_concept_id"="concept_id")) %>%
      rename(concept_id=procedure_concept_id) %>% 
      distinct(person_id, month_year, concept_id, site) %>% 
      output_tbl('proc_codes', indexes=list(c("person_id", "month_year", "site")))
  
  
  n_codes=results_tbl('cond_codes') %>%
    dplyr::union_all(results_tbl('drug_codes')) %>%
    dplyr::union_all(results_tbl('proc_codes')) %>%
    compute_new(indexes=list(c('person_id'))) %>% 
#    union_all(results_tbl('lab_codes_v2')) %>% 
#    union_all(results_tbl('vitals_codes_v2')) %>% 
    group_by(person_id, month_year, site) %>%
    mutate(n=n()) %>%
    ungroup %>%
    filter(n>1) %>% summarize(n=n()) %>% pull(n)
  n_codes=n
  
  code_tbl<-results_tbl('cond_codes') %>% rename(concept=concept_id) %>% mutate(concept=as.character(concept)) %>% 
    union_all(results_tbl('drug_codes') %>% rename(concept=concept_id)%>% mutate(concept=as.character(concept))) %>%
    union_all(results_tbl('proc_codes') %>% rename(concept=concept_id)%>% mutate(concept=as.character(concept))) %>%
    group_by(person_id, month_year, site) %>%
    mutate(n=n()) %>%
    ungroup %>%
    filter(n>1) %>%
    group_by(person_id, month_year, site) %>% 
    slice_sample(n=n_codes) %>%  
    summarize(codes=str_flatten(as.character(concept), collapse=" ")) %>% 
    compute_new(indexes=list(c("person_id", "month_year", "site")))
  
  
  
  code_tbl %>% output_tbl("codes_by_site_aws", indexes=list(c("person_id", "month_year", "site")))
  
 
  message('Done.')
  
  invisible(rslt)
  
}
