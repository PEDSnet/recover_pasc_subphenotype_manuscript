# Vector of additional packages to load before executing the request
config_append('extra_packages', c("tidyverse"))

#' Execute the request
#'
#' This function presumes the environment has been set up, and executes the
#' steps of the request.
#'
#' In addition to performing queries and analyses, the execution path in this
#' function should include periodic progress messages to the user, and logging
#' ofthrough [append_sum()].
#'
#' @return The return value is dependent on the content of the request, but is
#'   typically a structure pointing to some or all of the retrieved data or
#'   analysis results.  The value is not use intermediate totals and timing data d by the framework itself.
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
  
  cp_cohort_incident_non_misc<-results_tbl("cp_cohort_incident_noncomplex_v2") %>%
    distinct(person_id, site, observation_date=cohort_entry_date) %>%
    compute_new(indexes=list(c("person_id", "site")))
  
  cp_cohort_incident_non_misc_sentences<-get_cohort_sentences(cohort=cp_cohort_incident_non_misc, r1=28, r2=179, incident=TRUE, washout_months=18, domains=c('condition', 'drug', 'procedure'))
  
  cp_cohort_incident_non_misc_sentences[[1]] %>%
    output_tbl("cp_cohort_incident_noncomplex_sentences_aws_v2")
  
 
  
  
  #### patient level split
  
  require(caret)
    set.seed(42)
    
    
    
    
  pats<-results_tbl('cp_cohort_incident_noncomplex_v2') %>%
#    inner_join(cdm_tbl('person'), by='person_id') %>%
    filter(site!='mshs')%>%
    distinct(person_id, site) %>% collect
  
  train.index <- createDataPartition(pats$site, p = .5, list = FALSE)
  pats_train <- pats[ train.index,] %>% pull(person_id)
  pats_test  <- pats[-train.index,] %>% pull(person_id)

  
  results_tbl('cp_cohort_incident_noncomplex_sentences_aws_v2') %>%
    filter(person_id %in% pats_train) %>% 
    inner_join(cdm_tbl('person') %>% dplyr::select(person_id, site), by='person_id') %>%
    output_tbl('cp_cohort_sentences_aws_v2_train', indexes=list(c('person_id', 'site')))
  results_tbl('cp_cohort_incident_noncomplex_sentences_aws_v2') %>% 
    filter(person_id %in% pats_test) %>% 
    inner_join(cdm_tbl('person') %>% dplyr::select(person_id, site), by='person_id') %>%
    output_tbl('cp_cohort_sentences_aws_v2_test')
   results_tbl('cp_cohort_incident_noncomplex_sentence_names_aws_v2') %>% 
    filter(person_id %in% pats_train) %>% 
    inner_join(cdm_tbl('person') %>% dplyr::select(person_id, site), by='person_id') %>%
    output_tbl('cp_cohort_sentence_names_aws_v2_train')
  results_tbl('cp_cohort_incident_noncomplex_sentence_names_aws_v2') %>% 
    filter(person_id %in% pats_test) %>% 
    inner_join(cdm_tbl('person') %>% dplyr::select(person_id, site), by='person_id') %>%
    output_tbl('cp_cohort_sentence_names_aws_v2_test')
 
  results_tbl('cp_cohort_sentence_names_aws_v2_train') %>%
    dplyr::select(-concept_name) %>% mutate(concept_id=as.integer(concept_id)) %>% 
    inner_join(vocabulary_tbl('concept') %>% dplyr::select(concept_id, concept_name), by='concept_id') %>%
    output_tbl('cp_cohort_sentence_names_fixed_aws_v2_train', indexes=list(c('person_id', 'concept_id')))
  
   results_tbl('cp_cohort_sentence_names_aws_v2_test') %>%
    dplyr::select(-concept_name) %>% mutate(concept_id=as.integer(concept_id)) %>% 
    inner_join(vocabulary_tbl('concept') %>% dplyr::select(concept_id, concept_name), by='concept_id') %>%
    output_tbl('cp_cohort_sentence_names_fixed_aws_v2_test', indexes=list(c('person_id', 'concept_id')))
  
  
  # NO evidence cohort 
  cohort_no_evidence<-results_tbl("cohort_no_evidence_matched") %>%
    distinct(person_id, site, observation_date=cohort_entry_date) %>%
    compute_new(indexes=list(c("person_id", "site")))
  
  cohort_no_evidence_sentences<-get_cohort_sentences(cohort=cohort_no_evidence, r1=28, r2=179, incident=TRUE, washout_months=18, domains=c('condition', 'drug', 'procedure'))
  
  cohort_no_evidence_sentences[[1]] %>%
    output_tbl("cohort_no_evidence_sentences_aws_v2")
  
  #  cp_cohort_incident_non_misc_sentence_names<-get_cohort_sentence_names(cp_cohort_incident_non_misc, r1=28, r2=179,
  #                                                                        incident=TRUE)
  
  cohort_no_evidence_sentences[[2]] %>%
    output_tbl("cohort_no_evidence_sentence_names_aws_v2")
  
  
  
 message('Done.')
  
  
  
  
  ##
  invisible(rslt)
  
}




