# Vector of additional packages to load before executing the request
config_append('extra_packages', c("tidyverse", "MatchIt"))

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
  
  
  ##CP cohort INCIDENT NO MIS-C
  
  misc_ids<-cdm_tbl("observation_derivation_recover") %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==703578L) %>%
    distinct(person_id, site)
  
  ### take PASC pts from ODR
  ### join back to ODR to get COVID+ date if it occurs wtihin n days of PASC evidence
  ### run PMCA
  ### exclude complex chronic
  
  cp_cohort_incident_non_misc_temp<-cdm_tbl('observation_derivation_recover') %>% 
    #    results_tbl(in_schema('concept_embeddings', 'observation_derivation_recover'), results_tag='_pasc_104') %>%
    filter(observation_concept_id==2000001535L, value_as_concept_id %in% c(2000001536L, 2000001537L)) %>%
    #results_tbl(in_schema('phenotype_incident', 'observation_derivation_pasccp'), results_tag='_pasc_78') %>%
    #    filter(observation_concept_id==2000001535L, value_as_concept_id %in% c(2000001536L, 2000001537L)) %>%
    anti_join(misc_ids, by=c("person_id", "site")) %>% 
    group_by(person_id, site) %>% 
    summarize(pasc_date=min(observation_date)) %>% 
    ungroup %>% 
    compute_new(indexes=list(c("person_id", "site")))
  
  
  cp_cohort_incident_non_misc<-cp_cohort_incident_non_misc_temp %>%
    inner_join(cdm_tbl('observation_derivation_recover') %>%
                 filter(observation_concept_id==2000001556L, observation_source_value=='def_study_full'),
               by=c('person_id', 'site')) %>%
    filter(observation_date>pasc_date-days(180), observation_date<pasc_date-days(28)) %>%
    dplyr::select(person_id, site, cohort_entry_date=observation_date) %>%
    compute_new(indexes=list(c('person_id', 'site')))
  
  
  
  
  cp_cohort_incident_non_misc_demo <- cp_cohort_incident_non_misc %>%
    join_cohort_demo() %>% #impute_date(low=28, high=90)  %>% 
    make_pmca_tbl()
  
  
  
#  cp_cohort_incident_non_misc_demo %>% 
#    collect %>% rowwise() %>%  mutate(pmca_index=max(c_across(all_of(pmca_cols)))) %>% 
#    filter(pmca_index!=2) %>% select(-pmca_index) %>% 
#    output_tbl("cp_cohort_incident_non_misc_demo", index="person_id")
  
#  cp_cohort_incident_non_misc<-
#    results_tbl(in_schema('odr_incident', 'observation_derivation_recover'), results_tag='_pasc_104') %>%
#    filter(observation_concept_id==2000001535L, value_as_concept_id %in% c(2000001536L, 2000001537L)) %>%
#    anti_join(misc_ids, by="person_id") %>% 
#    group_by(person_id) %>% 
#    summarize(cohort_entry_date=min(observation_date)) %>% 
#    ungroup %>% 
#    compute_new(indexes=list("person_id"))
#  
# cp_cohort_incident_non_misc_v2<-
#   results_tbl(in_schema('pasc_cp_rules_v2', 'cohort_rules_flags'), results_tag='_pasc_182') %>%
#   filter(pasc_level %in% c('conclusive', 'probable'))%>% 
#   anti_join(misc_ids, by='person_id') %>% 
#   group_by(person_id) %>%
#   summarize(cohort_entry_date=min(ce_date)) %>% 
#   ungroup %>%
#   output_tbl('cohort_v2', temp=TRUE, indexes=list(c('person_id')))
   
   
  
#  cp_cohort_incident_non_misc_v2_demo <- cp_cohort_incident_non_misc_v2 %>%
#    join_cohort_demo() %>% #impute_date(low=28, high=90)  %>% 
#    make_pmca_tbl()
 
#  cp_cohort_incident_non_misc_demo <- cp_cohort_incident_non_misc %>%
#    join_cohort_demo() %>% #impute_date(low=28, high=90)  %>% 
#    make_pmca_tbl()
   
#  cp_cohort_incident_non_misc_demo %>% output_tbl("cp_cohort_incident_non_misc_demo", indexes=list("person_id"))
#  cp_cohort_incident_non_misc_v2_demo %>% output_tbl("cp_cohort_incident_non_misc_v2_demo", indexes=list("person_id"))
#  
#  pmca_cols<-results_tbl('cp_cohort_incident_non_misc_demo') %>% 
#    dplyr::select(starts_with('pmca')) %>% 
#    colnames
#    
  pmca_cols_v2<-cp_cohort_incident_non_misc_demo %>% 
    dplyr::select(starts_with('pmca')) %>% 
    colnames
    
  
  cp_cohort_incident_non_misc_demo %>%
    mutate(pmca_index=pmax(!!!rlang::syms(pmca_cols_v2))) %>%
    filter(pmca_index!=2) %>%
    output_tbl('cp_cohort_incident_noncomplex_v2', indexes=list(c('person_id', 'site')))
#  results_tbl('cp_cohort_incident_non_misc_demo') %>%
#    mutate(pmca_index=pmax(!!!rlang::syms(pmca_cols))) %>%
#    filter(pmca_index!=2) %>%
#    output_tbl('cp_cohort_incident_noncomplex', indexes=list(c('person_id')))
#
  
  ###### COVID positive 
  cp_cohort_positives<-cdm_tbl('observation_derivation_recover') %>%
   filter(observation_concept_id==2000001556L, observation_source_value=='def_infection_date_any') %>% 
  anti_join(results_tbl('cp_cohort_incident_noncomplex_v2'), by='person_id') %>% 
   anti_join(misc_ids, by='person_id') %>% 
   mutate(cohort_entry_date=observation_date) %>% 
   output_tbl('cohort_positives_v2', temp=TRUE, indexes=list(c('person_id')))
   
   
  
  cp_cohort_positives_v2_demo <- cp_cohort_positives %>%
    dplyr::select(person_id, cohort_entry_date) %>% 
    join_cohort_demo() %>% #impute_date(low=28, high=90)  %>% 
    make_pmca_tbl()
 
  cp_cohort_positives_v2_demo %>% output_tbl("cp_cohort_positives_v2_demo", indexes=list("person_id"))
  
  pmca_cols_v2<-results_tbl('cp_cohort_positives_v2_demo') %>% 
    dplyr::select(starts_with('pmca')) %>% 
    colnames
  
  results_tbl('cp_cohort_positives_v2_demo') %>%
    require_post_acute_dxs() %>% 
    mutate(pmca_index=pmax(!!!rlang::syms(pmca_cols_v2))) %>%
    filter(pmca_index!=2) %>%
    output_tbl('cp_cohort_positives_v2', indexes=list(c('person_id')))   
  
 # remove PASC and MIS-C patients. 
  misc_ids<-cdm_tbl("observation_derivation_recover") %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==703578L) %>%
    distinct(person_id)
  
  pasc_ids<-cdm_tbl('observation_derivation_recover') %>% 
    filter(observation_concept_id==2000001535L, value_as_concept_id %in% c(2000001536L, 2000001537L)) %>%
    distinct(person_id)
  
  covid_non_pasc<-results_tbl('cp_cohort_positives_v2') %>%
    anti_join(misc_ids, by='person_id') %>%
    anti_join(pasc_ids, by='person_id') %>%
    collect
    
  
  # Match
  
  
  
  
 ###### No evidence control 
  max_date='2022-09-01'
  
  covid_pos<-cdm_tbl('observation_derivation_recover') %>%
   filter(observation_concept_id==2000001556L, observation_source_value=='def_study_full') %>% 
   mutate(covid_date=observation_date) %>% 
   dplyr::select(person_id, covid_date) %>%
    filter(covid_date<max_date) %>% 
   output_tbl('covid_pos', temp=TRUE, indexes=list(c('person_id')))

  misc_ids<-cdm_tbl("observation_derivation_recover") %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==703578L) %>%
    distinct(person_id)
  
  pasc_ids<-cdm_tbl('observation_derivation_recover') %>% 
    filter(observation_concept_id==2000001535L, value_as_concept_id %in% c(2000001536L, 2000001537L)) %>%
    distinct(person_id)
  
  cohort_no_evidence<-cdm_tbl('visit_occurrence') %>%
    filter(visit_start_date>='2020-03-01', visit_start_date<'2022-09-01') %>%
    anti_join(misc_ids, by='person_id') %>%
    anti_join(pasc_ids, by='person_id') %>%
    anti_join(covid_pos, by='person_id') %>% 
    group_by(person_id) %>%
    slice_sample(n=1) %>%
    mutate(cohort_entry_date=visit_start_date) %>%
    distinct(person_id, cohort_entry_date) %>%
    output_tbl('cohort_no_evidence_temp', temp=TRUE, indexes=list(c('person_id')))
    
   
  cohort_no_evidence_demo <- cohort_no_evidence %>%
    join_cohort_demo() %>% #impute_date(low=28, high=90)  %>% 
    make_pmca_tbl()
 
  cohort_no_evidence_demo %>% output_tbl("cohort_no_evidence_demo", indexes=list("person_id"))

  covid_evidence<-cdm_tbl('observation_derivation_recover') %>%
    filter(
      (observation_concept_id==2000001527L & 
         value_as_concept_id %in% c(2000001523L, 2000001522L, 2000001525L, 2000001520L, 703578L, 2000001533L))|
        (observation_concept_id %in% c(2000001528L, 2000001529L, 2000001530L) & 
           value_as_concept_id %in% c(2000001526L, 9191L))|
        observation_concept_id==2000001555L
    ) %>%
    output_tbl('covid_ev', temp=TRUE, indexes=list(c('person_id')))
    
  cohort_drop<-results_tbl('cohort_no_evidence_demo') %>%
    inner_join(covid_evidence, by='person_id') %>%
    filter(observation_date<cohort_entry_date+days(180)) %>%
    output_tbl('cohort_drop', temp=TRUE, indexes=list(c('person_id')))
  
  cohort_no_evidence_keep<-results_tbl('cohort_no_evidence_demo') %>%
    anti_join(cohort_drop, by='person_id') %>%
    output_tbl('cohort_no_evidence_keep', indexes=list(c('person_id')))
      
    
      
  pmca_cols_v2<-results_tbl('cohort_no_evidence_keep') %>% 
    dplyr::select(starts_with('pmca')) %>% 
    colnames
  
  results_tbl('cohort_no_evidence_keep') %>%
    require_post_acute_dxs() %>% 
    mutate(pmca_index=pmax(!!!rlang::syms(pmca_cols_v2))) %>%
    filter(pmca_index!=2) %>%
    output_tbl('cohort_no_evidence_pre_match', indexes=list(c('person_id')))   
  

  cohort_no_evidence_pre_match<-results_tbl('cohort_no_evidence_pre_match') %>% 
    mutate(pasc=0L) %>%
    collect
  cohort_pasc_pre_match<-results_tbl('cp_cohort_incident_noncomplex_v2') %>%
    mutate(pasc=1L) %>%
    collect
    
  # Match
  cohort_combined<-cohort_no_evidence_pre_match %>%
    dplyr::union(cohort_pasc_pre_match)
  colnames(cohort_combined)<-make.names(colnames(cohort_combined))
  require(MatchIt)
  m.out=matchit(pasc~site+obs_age_cat+sex_cat+eth_cat+cohort_entry_period+
                  pmca_hematological+pmca_renal+pmca_gastrointestinal+
                  pmca_musculoskeletal+pmca_pulmonary.respiratory+pmca_genitourinary+
                  pmca_otologic+pmca_genetic+pmca_malignancy+pmca_metabolic+pmca_endocrinological+
                  pmca_craniofacial+pmca_cardiac+pmca_immunological+pmca_dermatological+
                  pmca_ophthalmological+pmca_mental.health, 
                data=cohort_combined, method='nearest',
                distance='glm', estimand='ATT', ratio=1, verbose=TRUE)
  s.out<-summary(m.out)
  png('./results/pasc_no_ev_match.png')
  plot(s.out)
  dev.off()
  
  cohort_combined$weight=m.out$weights
  cohort_matched<-cohort_combined %>% filter(weight==1)
  cohort_no_ev_matched<-cohort_matched %>% filter(pasc==0)
  
  cohort_no_ev_matched %>%
    output_tbl('cohort_no_evidence_matched', indexes=list(c('person_id')))
  
  
  
  
  message('Done.')
  
  invisible(rslt)
  
}
