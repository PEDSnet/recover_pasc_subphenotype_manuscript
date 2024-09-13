

get_pmca_post_acute<-function(cohort, days_min=180, days_max=179,
                              pmca_xwalk=load_codeset('pmca_icd10'),
                              condition_tbl=cdm_tbl('condition_occurrence'),
                              visit_tbl=cdm_tbl('visit_occurrence')){
  
  only_pmca_conds <- 
    cohort%>% 
    inner_join(
      dplyr::select(condition_tbl,
             person_id,
             condition_concept_id,
             condition_concept_name,
             condition_source_concept_id,
             condition_source_concept_name,
             visit_occurrence_id,
             condition_start_date),
      by=c('person_id')
    ) %>% inner_join(visit_tbl %>% dplyr::select(visit_occurrence_id, visit_start_date), by="visit_occurrence_id")%>% 
    filter(visit_start_date <= cohort_entry_date+days(days_max) &
             visit_start_date > cohort_entry_date - days(days_min)) %>%
    inner_join(
      pmca_xwalk,
      by=c('condition_source_concept_id'='concept_id')
    ) %>% 
    inner_join(
      dplyr::select(
        visit_tbl,
        visit_occurrence_id,
        visit_concept_id
      )
      %>% filter(visit_concept_id %in% c(9202L,9201L,9203L,
                                         2000000048L,2000000088L,581399L)), by="visit_occurrence_id") %>%
    distinct() #%>% 
#    add_site_split()
}






#' produce PMCA table wiith 3 year lookback
#' 
#' @param cohort cohort of patients with patients and cohort_entry_date
#' @param pmca_xwalk codeset that has flags for body systems and whether or not progressive
#' @param condition_tbl formatting of the condition occurrence table
#' @param visit_tbl formatting of the visit occurrence table
#' 
#' @return table that has conditions and flags for body systems
#' columns: 
#' person_id | cohort_entry_date | result_derivation | condition_concept_id | condition_concept_name | 
#' condition_source_concept_id | condition_source_value | visit_occurrence_id | condition_start_state |
#' description | body_system | progressive | visit_concept_id | site
#' 

produce_pmca_lookup <- function (cohort,
                                 pmca_xwalk=load_codeset('pmca_icd10'),
                                 condition_tbl=cdm_tbl('condition_occurrence'),
                                 visit_tbl=cdm_tbl('visit_occurrence')) {
  only_pmca_conds <- 
    cohort %>% 
    inner_join(
      dplyr::select(condition_tbl,
             person_id, site, 
             condition_concept_id,
             condition_concept_name,
             condition_source_concept_id,
             condition_source_concept_name,
             visit_occurrence_id,
             condition_start_date),
      by=c('person_id', 'site')
    ) %>% inner_join(visit_tbl %>% dplyr::select(visit_occurrence_id, site, visit_start_date), by=c("visit_occurrence_id", "site"))%>% 
    filter(cohort_entry_date <= visit_start_date+years(3) &
             cohort_entry_date > visit_start_date + days(7)) %>%
    inner_join(
      pmca_xwalk,
      by=c('condition_source_concept_id'='concept_id')
    ) %>% 
    inner_join(
      dplyr::select(
        visit_tbl, site,
        visit_occurrence_id,
        visit_concept_id
      )
      %>% filter(visit_concept_id %in% c(9202L,9201L,9203L,
                                         2000000048L,2000000088L,581399L)), by=c("visit_occurrence_id", "site")) %>%
    distinct() # %>% 
#    add_site_split()
  
}


#' compute patient, body system with visit number, and flags for malignancy and progressive
#' 
#' @param pmca_lookup_tbl pmca table output from `produce_pmca_lookup`; 
#' must contain `cohort_entry_date` and `body_system` and `condition_start_date` and flagged conditions
#' 
#' @return computes information to be able to apply algorithms; groups by body system and counts visits,
#' with flags for progressive or malignancy for patients
#' 
#' person_id | cohort_entry_date | result_derivation | body_system | 
#' yr_1 | yr_2 | yr_3 | total_visits | progressive | malignancy
#' 

compute_pmca_summary <- function(pmca_lookup_tbl) {
  
  add_year <- 
    pmca_lookup_tbl %>%
    filter(
      ! body_system == 'malignancy'
    ) %>% mutate(
      flag_yr = case_when(
        condition_start_date < cohort_entry_date &
          condition_start_date >= sql("(cohort_entry_date - interval '1 year')::date") ~ 'yr_1',
        condition_start_date < sql("(cohort_entry_date - interval '1 year')::date") &
          condition_start_date >= sql("(cohort_entry_date - interval '2 years')::date") ~ 'yr_2',
        condition_start_date < sql("(cohort_entry_date - interval '2 years')::date") &
          condition_start_date >= sql("(cohort_entry_date - interval '3 years')::date") ~ 'yr_3',
        TRUE ~ 'no_yr'
      )) %>% 
    group_by(
      person_id, site, 
      cohort_entry_date,
      #result_derivation,
      body_system,
      flag_yr
    ) %>% summarise(
      visit_yr_ct=as.integer(n_distinct(condition_start_date))
    ) %>% pivot_wider(names_from = flag_yr,
                      values_from = visit_yr_ct,
                      values_fill = 0L) %>% 
    mutate(total_visits = yr_1 + yr_2 + yr_3) %>% 
    ungroup() %>% compute_new(temporary=TRUE,
                              indexes=list(c('person_id', 'site')))
  
  progressive_malignant_pts <- 
    pmca_lookup_tbl %>%
    filter(
      progressive == 'yes' |
        body_system == 'malignancy'
    ) %>% mutate(malignancy = 
                   case_when(
                     body_system == 'malignancy' ~ 'yes',
                     TRUE ~ 'no'
                   )) %>%
    filter(malignancy=='yes' | progressive == 'yes') %>%
    dplyr::select(person_id, site, 
           progressive,
           malignancy) %>% distinct %>%
    compute_new(temporary=TRUE,
                indexes=list('person_id', 'site'))
  
  all_pts <- 
    dplyr::union(
      add_year %>% ungroup() %>% dplyr::select(person_id, site),
      progressive_malignant_pts %>% dplyr::select(person_id, site)
    )
  
  all_pts %>%
    left_join(add_year) %>%
    left_join(progressive_malignant_pts) %>%
    mutate(
      progressive=case_when(
        is.na(progressive) ~ 'no',
        TRUE ~ progressive
      ),
      malignancy=case_when(
        is.na(malignancy) ~ 'no',
        TRUE ~ malignancy
      )
    ) %>% dplyr::select(
      person_id, site, cohort_entry_date,#result_derivation,
      body_system,yr_1,yr_2,yr_3,total_visits,progressive,malignancy
    )
  
}



#' compute classification algorithm for *most conservative*: 
#' *complex chronic* is defined as patients with at least 1 visit for two body systems for all three years OR progressive OR malignant
#' *chronic* is defined as having at least 1 visit all three years for just one body system
#' 
#' @param pmca_lookup_tbl output from `produce_pmca_lookup`
#' 
#' @return table that has patients in the *most conservative* category with the following columns:
#' person_id | cohort_entry_date | result_derivation | body_system | yr_1 | yr_2 | yr_3 | total_visits |
#' progressive | malignancy | complex_chronic | chronic | non_complex_chronic
#'

compute_pmca_cats_cons <- function(pmca_summary_tbl) {
  
  gt_two_bs <- 
    pmca_summary_tbl %>%
    filter(
      yr_1 > 0 &
        yr_2 > 0 &
        yr_3 > 0
    ) %>%
    group_by(person_id,
             cohort_entry_date,
             result_derivation) %>%
    summarise(body_system_ct=n_distinct(body_system)) %>%
    filter(
      body_system_ct > 1
    ) %>% ungroup()
  
  prog_or_malig <-
    pmca_summary_tbl %>% 
    filter(progressive == 'yes' | malignancy == 'yes') 
  
  complex_pts <- 
    dplyr::union(
      gt_two_bs %>% dplyr::select(person_id,cohort_entry_date),
      prog_or_malig %>% dplyr::select(person_id,cohort_entry_date)
    ) %>% mutate(complex_chronic = 1L) %>% 
    compute_new(temporary=TRUE,
                indexes=list('person_id'))
  
  chronic_pts <- 
    pmca_summary_tbl %>%
    filter(
      yr_1 > 0 &
        yr_2 > 0 &
        yr_3 > 0
    ) %>% anti_join(complex_pts,
                    by=c('person_id','cohort_entry_date')) %>%
    distinct(person_id, cohort_entry_date) %>% mutate(chronic = 1L) %>%
    compute_new(temporary=TRUE,
                indexes=list('person_id'))
  
  pmca_summary_tbl %>%
    left_join(complex_pts) %>%
    left_join(chronic_pts) %>%
    mutate(complex_chronic=case_when(is.na(complex_chronic) ~ 0L,
                                     TRUE ~ complex_chronic),
           chronic=case_when(is.na(chronic) ~ 0L,
                             TRUE ~ chronic)) %>%
    mutate(non_complex_chronic = 
             case_when(complex_chronic == 1 | chronic == 1 ~ 0L,
                       TRUE ~ 1L)) %>% add_site_split()
}


#' compute PMCA *more conservative* algorithm which we will shorthand *lib*
#' *complex chronic*: >= 2 claims per body system for 2 different body systems OR progressive OR malignant over measurement period
#' *chronic*: >= 2 claims for a single body system not flagged as progressive or malignant
#' 
#' 
#' @param pmca_summary_tbl output from `compute_pmca_summary()`
#' 
#' @return table that has patients in the *more conservative* / *lib* category with the following columns:
#' person_id | cohort_entry_date | result_derivation | flag_yr | body_system| 
#' yr_1 | yr_2 | yr_3 | total_visits | progressive | malignancy | complex_chronic | chronic | non_complex_chronic
#'

compute_pmca_cats_lib <- function(pmca_summary_tbl) {
  
  gt_two_bs <- 
    pmca_summary_tbl %>%
    filter(
      total_visits >= 2
    ) %>%
    group_by(person_id, site,
             cohort_entry_date,
             #result_derivation
    ) %>%
    summarise(body_system_ct=n_distinct(body_system)) %>%
    filter(
      body_system_ct > 1
    ) %>% ungroup()
  
  prog_or_malig <-
    pmca_summary_tbl %>% 
    filter(progressive == 'yes' | malignancy == 'yes') 
  
  complex_pts <- 
    dplyr::union(
      gt_two_bs %>% dplyr::select(person_id,site, cohort_entry_date),
      prog_or_malig %>% dplyr::select(person_id, site, cohort_entry_date)
    ) %>% mutate(complex_chronic = 1L) %>% 
    compute_new(temporary=TRUE,
                indexes=list('person_id', 'site'))
  
  chronic_pts <- 
    pmca_summary_tbl %>%
    filter(
      total_visits >= 2
    ) %>% anti_join(complex_pts,
                    by=c('person_id','cohort_entry_date', 'site')) %>%
    distinct(person_id, site, cohort_entry_date) %>% mutate(chronic = 1L) %>%
    compute_new(temporary=TRUE,
                indexes=list('person_id', 'site'))
  
  pmca_summary_tbl %>%
    left_join(complex_pts, by=c('person_id', 'site')) %>%
    left_join(chronic_pts, by=c('person_id', 'site')) %>%
    mutate(complex_chronic=case_when(is.na(complex_chronic) ~ 0L,
                                     TRUE ~ complex_chronic),
           chronic=case_when(is.na(chronic) ~ 0L,
                             TRUE ~ chronic)) %>%
    mutate(non_complex_chronic = 
             case_when(complex_chronic == 1 | chronic == 1 ~ 0L,
                       TRUE ~ 1L)) %>% add_site_split()
}

#' patients with no pmca because no history three years prior
#' 
#' @param test_tbl
#' @param pmca_lookup_tbl
#' @param condition_tbl
#' 
#' @return list of patients with their result status, and column `pmca_flag` with
#' valueset `complex_chronic`, `chronic`, `non_complex_chronic`, `no_followup`
#' 

categorize_pmca_all_pts <- function(test_tbl,
                                    pmca_cats,
                                    pmca_lookup_tbl=results_tbl('pmca_lookup'),
                                    condition_tbl=cdm_tbl('condition_occurrence')) {
  
  no_three_yrs <- 
    test_tbl %>%
    inner_join(
      dplyr::select(
        condition_tbl,
        person_id,
        condition_start_date
      )
    ) %>% mutate(three_yrs_flag = 
                   case_when(
                     (cohort_entry_date - condition_start_date <= 1096L &
                        cohort_entry_date - condition_start_date > 0L) ~ 1L,
                     TRUE ~ 0L
                   )
    ) %>% 
    anti_join(
      pmca_lookup_tbl,
      by='person_id'
    ) %>% group_by(
      person_id,
      three_yrs_flag
    ) %>% summarise(
      ct=n_distinct(condition_start_date)
    ) %>% ungroup() %>% group_by(person_id) %>%
    mutate(three_yrs_flag_sum=sum(three_yrs_flag)) %>%
    filter(three_yrs_flag_sum == 0) %>%
    compute_new(temporary=TRUE,
                indexes=list('person_id'))
  
  test_tbl %>%
    left_join(no_three_yrs,by='person_id') %>%
    left_join(dplyr::select(
      pmca_cats,
      person_id,
      complex_chronic,
      chronic,
      non_complex_chronic
    ) %>% distinct(),
    by='person_id'
    ) %>% mutate(
      pmca_flag=
        case_when(
          three_yrs_flag == 0L ~ 'no_followup',
          complex_chronic == 1L ~ 'complex_chronic',
          chronic == 1L ~ 'chronic',
          TRUE ~ 'non_complex_chronic'
        )
    ) %>% 
    dplyr::select(person_id,
           cohort_entry_date,
           pmca_flag) %>% 
    distinct()
  
  
  
}



#' computes summaries
#' 
#' 
#' @param pmca_lookup
#' @param pmca_cons
#' @param pmca_lib
#' 
#' 
#' 

compute_pmca_cts <- function(pmca_lookup_tbl,
                             pmca_alg,
                             test_pts) {
  
  neg_only_pts <- 
    test_pts %>%
    filter(result_derivation == 2000001414) %>%
    anti_join(
      test_pts %>% filter(result_derivation == 2000001413),
      by='person_id'
    ) 
  
  total_pts <- 
    dplyr::union(
      test_pts %>% filter(result_derivation == 2000001413),
      neg_only_pts
    ) %>%
    group_by(result_derivation) %>%
    summarise(ct=n_distinct(person_id))
  
  body_system_ct <-
    pmca_lookup_tbl %>% 
    group_by(result_derivation,
             body_system) %>% 
    summarise(total_pt_ct = n_distinct(person_id)) %>%
    inner_join(total_pts) %>% collect() %>% ungroup() %>%
    mutate(prop_pt = round(total_pt_ct / ct,2)) %>% 
    mutate(pt_status = case_when(
      result_derivation == 2000001413 ~ 'positive', 
      TRUE ~ 'negative')) %>% 
    dplyr::select(-result_derivation) 
  
  pmca_alg_complex <- 
    pmca_alg %>%
    group_by(result_derivation,
             complex_chronic) %>%
    summarise(total_pt_ct = n_distinct(person_id)) %>%
    inner_join(total_pts) %>% collect() %>% ungroup() %>%
    mutate(prop_pt = round(total_pt_ct / ct,2)) %>% 
    mutate(pt_status = case_when(
      result_derivation == 2000001413 ~ 'positive', 
      TRUE ~ 'negative')) %>% 
    dplyr::select(-result_derivation) 
  
  pmca_alg_chronic <- 
    pmca_alg %>%
    group_by(result_derivation,
             chronic) %>%
    summarise(total_pt_ct = n_distinct(person_id)) %>%
    inner_join(total_pts) %>% collect() %>% ungroup() %>%
    mutate(prop_pt = round(total_pt_ct / ct,2)) %>% 
    mutate(pt_status = case_when(
      result_derivation == 2000001413 ~ 'positive', 
      TRUE ~ 'negative')) %>% 
    dplyr::select(-result_derivation) 
  
  
  pmca_alg_none <- 
    pmca_alg %>%
    group_by(result_derivation,
             non_complex_chronic) %>%
    summarise(total_pt_ct = n_distinct(person_id)) %>%
    inner_join(total_pts) %>% collect() %>% ungroup() %>%
    mutate(prop_pt = round(total_pt_ct / ct,2)) %>% 
    mutate(pt_status = case_when(
      result_derivation == 2000001413 ~ 'positive', 
      TRUE ~ 'negative')) %>% 
    dplyr::select(-result_derivation) 
  
  
}




make_pmca_tbl<-function(cohort){
  pmca_lookup <- 
    produce_pmca_lookup(cohort=cohort) %>%
    output_tbl('pmca_lookup',
               indexes=list('person_id', "site",
                            'body_system',
                            'condition_concept_id',
                            'visit_occurrence_id'),
               db=TRUE,file=FALSE)
  
  pmca_summary <- 
    compute_pmca_summary(pmca_lookup_tbl=results_tbl('pmca_lookup')) %>%
    output_tbl('pmca_summary',
               indexes=list('person_id', "site", 
                            'body_system'))
  
  pmca_cats_lib <- 
    compute_pmca_cats_lib(pmca_summary_tbl = results_tbl('pmca_summary')) %>%
    output_tbl('pmca_cats_lib',
               indexes=list('person_id', "site", 
                            'cohort_entry_date')) 
  
  pmca_tbl<-results_tbl("pmca_lookup") %>%
    mutate(temp_index=case_when(
      progressive %in% c("yes", "n/a")~2,
      TRUE~1
    )) %>% 
    group_by(person_id, site, body_system) %>%
    summarize(pmca_index=max(temp_index)) %>% 
    ungroup %>% 
    compute_new(indexes=list(c("person_id", "site")))
  
  cohort_cols<-colnames(cohort)
  
  cohort_pmca_temp<- cohort %>%
    left_join(
      pmca_tbl%>% 
        distinct(person_id, site, body_system, pmca_index),
      by=c("person_id", "site")
    ) %>%
    mutate(pmca_index=case_when(is.na(pmca_index)~0, TRUE~pmca_index)) %>%
    pivot_wider(id_cols=
                  all_of(cohort_cols),
                names_from=body_system, values_from=pmca_index, values_fill=0, names_prefix="pmca_") %>% 
    dplyr::select(-'pmca_NA') 
  
  cohort_pmca<-cohort_pmca_temp %>%
    replace_na(setNames(lapply(vector("list", ncol(cohort_pmca_temp)-length(cohort_cols)), function(x) x <- 0), setdiff(colnames(cohort_pmca_temp), 
                                                                                                                        cohort_cols))) %>%
    compute_new(indexes=list(c("person_id", "site")))
  return(cohort_pmca)
}





