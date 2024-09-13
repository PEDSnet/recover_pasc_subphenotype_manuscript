#'
#' This file contains functions to identify cohorts in a study.  Its
#' contents, as well as the contents of any R files whose name begins
#' with "cohort_", will be automatically sourced when the request is
#' executed.
#'
#' For simpler requests, it will often be possible to do all cohort
#' manipulation in functions within this file.  For more complex
#' requests, it may be more readable to separate different cohorts or
#' operations on cohorts into logical groups in different files.
#'



join_cohort_demo<-function(cohort, max_date="2022-09-01"){
  cohort_demo<- cohort %>%
    mutate(cohort_entry_date=as.Date(cohort_entry_date)) %>% 
    inner_join(cdm_tbl("person") %>% dplyr::select(person_id, site, birth_date,
                                                   gender_concept_id, race_concept_id,
                                                   ethnicity_concept_id), by=c("person_id")) %>%
    mutate(entry_age=floor((cohort_entry_date-birth_date)/365.25)) %>%
    filter(entry_age<21) %>%
    mutate(obs_age_cat = case_when(entry_age < 1 ~ '<1',
                                   entry_age <  5 ~ '1-4',
                                   entry_age < 12 ~ '5-11',
                                   entry_age < 16 ~ '12-15',
                                   entry_age < 21 ~ '16-20')) %>% 
    mutate(sex_cat = case_when(gender_concept_id == 8507L ~ 'Male',
                               gender_concept_id == 8532L ~ 'Female',
                               TRUE ~ 'Other/unknown'),
           eth_cat = case_when(ethnicity_concept_id == 38003563L ~ 'Hispanic',
                               race_concept_id == 8516L ~ 'Black/AA',
                               race_concept_id %in% c(8515L, 8557L) ~
                                 'Asian/PI',
                               #                               race_concept_id == 8657L ~ 'Native American',
                               race_concept_id == 8527L ~ 'White',
                               race_concept_id == 44814659L ~ 'Multiple',
                               TRUE ~ 'Other/Unknown')) %>%
    dplyr::select(-gender_concept_id, -race_concept_id, -ethnicity_concept_id) %>%
    filter(cohort_entry_date<as.Date(max_date), cohort_entry_date>=as.Date("2020-03-01")) %>%
    mutate(cohort_entry_month=paste(month(cohort_entry_date), year(cohort_entry_date), sep=",")) %>%
    mutate(cohort_entry_period=case_when(
      year(cohort_entry_date)=="2020" & month(cohort_entry_date) %in% c(3, 4, 5, 6)~"mar_jun_20",
      year(cohort_entry_date)=="2020" & month(cohort_entry_date) %in% c(7, 8, 9, 10)~"jul_oct_20",
      (year(cohort_entry_date)=="2020" & month(cohort_entry_date) %in% c(11, 12))|
        (year(cohort_entry_date)=="2021" & month(cohort_entry_date) %in% c(1, 2))~"nov_feb_21",
      year(cohort_entry_date)=="2021" & month(cohort_entry_date) %in% c(3, 4, 5, 6)~"mar_jun_21",
      year(cohort_entry_date)=="2021" & month(cohort_entry_date) %in% c(7, 8, 9, 10)~"jul_oct_21",
      (year(cohort_entry_date)=="2021" & month(cohort_entry_date) %in% c(11, 12))|
        (year(cohort_entry_date)=="2022" & month(cohort_entry_date) %in% c(1, 2))~"nov_feb_22",
      year(cohort_entry_date)=="2022" & month(cohort_entry_date) %in% c(3, 4, 5, 6)~"mar_jun_22",
      year(cohort_entry_date)=="2022" & month(cohort_entry_date) %in% c(7, 8)~"jul_aug_22"
    ))  %>%
    mutate(follow_days=as.numeric(as.Date(max_date)-cohort_entry_date)) %>%
    mutate(follow_months=floor(follow_days/(365.25/12))) %>%
    compute_new(indexes=list(c("person_id", "site")))
}

get_b94_dx<-function(
    b94_src_val_exclude=read.csv("./specs/b94_src_val_exclude.csv") %>% 
      dplyr::select(condition_source_value) %>%
      output_tbl("b94_src_val_exclude", temp=TRUE, index="condition_source_value")
){
  b94_code_dx<-vocabulary_tbl("concept") %>% 
    filter(grepl("B94.8", concept_code), vocabulary_id==("ICD10CM")) %>% 
    dplyr::select(concept_id) %>%
    inner_join(cdm_tbl("condition_occurrence"), by=c("concept_id"="condition_source_concept_id")) %>%
    anti_join(b94_src_val_exclude, by="condition_source_value") %>%
    left_join(cdm_tbl("visit_occurrence") %>% 
                dplyr::select(visit_occurrence_id, visit_concept_id, visit_start_date), by="visit_occurrence_id") %>%
    mutate(event_loc=case_when(
      visit_concept_id %in% c(9201L,2000000088L,2000000048L) ~ 'Inpatient',
      visit_concept_id %in% c(9202L,581399L) ~ 'Outpatient Office',
      visit_concept_id %in% c(9203L) ~ 'ED',
      visit_concept_id %in% c(2000000469L,44814711L) ~ 'Outpatient: Test Only',
      TRUE ~ 'Other/Unknown'
    )) %>%
    mutate(event_type="b94_8_dx") %>% 
    mutate(cohort_entry_date=visit_start_date-weeks(4)) %>% 
    distinct(person_id, cohort_entry_date, site, event_type, event_loc) %>%
    compute_new(index="person_id")
  
  
  
  
}


require_prior_encounter<-function(cohort){
  keep_ids<-cohort %>% 
    inner_join(cdm_tbl("visit_occurrence") %>% distinct(person_id, visit_start_date), by="person_id") %>%
    filter(visit_start_date<cohort_entry_date-days(7), visit_start_date>cohort_entry_date-months(18)) %>%
    distinct(person_id) %>%
    compute_new(index="person_id")
  
  cohort %>% inner_join(keep_ids, by="person_id") %>% compute_new(index="person_id")
}


#'need to update to get b94.8 also
#'
get_pasc_dx_flag<-function(cohort, max_date="2022-09-01"){
  u09_dx<-cdm_tbl("observation_derivation_recover") %>% 
    filter(observation_date<max_date) %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==2000001520L) %>%
    distinct(person_id) %>%
    mutate(pasc_flag=1) %>%
    compute_new(index="person_id")
  
  b94_dx<-get_b94_dx() %>%
    distinct(person_id) %>%
    mutate(pasc_flag=1) %>%
    compute_new(index="person_id")
  
  pasc_dx<-u09_dx %>%
    dplyr::union(b94_dx) %>%
    compute_new(index="person_id")
  
  cohort %>% 
    left_join(pasc_dx, by="person_id") %>%
    mutate(pasc_flag=case_when(
      is.na(pasc_flag)~0,
      TRUE~pasc_flag
    )) %>%
    compute_new(index="person_id")
  
  
}


require_follow_up_code<-function(cohort, cond_codeset=load_codeset("cluster_master_pasc")){
  cond_visit_tbl<-cdm_tbl("condition_occurrence") %>% 
    inner_join(cdm_tbl("visit_occurrence") %>% dplyr::select(visit_occurrence_id, visit_start_date), by="visit_occurrence_id")
  
  cohort_cond_occurrence<-cohort %>% 
    inner_join(cond_visit_tbl, by="person_id") %>%
    inner_join(cond_codeset, by=c("condition_source_concept_id"="concept_id")) %>%
    filter(visit_start_date>=cohort_entry_date+days(28), visit_start_date<=cohort_entry_date+days(179)) %>%
    compute_new(index="person_id")
  
  code_occurrence_ids<-cohort_cond_occurrence %>% distinct(person_id) %>%
    mutate(code_occurrence_flag=1)
  
  cohort %>% 
    left_join(code_occurrence_ids, by="person_id") %>%
    mutate(code_occurrence_flag=case_when(
      is.na(code_occurrence_flag)~0,
      TRUE~code_occurrence_flag
    )) %>%
    filter(pasc_flag==1 | code_occurrence_flag==1) %>%
    compute_new(index="person_id")
  
}


get_cluster_visits<-function(cohort, cond_codeset=load_codeset("cluster_master_pasc")){
  cond_visit_tbl<-cdm_tbl("condition_occurrence") %>% 
    inner_join(cdm_tbl("visit_occurrence") %>% dplyr::select(visit_occurrence_id, visit_start_date), by="visit_occurrence_id")
  
  cohort_clust_occurrence<-cohort %>% 
    inner_join(cond_visit_tbl %>% dplyr::select(-site), by="person_id") %>%
    inner_join(cond_codeset, by=c("condition_source_concept_id"="concept_id")) %>%
    filter(visit_start_date>=cohort_entry_date+days(28), visit_start_date<=cohort_entry_date+days(179)) %>%
    group_by(person_id, cluster) %>%
    summarize(n_visits=n_distinct(visit_start_date)) %>%
    filter(n_visits>0) %>% 
    ungroup %>% 
    mutate(clust_flag=1) %>% 
    pivot_wider(id_cols="person_id", names_from="cluster", values_from="clust_flag", values_fill=0, names_prefix="visits_") %>% 
    compute_new(index="person_id")
  
  myList <- setNames(lapply(vector("list", ncol(cohort_clust_occurrence)), function(x) x <- 0), colnames(cohort_clust_occurrence))
  
  cohort %>% left_join(cohort_clust_occurrence, by="person_id") %>%
    replace_na(myList) %>%compute_new(index="person_id")
  
}




#' Add a site identifier, splitting Nemours by region
#'
#' Given a tbl containing `person_id`s, add a `site` column indicating the home
#' site for that person, based on the location of that person's home site as
#' specified in `site_tbl`. For patients whose home site is Nemours, further
#' indicate whether their care is based in Delaware or Florida, based on their
#' home `care_site` as specified by `site_tbl`.
#'
#' @param cohort The tbl containing the persons of interest.
#' @param site_tbl A tbl containing at least `person_id`, `care_site_id`, and
#'   `site` columns to place the patient.
#' @return The cohort tbl with a `site` column as described.  Any existing
#'   `site` column is removed.
#' @md
add_site_split <- function(cohort, site_tbl = add_site(cdm_tbl('person'))) {
  sites <- site_tbl %>% semi_join(cohort, by = 'person_id') %>%
    dplyr::select(person_id, care_site_id, site) %>%
    left_join(dplyr::select(cdm_tbl('care_site', db = site_tbl$src), care_site_id,
                     location_id), by = 'care_site_id') %>%
    left_join(dplyr::select(cdm_tbl('location', db = site_tbl$src),
                     location_id, state), by = 'location_id') %>%
    mutate(site = case_when(site != 'nemours' ~ site,
                            state == 'FL' ~ 'nemours_fl',
                            TRUE ~ 'nemours_de')) %>%
    dplyr::select(person_id, site)
  
  if (any(tbl_vars(cohort) == 'site')) cohort <- dplyr::select(cohort, -site)
  left_join(cohort, sites, by = 'person_id')
}


get_util<-function(cohort,
                   visit_tbl=cdm_tbl("visit_occurrence") %>% dplyr::select(visit_start_date, person_id)
){
  cohort_util<-cohort %>% 
    inner_join(visit_tbl, by="person_id") %>%
    filter(visit_start_date<cohort_entry_date, visit_start_date>cohort_entry_date-months(18)) %>%
    group_by(person_id) %>%
    summarize(n_visits=n_distinct(visit_start_date)) %>%
    mutate(visits_per_month=n_visits/18) %>%
    compute_new(index="person_id")
  
  cohort %>%
    left_join(cohort_util, by="person_id")
  
  
  
}





#' Function to find any ICU event in relation to an observation_date
#' @param adt_tbl table with adt_occurrences, defaulting to the cdm table adt_occurrence
#' @param cohort_tbl table with all person_ids for cohort, including their observation_date
#' @return table with NICU, PICU, CICU occurrences of any kind along with adt_type_concept_id and adt_date
find_icu <- function(adt_tbl = cdm_tbl('adt_occurrence'),
                     cohort_tbl) {
  cohort_tbl %>% dplyr::select(person_id, observation_date) %>%
    inner_join(adt_tbl, by = 'person_id') %>%
    filter(service_concept_id %in% c(2000000079L,2000000080L,2000000078L)) %>%
    dplyr::select(person_id, adt_date, observation_date, adt_type_concept_id) 
}

#' Function to find hospitalizations in relation to observation_date
#' @param visit_tbl table with visit_occurrences, defaulting to the cdm table visit_occurrence
#' @param cohort_tbl table with person_ids for cohort, including their observation_date
#' @param hosp_visit_types a list of visit_concept_ids to classify as hospitalizations
#' @return table with hospitalizations for the cohort at any time along with visit_start_date and visit_concept_id
find_hosp <- function(visit_tbl = cdm_tbl('visit_occurrence'),
                      cohort_tbl,
                      hosp_visit_types = c(9201L,
                                           2000000048L,
                                           2000000088L)) {
  cohort_tbl %>% dplyr::select(person_id, observation_date) %>%
    inner_join(visit_tbl, by = 'person_id') %>%
    filter(visit_concept_id %in% !!hosp_visit_types) %>%
    dplyr::select(person_id, visit_start_date, observation_date, visit_concept_id)
}

make_icu_flag<-function(cohort_tbl, days_min=-7, days_max=13){
  cohort_icu<-cohort_tbl %>%
    find_icu(cohort_tbl=.) %>%
    filter(adt_date>=observation_date+days(!!days_min), adt_date<observation_date+days(!!days_max)) %>%
    mutate(icu_flag=1) %>%
    distinct(person_id, icu_flag) %>%
    compute_new(index="person_id")
  
  cohort_tbl %>%
    left_join(cohort_icu, by="person_id") %>%
    mutate(icu_flag=case_when(is.na(icu_flag)~0,
                              TRUE~icu_flag)) %>%
    compute_new(index="person_id") %>%
    return()
}

make_hosp_flag<-function(cohort_tbl, days_min=-7, days_max=13){
  cohort_hosp<-cohort_tbl %>%
    find_hosp(cohort_tbl=.) %>%
    filter(visit_start_date>=observation_date+days(!!days_min), visit_start_date<observation_date+days(!!days_max)) %>%
    mutate(hosp_flag=1) %>%
    distinct(person_id, hosp_flag) %>%
    compute_new(index="person_id")
  
  cohort_tbl %>%
    left_join(cohort_hosp, by="person_id") %>%
    mutate(hosp_flag=case_when(is.na(hosp_flag)~0,
                              TRUE~hosp_flag)) %>%
    compute_new(index="person_id") %>%
    return()
}




require_post_acute_dxs<-function(cohort){
  keep_ids<-cohort %>% 
    inner_join(cdm_tbl('condition_occurrence'), by='person_id') %>%
    filter(condition_start_date>cohort_entry_date+28L, condition_start_date<cohort_entry_date+180L) %>%
    group_by(person_id) %>%
    summarize(min_date=min(condition_start_date), max_date=max(condition_start_date)) %>% 
    mutate(date_diff=max_date-min_date) %>%
    filter(date_diff>28) %>%
    distinct(person_id) %>%
    compute_new(indexes=list(c('person_id')))
  
  cohort %>%
    inner_join(keep_ids, by='person_id') %>%
    compute_new(indexes=list(c('person_id'))) %>%
    return()
}



