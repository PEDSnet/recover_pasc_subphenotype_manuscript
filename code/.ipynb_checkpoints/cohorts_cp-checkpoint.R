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

#' Function to classify observation types in an observation table
#' @param obs_tbl table structured as the observation table
#' @param cohort table with person_ids
#' @param obs_ids list of observation_concept_ids
#' @param val_ids list of value_as_concept_ids
#' @param rule_name string to classify observation as
#' @param date_bool boolean indicating whether observation date can be relied on for the occurrence
#' @return a table with the columns in obs_tbl + the columns:
#'          `rule` name of the rule
#'          `date_known` whether or not can use the observation_date as the date of event
#'          
classify_observation <- function(obs_tbl=cdm_tbl('observation_derivation_recover'), 
                                 cohort_tbl=cdm_tbl('person'),
                                 obs_ids, val_ids, rule_name, date_bool, fact_concl){
  
cohort_tbl %>% distinct(person_id) %>%
    inner_join(obs_tbl, by = 'person_id')%>%
    filter(observation_concept_id %in% !!obs_ids &
             value_as_concept_id %in% !!val_ids) %>%
    mutate(rule=rule_name,
           date_known=date_bool,
           fact_infection=fact_concl)
}

#' Function to determine serology positives, from the treescan code (https://reslnpedsnops01.research.chop.edu/bitbucket/projects/PASC/repos/treescan/browse/code/cohorts.R#137-199)
#' @param obs_der_tbl table in the format of observation_derivation_recover
get_serology_positives<-function(obs_der_tbl=cdm_tbl("observation_derivation_recover") %>% filter(observation_date<as.Date("2022-05-01"))){
  ## Positive/Negative Serology Results
  serology_tbl<-obs_der_tbl %>% 
    filter(observation_concept_id==2000001528L)%>%
    mutate(test_result=case_when(
      value_as_concept_id %in% c(9191L, 2000001526L)~"positive",
      value_as_concept_id==9189L~"negative",
      TRUE~"unknown"
    )) %>%
    filter(test_result!="unknown") %>%
    compute_new(indices=c("person_id", "visit_occurrence_id", "observation_source_concept_id"))
  
  
  measurement_tbl<-cdm_tbl('measurement_labs') %>%
    filter(measurement_date >= as.Date('2020-03-01')) %>%
    mutate(vsv_lower = tolower(value_source_value), unit_lower=tolower(unit_source_value))
  
  serology_meas<-serology_tbl %>% 
    select(-value_source_value, -unit_source_value, -unit_concept_id) %>%
    inner_join(measurement_tbl %>% 
                 select(measurement_id, measurement_date, vsv_lower, measurement_source_value, measurement_concept_id),
               by=c("observation_source_concept_id"="measurement_id")) %>%
    compute_new(indices=c("person_id", "visit_occurrence_id", "observation_source_concept_id"))
  
  serology<-serology_meas %>% left_join(select(cdm_tbl('person'),person_id,birth_date),by='person_id') %>%
    mutate(measurement_age = (measurement_date - birth_date)/365.25) %>%
    filter(measurement_age >= 0, measurement_age < 21) %>%
    mutate(age_range = case_when(measurement_age < 0.5 ~ "<6mo",
                                 measurement_age >= 0.5 & measurement_age < 5 ~ "6mo-5",
                                 measurement_age >= 5 & measurement_age < 12 ~ "5-11",
                                 measurement_age >= 12 & measurement_age < 16 ~ "12-15",
                                 measurement_age >= 16 ~ "16+"))%>% 
    mutate(sv_lower = tolower(measurement_source_value)) %>%
    mutate(measurement_concept = case_when(str_detect(sv_lower, 'nucleocapsid') ~ 2000001501,
                                           str_detect(sv_lower, 'spike') ~ 2000001502,
                                           TRUE ~ measurement_concept_id))%>% 
    mutate(serology_type = case_when(measurement_concept == 2000001501L ~ 'IgG N protein',
                                     measurement_concept %in% c(723474L, 706177L, 706181L) ~ 'IgG undifferentiated',
                                     measurement_concept %in% c(723475L, 706178L) ~ 'IgM',
                                     measurement_concept == 723473L ~ 'IgA',
                                     measurement_concept %in% c(586515L,586522L,723480L) ~ 'Ab undifferentiated',
                                     measurement_concept %in% c(36032309L, 36031956L, 36031969L) ~ 'stimulated gamma interferon release',
                                     measurement_concept %in% c(2000001502L, 1619027L, 36031734L) ~ 'S protein or RBD')) %>%
    filter(serology_type != 'stimulated gamma interferon release')
  
  serology_filtered<-serology %>%
    mutate(include_flag=case_when(
      serology_type %in% c('IgG N protein', 'IgM')~1,
      age_range=='<6mo'~1,
      age_range=='6mo-5' & measurement_date<as.Date('2022-06-18')~1,
      age_range=='16+' & measurement_date<as.Date('2020-12-12')~1,
      age_range=='12-15' & measurement_date<as.Date('2021-05-12') ~1,
      age_range=='5-11' & measurement_date<as.Date('2021-11-02')~1,
      TRUE~0
    )) %>%
    filter(include_flag==1) %>% distinct(observation_id) %>% compute_new(index="observation_id")
  
  
  serology_tbl %>% 
    filter(test_result=="positive") %>%
    select(-test_result) %>% 
    inner_join(serology_filtered, by="observation_id") %>%
    compute_new(indices=c("person_id", "visit_occurrence_id", "observation_source_concept_id")) %>%
    return()
}

#' Function to classify patient's SARS-CoV-2 infection according to all of the rules they meet,
#'         and find the date of their first infection during which they meet age criteria.
#'         IF exact date of infection is not known and patient is less than `date_back` days, 
#'         birth date is used as date of infection
#' @param rules_tbl table with a `rule` column indicating the CP rule that the observation meets
#' @param date_back number of days to count back for cohort entry date if date is not known
#' @return table with one row per patient with the highest rule on the first date met,
#'          along with cohort entry date details
apply_cp_hier <- function(rules_tbl, date_back){
  rules_tbl %>%
    mutate(rule_num=case_when(rule=='positive_viral_pcr'~1L,
                              rule=='positive_viral_antigen'~2L,
                              rule=='positive_nucleocapsid_serology'~3L,
                              rule=='specific_covid19_dx'~4L,
                              rule=='pasc_misc'~5L,
                              rule=='complication_covid19_dx'~6L,
                              rule=='history_covid19_dx'~7L,
                              rule=='exposure_covid19_dx'~8L,
                              rule=='negative_viral_test'~9L)) %>%
    mutate(ce_date_min=case_when(date_known~observation_date,
                             TRUE~observation_date-days(date_back)))%>%
    inner_join(select(cdm_tbl('person'), c(person_id, birth_date)), by = 'person_id')%>%
    mutate(ce_date=case_when(ce_date_min<birth_date~birth_date, 
                             TRUE~ce_date_min))%>%
    filter(ce_date>='2020-03-01')%>%
    mutate(ce_quarter=case_when(ce_date<='2020-03-31'~'2020 Q1',
                                ce_date<='2020-06-30'~'2020 Q2',
                                ce_date<='2020-09-30'~'2020 Q3',
                                ce_date<='2020-12-31'~'2020 Q4',
                                ce_date<='2021-03-31'~'2021 Q1',
                                ce_date<='2021-06-30'~'2021 Q2',
                                ce_date<='2021-09-30'~'2021 Q3',
                                ce_date<='2021-12-31'~'2021 Q4',
                                ce_date<='2022-03-31'~'2022 Q1',
                                ce_date<='2022-06-30'~'2022 Q2',
                                ce_date<='2022-09-30'~'2022 Q3',
                                ce_date<='2022-12-31'~'2022 Q4'),
           ce_year=str_remove(ce_quarter, " [Q][0-9]"))%>%
    select(-c(birth_date, ce_date_min))%>%
    limit_ce_age(., age_upper_years=21)%>%
    group_by(person_id) %>%
    filter(rule_num==min(rule_num)) %>%
    filter(ce_date==min(ce_date)) %>%
    ungroup() %>%
    select(person_id, site, rule, fact_infection, date_known, ce_date, rule_num, birth_date, ce_age_days, ce_age_years, ce_age_cat, ce_quarter, ce_year) %>%
    distinct()
}

#' Function to limit age on cohort entry date below a specified value
#' @param ce_tbl table with at least the columns `person_id`, `ce_date`
#' @param age_upper_years age, in years, of the maximum cohort entry years
#' @return table with the columns in the original `ce_tbl` + the columns `ce_age_days` and `ce_age_years`, limited to only patients under the age specified in the `age_upper_years` parameter
limit_ce_age <- function(ce_tbl,
                         age_upper_years){
  ce_tbl %>%
    select(person_id, ce_date) %>%
    distinct() %>%
    inner_join(select(cdm_tbl('person'), c(person_id, birth_date)), by = 'person_id') %>%
    mutate(ce_age_days=as.integer(as.Date(ce_date)-as.Date(birth_date)),
           ce_age_years=ce_age_days/365.25) %>%
    filter(ce_age_years>-28, ce_age_years<age_upper_years)%>%
    mutate(ce_age_cat=case_when(ce_age_years<1~"<1",
                                ce_age_years<5~"1-4",
                                ce_age_years<10~"5-9",
                                ce_age_years<16~"10-15",
                                TRUE~"16-21"))%>%
    inner_join(ce_tbl, by = c('person_id', 'ce_date'))
}

#' Function to count number of visits 
#'    before a cohort entry date,
#'    after a cohort entry date,
#'    in a specified time frame around a cohort entry date
#' @param ce_tbl table with the cols `person_id` and `ce_date`
#' @param visit_tbl table with visit_occurrences
#' @param days_start number of days after `ce_date` to start counting visits
#' @param days_end number of days after `ce_date` to finish counting visits
#' @return table with a count of the number of visits patient had within the `days_start` and `days_end` time frame
count_visits <- function(ce_tbl,
                         visit_tbl=cdm_tbl('visit_occurrence'),
                         days_start,
                         days_end){
  cohort_visits <- ce_tbl %>% select(ce_date, person_id) %>%
    inner_join(visit_tbl, by = 'person_id') 
  
  visits_pre_ce <- cohort_visits %>%
    filter(visit_start_date<ce_date) %>%
    group_by(person_id, ce_date) %>%
    summarise(num_visits_pre=as.integer(n_distinct(visit_occurrence_id))) %>%
    ungroup()
  
  visits_post_ce <- cohort_visits %>%
    filter(visit_start_date>=ce_date) %>%
    group_by(person_id, ce_date) %>%
    summarise(num_visits_post=as.integer(n_distinct(visit_occurrence_id))) %>%
    ungroup()
  
  visits_within <- cohort_visits %>%
    filter(visit_start_date>=as.Date(ce_date)+days(days_start) &
             visit_start_date<=as.Date(ce_date)+days(days_end)) %>%
    mutate(in_person_flag=case_when(visit_concept_id %in% c(9201L, 9202L, 9203L, 42898160L, 44814710L,
                                                            2000000048L, 2000000088L, 581399L)~1L,
                                    TRUE~0L),
           admitted_flag=case_when(visit_concept_id %in% c(9201L, 2000000048L)~1L,
                                   TRUE~0L))%>%
    group_by(person_id) %>%
    summarise(num_visits_window=as.integer(n_distinct(visit_occurrence_id)),
              num_visits_window_inperson=as.integer(sum(in_person_flag)),
              num_visits_window_ip=as.integer(sum(admitted_flag)))%>%
    ungroup()
  
  visits_pre_ce %>%
    full_join(visits_post_ce, by = c('person_id', 'ce_date')) %>%
    full_join(visits_within, by = 'person_id')%>%
    mutate(across(starts_with("num_"), ~coalesce(.x, 0L)))
}

#' @md find min date and number of distinct dates
#' 
#' @param fact_tbl the tbl that contains 
#' the facts of interest
#' @param by the variables to group by.
#' Defaults to  c(`person_id`)
#' @param date_var the name of the date 
#' variable in the `fact_tbl`. Should
#' not be quoted.
#' 
#' @return the grouped variables and the min date
#' 

find_min_flag <- function(fact_tbl,
                          date_var = occ_date,
                          by = c('person_id')){
  
  
  fact_tbl %>%
    group_by(!!!syms(by)) %>%
    summarise(min_date=
                min({{date_var}}),
              n_dates=n_distinct({{date_var}})) %>%
    mutate(n_dates=as.integer(n_dates))%>%
    ungroup() %>%
    compute_new(temporary=TRUE,
                indexes=list('person_id'))
}
#' Function to find hits for a condition codeset in the condition_source_concept_id field
#' @param cohort table with at least a `person_id` column
#' @param condition_tbl table with condition_occurrences with a `condition_source_concept_id` field
#' @param condition_codes table with at least a `concept_id` field with the concepts of interest
#' @return a row for each occurrence in the `condition_tbl` for the `cohort` of any of the codes in `condition_codes`, 
#'              retaining the columns in the codeset (condition_codes)
find_condition_source_occurrence <- function(cohort,
                                             condition_tbl,
                                             condition_codes) {
  cohort%>% select(person_id) %>% distinct() %>%
    inner_join(condition_tbl, by = 'person_id') %>%
    inner_join(condition_codes, by = c('condition_source_concept_id'='concept_id'))
}

#' Function to find condition_occurrences within specified time frames
#' @param condition_tbl table in the format of condition_occurrence with at least condition_start_date column
#' @param rel_date_tbl table with person_id, site, and columns ending in _start_date and _end_date
#' @return table with condition_occurrences within the start_ and end_ dates in the rel_date_tbl
restrict_condition_occs <- function(condition_tbl,
                                    rel_date_tbl) {
  
  rel_date_tbl %>%
    pivot_longer(cols=ends_with("_date"),
                 names_to=c("phase", "type"),
                 values_to="date",
                 names_pattern = "(.*)_(.*_date)") %>%
    filter(!is.na(phase)) %>%
    pivot_wider(names_from=type,
                values_from = date) %>%
    inner_join(condition_tbl, by = c('person_id', 'site')) %>%
    filter(condition_start_date >= start_date &
             condition_start_date <= end_date)
}
#' Function to find visit info for condition_occurrences
#' @param condition_tbl table with condition_occurrences
#' @param visit_tbl table with visit_occurrences
#' @return table with condition_occurrences + additional columns from visit_occurrence table 
find_condition_visits <- function(condition_tbl,
                                  visit_tbl){
  condition_tbl %>%
    inner_join(select(visit_tbl, c(visit_occurrence_id, visit_concept_id)), by = 'visit_occurrence_id')
}

#' Function to classify condition occurrences grouped by cluster classifications
#' @param ce_tbl table with at least person_id and ce_date
#' @param condition_tbl table with at least person_id, condition_start_date, cluster
#' @return table with person_id, cluster, phase, and within those phase combinations:
#'           min_dx_date: first condition_start_date in the phase
#'           max_dx_date: final condition_start_date in the phase
#'           num_dx: number of condition_start_date for any of the diagnoses within the phase
#'           max_days_diff: maximum number of days between diagnoses during the phase
characterize_clusters <- function(ce_tbl,
                                  condition_tbl){
  ce_tbl %>% select(person_id, ce_date) %>% distinct() %>%
    inner_join(condition_tbl, by = 'person_id') %>%
    mutate(days_to_dx=as.integer(as.Date(condition_start_date)-as.Date(ce_date)),
           phase=case_when(days_to_dx< -28~'prior',
                           days_to_dx<= 27L~'acute',
                           days_to_dx<= 179~'post_acute',
                           days_to_dx<= 657~'chronic',
                           TRUE~'post_chronic'))%>%
    group_by(person_id, cluster, phase)%>%
    summarise(min_dx_date=min(condition_start_date, na.rm=T),
              max_dx_date=max(condition_start_date, na.rm=T),
              num_dx=as.integer(n_distinct(condition_start_date)))%>%
    ungroup()%>%
    mutate(max_days_diff=as.integer(as.Date(max_dx_date)-as.Date(min_dx_date)))
}

#' Function to find follow up window dates for a cohort
#' @param ce_table table with at least person_id, site, ce_date for cohort
#' @param fu_day_list list of integer values to search around ce_date
#' @param fu_name_list list, matching number of elements in `fu_day_list`, for events occurring at each of the dates in `fu_day_list`
#' @return table with person_id, site, ce_date, and a column named for each of the elements in `fu_name_list` with values computed around ce_date for the elements of `fu_day_list`
find_fu_windows <- function(ce_table,
                            fu_day_list,
                            fu_name_list) {
  ce_table_collect <- ce_table%>%select(person_id, site, ce_date)%>%distinct()%>%collect()
  rslt<- NA
  for(i in 1:length(fu_day_list)) {
    ce_table_new <- ce_table_collect %>%
      mutate(event_name = fu_name_list[[i]],
             event_date=ce_date+fu_day_list[[i]])
    if(all(is.na(rslt))) rslt <- ce_table_new else rslt <- dplyr::union_all(rslt, ce_table_new)
  }
  rslt %>%
    pivot_wider(id_cols=c(person_id, site, ce_date),
                names_from=event_name,
                values_from=event_date)
}

#' Function to summarize cluster codes per person, based on whether patient had 
#'              a certain number of diagnoses
#'              with specified days separation
#'              during specified phase
#' @param cluster_summary_tbl table with at least the cols: 
#'                            `person_id`, `cluster`, `phase`, `min_dx_date`, `num_dx`, `max_days_diff`
#' @param day_diff minimum number of days required between diagnoses
#' @param min_n_dx minimum number of diagnoses required
#' @param phase_filt vector containing strings for acceptable phase in the `phase` column
#' @return tbl with the cols:
#'           person_id
#'           num_cluster_dx: number of diagnosis dx days for any of the clusters for which patient met criteria
#'           num_clusters: number of distinct clusters for which patient met the criteria during any of the phases specified
#'           max_days_diff: maximum number of days between diagnoses within the cluster
#'           min_dx_date: minimum diagnosis date for one of the cluster diagnoses within the phase
#'           cluster_dx_grx: indicator for whether or not greater than the specified number of cluster diagnoses with specified separation
rollup_cluster <- function(cluster_summary_tbl,
                           day_diff,
                           min_n_dx,
                           phase_filt){
  cluster_summary_tbl %>%
    filter(phase %in% !!phase_filt) %>%
    mutate(cluster_dx_grx=case_when(num_dx>=min_n_dx&
             max_days_diff>=day_diff~1L,
           TRUE~0L)) %>%
    group_by(person_id) %>%
    summarise(num_cluster_dx=as.integer(sum(num_dx)),
              num_clusters=as.integer(n_distinct(cluster)),
              cluster_dx_grx=max(cluster_dx_grx),
              max_days_diff=max(max_days_diff),
              min_dx_date=min(min_dx_date))%>%
    mutate(cluster_dx_grx=case_when(cluster_dx_grx==1L~TRUE,
                                    TRUE~FALSE))%>%
    ungroup()
}

#' Function to assign conclusivity of PASC
#' @param flag_tbl table with PASC-related flags
#' @return table with the original columns in `flag_tbl` + a `pasc_level` column indicating whether PASC is
#'                                             conclusive, probable, possible, or no evidence
assign_pasc_level <- function(flag_tbl){
  flag_tbl %>%
    mutate(pasc_level=case_when(num_pasc_dx_date>=2L|
                                  num_misc_dx_date>=2L|
                                  num_pv_nonspec_dx_date>=2L~'conclusive',
                                num_pasc_dx_date==1L|
                                  num_misc_dx_date==1L|
                                  (rule%in%c('specific_covid19_dx', 
                                             'complication_covid19_dx', 
                                             'history_covid19_dx')&
                                     cluster_dx_grx)|
                                  (rule%in%c('positive_viral_pcr',
                                             'positive_viral_antigen',
                                             'positive_nucleocapsid_serology')&
                                     cluster_dx_grx)|
                                  (rule=='positive_viral_pcr'&num_pv_nonspec_dx_date>=1L)~'probable',
                                fact_infection %in% c('conclusive', 'probable')&
                                  (cluster_dx_grx|num_pv_nonspec_dx_date>=1L)~'possible',
                                TRUE~'no_evidence'))
}

#' Function to find newly diagnosed conditions not present during washout
#' @param condition_tbl table with condition_occurrences for diagnoses of interest
#' @param cluster_list vector containing clusters to examine in relation to washout period
#' @param index_tbl table with at least a `person_id` and `ce_date` column
#' @param washout_start_date date on which to start the washout period
#' @return table with conditions that should **not** be washed out, with any of the cols in the original `condition_tbl`
apply_washout_clusters <- function(condition_tbl,
                                   cluster_list,
                                   index_tbl,
                          washout_start_date){
  # conditions present during washout period
  condition_tbl_lim <- condition_tbl %>%
    inner_join(select(index_tbl, c(person_id, ce_date)), by = 'person_id')%>%
    filter(condition_start_date>=washout_start_date&condition_start_date<ce_date&cluster%in%!!cluster_list)
  # remove those clusters from those persons
  person_conditions <- condition_tbl %>% distinct(person_id, cluster) %>%
    anti_join(condition_tbl_lim, by =c('person_id', 'cluster'))
  # bring back the conditions for the ones that should count
  person_conditions %>%
    inner_join(condition_tbl, by = c('person_id', 'cluster'))
}

#' Function to find newly diagnosed conditions not present during washout
#' @param condition_tbl table with condition_occurrences for diagnoses of interest
#' @param cluster_list vector containing clusters to examine in relation to washout period
#' @param index_tbl table with at least a `person_id` and `ce_date` column
#' @param washout_start_date date on which to start the washout period
#' @return table with conditions that should **not** be washed out, with any of the cols in the original `condition_tbl`
apply_washout_clusters_rel <- function(condition_tbl,
                                   cluster_list,
                                   index_tbl,
                                   washout_months_back){
  # conditions present during washout period
  condition_tbl_lim <- condition_tbl %>%
    inner_join(select(index_tbl, c(person_id, ce_date)), by = 'person_id')%>%
    filter(condition_start_date>=ce_date-months(washout_months_back) &condition_start_date<ce_date &cluster%in%!!cluster_list)
  # remove those clusters from those persons
  person_conditions <- condition_tbl %>% distinct(person_id, cluster) %>%
    anti_join(condition_tbl_lim, by =c('person_id', 'cluster'))
  # bring back the conditions for the ones that should count
  person_conditions %>%
    inner_join(condition_tbl, by = c('person_id', 'cluster'))
}


#' Function to find follow up window dates for a cohort
#' @param ce_table table with at least person_id, site, ce_date for cohort
#' @param fu_day_list list of integer values to search around ce_date
#' @param fu_name_list list, matching number of elements in `fu_day_list`, for events occurring at each of the dates in `fu_day_list`
#' @return table with person_id, site, ce_date, and a column named for each of the elements in `fu_name_list` with values computed around ce_date for the elements of `fu_day_list`
find_fu_windows <- function(ce_table,
                            fu_day_list,
                            fu_name_list) {
  ce_table_collect <- ce_table%>%select(person_id, site, ce_date)%>%distinct()%>%collect()
  rslt<- NA
  for(i in 1:length(fu_day_list)) {
    ce_table_new <- ce_table_collect %>%
      mutate(event_name = fu_name_list[[i]],
             event_date=ce_date+fu_day_list[[i]])
    if(all(is.na(rslt))) rslt <- ce_table_new else rslt <- dplyr::union_all(rslt, ce_table_new)
  }
  rslt %>%
    pivot_wider(id_cols=c(person_id, site, ce_date),
                names_from=event_name,
                values_from=event_date)
  
  
}

#' Function to find visit_occurrences within specified time frames
#' @param visit_tbl table in the format of visit_occurrence with at least visit_start_date column
#' @param rel_date_tbl table with person_id, site, and columns ending in _start_date and _end_date
#' @param visit_types vector of eligible visit_concept_ids
#' @return table with visit_occurrences within the start_ and end_ dates in the rel_date_tbl
count_phase_occs <- function(visit_tbl,
                                    rel_date_tbl,
                             visit_types) {
  visits_relevant<-visit_tbl%>%filter(visit_concept_id%in%!!visit_types)
  
  rel_date_tbl %>%
    pivot_longer(cols=ends_with("_date"),
                 names_to=c("phase", "type"),
                 values_to="date",
                 names_pattern = "(.*)_(.*_date)") %>%
    filter(!is.na(phase)) %>%
    pivot_wider(names_from=type,
                values_from = date) %>%
    inner_join(visits_relevant, by = c('person_id', 'site')) %>%
    filter(visit_start_date >= start_date &
             visit_end_date <= end_date)%>%
    group_by(person_id, phase)%>%
    summarise(num_phase_visits=as.integer(n_distinct(visit_start_date)))%>%
    ungroup()
}