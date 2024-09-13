



#' Find in-person visits near test date
#'
#' Given a cohort of patients with test results (see [get_tested()]), retrieve
#' all in-person visits..
#'
#' An outpatient visit is considerd to occur on the day of `visit_start_date`.
#' For inpatient visits, if `visit_end_date` is not present, it is set to the
#' current date (i.e. we presume the visit is ongoing).  The visit is included
#' if it ended no earlier than 7 days prior to the test date.
#'
#' @param cohort The cohort of patients for whom to find visits.  In addition to
#'   the `person_id`, this should have the test date in `observation_date`.
#' @param visit_tbl The visit data
#' @param adt_tbl The adt occurrence table
#'
#' @return A tbl of visit date, with the end date for inpatient visits
#'   potentially set as described above, a `visit_offset` column noting the
#'   days between the test date and the visit_start_date, an `icu_yn` flag if the 
#'   patient was admitted to the icu, the `icu_admit_date` and an `icu_lag` column noting the
#'   number of days the after the visit start date the patient was admitted to the icu.
#' @md
get_visits <- function(cohort,
                       visit_tbl = cdm_tbl('visit_occurrence'),
                       adt_tbl = cdm_tbl('adt_occurrence')
                       ) {
  
  adt_vis <- cohort%>% ## question: Do we want to filter and restrict to inpatient visits? ##EB: Thinking we might be okay with a left join below? This is 
              inner_join(adt_tbl,by=c('person_id', 'site'))%>%
                filter(service_concept_id %in% c(2000000079L,2000000080L,2000000078L))%>%
                group_by(person_id,site, visit_occurrence_id)%>%
                summarise(icu_admit_date=min(adt_date))%>%
                distinct()%>% ungroup() %>%
                mutate(icu_yn='Y')
  
  cohort_vis <- visit_tbl %>% inner_join(cohort, by = c('person_id', 'site'))%>%
                left_join(adt_vis,by=c('visit_occurrence_id','person_id', 'site'))
  
  op_vis <- cohort_vis %>%
    filter(visit_concept_id %in% c(9202L, 9203L,581399L,44814711L) &
             visit_start_date >= observation_date - 7L & ## question: any date before the covid positive test?
             visit_start_date <= observation_date + 13L) %>%
    mutate(visit_offset = visit_start_date - observation_date) %>%
    mutate(visit_type = 
             case_when(visit_concept_id %in% c(9202L) ~ 'op',
                       visit_concept_id %in% c(9203L) ~ 'ed',
                       visit_concept_id %in% c(581399L) ~ 'telehealth',
                       visit_concept_id %in% c(44814711L) ~ 'oa'))

  ip_vis <- cohort_vis %>%
    filter(visit_concept_id %in% c(9201L, 2000000048L, 2000000088L)) %>%
    mutate(visit_offset = visit_start_date - observation_date,
           visit_end_date = case_when(!is.na(visit_end_date) ~ visit_end_date,
                                      TRUE ~ sql('current_date'))) %>%
    filter(visit_start_date >= observation_date - 7L & visit_start_date <= observation_date + 13L) %>%
    mutate(visit_type =
             case_when(visit_concept_id %in% c(9201L) ~ 'ip',
                       visit_concept_id %in% c(2000000048L) ~ 'ed/ip',
                       visit_concept_id %in% c(2000000088L) ~ 'obs'),
           los=visit_end_date-visit_start_date+1)
  
  combined <- 
    dplyr::union(ip_vis, op_vis) %>%
    dplyr::select(person_id, site, test_result, observation_date,visit_occurrence_id, visit_concept_id,
           visit_start_date,visit_start_datetime, 
           visit_end_date, visit_offset, 
           visit_type,icu_yn,
           icu_admit_date,los) %>%
            mutate(icu_admit_lag=icu_admit_date-visit_start_date,
                   icu_test_lag=icu_admit_date-observation_date)%>%
            distinct()
  
  neither <- 
    cohort %>%
    anti_join(combined,
              by=c('person_id',
                   'observation_date', 'site')) %>%
    mutate(visit_type = 'none')
  
  dplyr::union(
    combined,
    neither
  )
    
}


#' computes flag 1 for patients
#' 
#' @param visit_cohort the cohort with visit information (`visit_occurrence_id`)
#' @param flag_1_codeset the codeset for flag 1
#' @param condition_tbl the cdm_tbl that has diagnosis codes for flag 1
#' 
#' @return fields from the condition_tbl with a flag variable indicating `flag_1`

compute_flag1 <- function(visit_cohort,
                          flag_1_codeset=load_codeset('flag_1_codeset'),
                          condition_tbl=cdm_tbl('condition_occurrence')) {
  visits<-visit_cohort %>% 
    dplyr::select(
    person_id, site,
    observation_date,
    visit_occurrence_id,
    visit_start_date
    ) %>% distinct() 
  
  flag_1_non_covid<-
    visits %>%
    inner_join(
      dplyr::select(
        condition_tbl,
        person_id, site,
        visit_occurrence_id,
        condition_start_date,
        condition_type_concept_id,
        condition_concept_id,
        condition_occurrence_id
      ), by = c('person_id', 'site', 'visit_occurrence_id')
    ) %>% inner_join(
      flag_1_codeset%>%filter(!codeset=='covid_dx'),
      by=c('condition_concept_id'='concept_id')
    ) %>% mutate(
      qualifier='non_covid_condition'
    )
  
  flag_1_covid<-
    visits %>%
    inner_join(
      dplyr::select(
        condition_tbl,
        person_id, site,
        visit_occurrence_id,
        condition_start_date,
        condition_type_concept_id,
        condition_concept_id,
        condition_occurrence_id
      ), by = c('person_id', 'site', 'visit_occurrence_id')
    ) %>% inner_join(
      flag_1_codeset%>%filter(codeset=='covid_dx'),
      by=c('condition_concept_id'='concept_id')
    )%>%filter(visit_start_date>observation_date &
              visit_start_date <= observation_date +13L)%>%
    mutate(
      qualifier='covid_condition'
    )
 
  dplyr::union(
    flag_1_non_covid,
    flag_1_covid)%>%
    mutate(flag=1L)
  
}


#' computes flag 2 for patients
#' 
#' @param visit_cohort the cohort with visit information (`visit_occurrence_id`)
#' @param flag_2_conditions the codeset for flag 2 (condiitions)
## @param flag_2_procs the codeset for flag 2 (procedures)
#' @param flag_2_drugs the codeset for flag 2 (drugs
#' @param condition_tbl the cdm_tbl that has diagnosis codes for flag 2
## @param procedure_tbl the cdm_tbl that contains procedure codes for flag 2
#' @param drug_tbl the cdm_tbl that contains drug codes for flag 2
#' 
#' @return metadata fields `flag_2`
#' 


compute_flag2 <- function(visit_cohort,
                          flag_2_conditions=load_codeset('flag_2_dx_codeset'),
                          #flag_2_procs=load_codeset('supp_ox_codeset'),
                          flag_2_drugs= load_codeset('iv_fluids_codeset',col_types = 'icccc'),
                          condition_tbl=cdm_tbl('condition_occurrence'),
                          #procedure_tbl=cdm_tbl('procedure_occurrence'),
                          drugs_tbl=cdm_tbl('drug_exposure')) {
  
  visits <- 
    visit_cohort %>%
    dplyr::select(
      person_id, site,
      observation_date,
      visit_occurrence_id,
      visit_type,
      visit_start_date,
      visit_start_datetime
    ) %>% distinct()
  
  conds <- 
    visits %>%
    inner_join(
      dplyr::select(
        condition_tbl,
        person_id, site,
        visit_occurrence_id,
        condition_concept_id
      ), by = c('person_id', 'site', 'visit_occurrence_id')
    ) %>% inner_join(
      flag_2_conditions,
      by=c('condition_concept_id'='concept_id')
    ) %>%
    dplyr::select(
      person_id, site,
      observation_date,
      visit_occurrence_id,
      condition_concept_id,
      codeset,
    ) %>% mutate(qualifier='condition') %>%
    rename(concept_id=condition_concept_id) %>%
    distinct()
  
  # procedures_mapped <- 
  #   visits %>%
  #   inner_join(
  #     dplyr::select(
  #       procedure_tbl,
  #       person_id,
  #       visit_occurrence_id,
  #       procedure_concept_id
  #     ), by = c('person_id','visit_occurrence_id')
  #   ) %>% inner_join(
  #     flag_2_procs,
  #     by=c('procedure_concept_id'='concept_id')
  #   )%>%
  #   dplyr::select(
  #     person_id,
  #     observation_date,
  #     visit_occurrence_id,
  #     procedure_concept_id,
  #     site,
  #     codeset
  #   ) %>% mutate(qualifier='procedure_mapped') %>%
  #   rename(concept_id=procedure_concept_id) %>%
  #   distinct()
  # 
  # procedures_unmapped <- 
  #   visits %>%
  #   inner_join(
  #     dplyr::select(
  #       procedure_tbl,
  #       person_id,
  #       visit_occurrence_id,
  #       procedure_concept_id,
  #       procedure_source_value
  #     ), by = c('person_id','visit_occurrence_id')
  #   )%>%
  #   filter(str_detect(procedure_source_value,'OXYGEN THERAPY')|str_detect(procedure_source_value,'OXYGEN TREATMENT'))%>%
  #   dplyr::select(
  #     person_id,
  #     observation_date,
  #     visit_occurrence_id,
  #     procedure_concept_id,
  #     site
  #   ) %>% mutate(codeset='supp_ox',
  #                qualifier='procedure_unnmapped') %>%
  #   rename(concept_id=procedure_concept_id) %>%
  #   distinct()
  
  iv_fluids <- 
    visits %>%
    filter(visit_type %in% c('ed','op','ed/ip'))%>%
    inner_join(
      dplyr::select(
        drugs_tbl,
        person_id, site,
        visit_occurrence_id,
        drug_concept_id,
        drug_exposure_start_date,
        drug_exposure_start_datetime
      ), by = c('person_id','site', 'visit_occurrence_id')
    ) %>% inner_join(
      flag_2_drugs,
      by=c('drug_concept_id'='concept_id')
    )%>%
    filter((visit_type %in% c('ed','ed/ip') & between(drug_exposure_start_datetime,visit_start_datetime,sql("(visit_start_datetime + interval '6 hour')")))|
             (visit_type=='op' & drug_exposure_start_date==visit_start_date)
             )%>%
    dplyr::select(
      person_id, site,
      observation_date,
      visit_occurrence_id,
      drug_concept_id,
      codeset
    ) %>% mutate(qualifier='drug') %>%
    rename(concept_id=drug_concept_id) %>%
    distinct()
  
  dplyr::union(
  #conds,procedures_mapped)%>%
  #dplyr::union(procedures_unmapped)%>%
  #dplyr::union(iv_fluids) %>%
   conds,
   iv_fluids)%>%
     mutate(flag=2L)
  
}


#' computes flag 3 for patients
#' 
#' @param visit_cohort the cohort with visit information (`visit_occurrence_id`)
#' @param flag_3_conditions the codeset for flag 3 (condiitions)
#' @param flag_3_procs the codeset for flag 3 (procedures)
#' @param flag_3_drugs the codeset for flag 3 (drugs)
#' @param flag_3_devices the codeset for flag 3 (devices)
#' @param condition_tbl the cdm_tbl that has diagnosis codes for flag 3
#' @param procedure_tbl the cdm_tbl that contains procedure information
#' @param drugs_tbl the cdm_tbl that contains drugs information
#' @param device_tbl the cdm_tbl that contains device information
#' @param death_tbl the cdm_tbl that contains death information
#' 
#' @return metadata fields `flag_3`
#' 

compute_flag3 <- function(visit_cohort,
                          flag_3_conditions=load_codeset('flag_3_dx_codeset'),
                          flag_3_procedures=load_codeset('mech_vent_px_codeset',col_types = 'iccccc'),
                          flag_3_devices=load_codeset('mech_vent_device_codeset',col_types = 'iccccc'),
                          flag_3_drugs=load_codeset('pressors_codeset'),
                          condition_tbl=cdm_tbl('condition_occurrence'),
                          procedure_tbl=cdm_tbl('procedure_occurrence'),
                          drugs_tbl=cdm_tbl('drug_exposure'),
                          device_tbl=cdm_tbl('device_exposure'),
                          death_tbl=cdm_tbl('death')
                          ) {
  
  visits <- 
    visit_cohort %>%
    dplyr::select(
      person_id, site,
      observation_date,
      visit_occurrence_id,
      icu_yn,
      visit_type,
      visit_start_date,
      visit_end_date,
      icu_test_lag
    ) %>% distinct()
  
  conds <- 
    visits %>%
    inner_join(
      dplyr::select(
        condition_tbl,
        person_id, site,
        visit_occurrence_id,
        condition_concept_id
      ), by = c('person_id','site', 'visit_occurrence_id')
    ) %>% inner_join(
      flag_3_conditions,
      by=c('condition_concept_id'='concept_id')
    ) %>%
    dplyr::select(
      person_id, site,
      observation_date,
      visit_occurrence_id,
      condition_concept_id,
      codeset
    ) %>% mutate(qualifier='condition') %>%
    rename(concept_id=condition_concept_id) %>%
    distinct()
  
  procedures_devices <- 
    visits %>%
    inner_join(
      dplyr::select(
        procedure_tbl,
        person_id, site,
        visit_occurrence_id,
        procedure_concept_id
      ), by = c('person_id','site', 'visit_occurrence_id')
    ) %>% inner_join(
      flag_3_procedures,
      by=c('procedure_concept_id'='concept_id')
    ) %>%
    mutate(codeset = paste(codeset,'-',type))%>%
    dplyr::select(
      person_id, site,
      observation_date,
      visit_occurrence_id,
      procedure_concept_id,
      codeset
    ) %>% mutate(qualifier='procedure_mech_vent') %>%
    rename(concept_id=procedure_concept_id) %>%
    distinct()
  
  devices <- 
    visits %>%
    inner_join(
      dplyr::select(
        device_tbl,
        person_id, site,
        visit_occurrence_id,
        device_concept_id
      ), by = c('person_id', 'site','visit_occurrence_id')
    ) %>% inner_join(
      flag_3_devices,
      by=c('device_concept_id'='concept_id')
    ) %>%
    mutate(codeset = paste(codeset,'-',type))%>%
    dplyr::select(
      person_id, site,
      observation_date,
      visit_occurrence_id,
      device_concept_id,
      codeset
    ) %>% mutate(qualifier='devices_mech_vent') %>%
    rename(concept_id=device_concept_id) %>%
    distinct()
  
  icu_events<-visits%>% ## should we also include ed to inpatient here?
          filter(icu_yn=='Y' & visit_type == 'ip' & icu_test_lag<=13)%>%
          dplyr::select(
            person_id, site,
            observation_date,
            visit_occurrence_id
          )%>%mutate(codeset = 'icu'
                    ,qualifier='icu_yn',
                     concept_id = 2000000078L #assert general ICU admission concept to do union-- can revisit
                     )%>%
          distinct()
    
  
  pressors <- 
    visits %>%
    inner_join(
      dplyr::select(
        drugs_tbl,
        person_id, site,
        visit_occurrence_id,
        drug_concept_id
      ), by = c('person_id', 'site','visit_occurrence_id')
    ) %>% inner_join(
      flag_3_drugs,
      by=c('drug_concept_id'='concept_id')
    ) %>%
    dplyr::select(
      person_id, site,
      observation_date,
      visit_occurrence_id,
      drug_concept_id,
      codeset
    ) %>% mutate(qualifier='drug') %>%
    rename(concept_id=drug_concept_id) %>%
    distinct()
  
  death <- 
    visits %>%
    filter(visit_type %in% c('ed/ip','ip','obs'))%>%
    inner_join(
      dplyr::select(
        death_tbl,
        person_id, site,
        death_date,
        death_type_concept_id
      ), by = c('person_id', 'site')
    )%>%
    filter(death_date>=visit_start_date&death_date<=visit_end_date)%>%
  distinct() %>%
  dplyr::select(
    person_id, site,
    observation_date,
    visit_occurrence_id,
    death_type_concept_id
  ) %>% mutate(codeset = 'death',
               qualifier='death') %>%
  rename(concept_id=death_type_concept_id) %>%
  distinct()
  
  dplyr::union(
    conds,
    icu_events)%>%
  dplyr::union(procedures_devices)%>%
  dplyr::union(devices)%>%
  dplyr::union( pressors)%>%
  dplyr::union(death)%>%
    mutate(flag=3L)
  
}

#' compute severity_visit_level for cohort
#' 
#' @param flag_1_tbl the metadata table for patients meeting flag 1
#' @param flag_2_tbl the metadata table for patients meeting flag 2
#' @param flag_3_tbl the metadata table for patients meeting flag 3
#' @param visits_tbl the visits table
#' 
#' @return metadata fields `visit_severity_flag`
#' 

compute_visit_level_severity <- function(visit_cohort,
                          flag_1_tbl=results_tbl('flag_1'),
                          flag_2_tbl=results_tbl('flag_2'),
                          flag_3_tbl=results_tbl('flag_3'), all=FALSE){
  
  ## Question: Do we need to check the flag date here to make sure it happens within 14 days to count it?
  ## HR: If we use the visit table, we are already restricting to visits occurring within 13 days, right?
  
  visit_flags<-dplyr::union(
    dplyr::select(flag_1_tbl,person_id, site,observation_date,flag)%>%distinct(),
    dplyr::select(flag_2_tbl,person_id, site,observation_date,flag)%>%distinct())%>%
    dplyr::union(dplyr::select(flag_3_tbl,person_id, site,observation_date,flag)%>%distinct())%>%
    group_by(person_id, site,observation_date)%>%
    summarise(visit_max_flag=max(flag)) %>% ungroup()
  
  temp<-visit_cohort%>%
    dplyr::select(
      person_id, site,
      observation_date,
      test_result
    ) %>% distinct() %>% 
    left_join(visit_flags,by=c('person_id', 'site','observation_date'))
  if (all){
    temp %>%
      mutate(
        visit_severity_flag=case_when(
          is.na(visit_max_flag) ~ 0L,#'asymptomatic',
          TRUE ~ (visit_max_flag)
        )
      ) %>%
      dplyr::select(person_id, site,observation_date,visit_max_flag,visit_severity_flag)  %>%
      distinct()%>%
      mutate(visit_severity_index = 
               case_when(
                 visit_severity_flag=='3' ~ 'Severe',
                 visit_severity_flag=='2' ~ 'Moderate',
                 visit_severity_flag=='1' ~ 'Mild',
                 visit_severity_flag=='0' ~ 'Asymptomatic'
               ))
    
  }else{
    temp %>%
      mutate(
        visit_severity_flag=case_when(
          test_result=="negative"~ -1L,#'negative', #can revisit
          test_result=="positive" & is.na(visit_max_flag) ~ 0L,#'asymptomatic',
          TRUE ~ (visit_max_flag)
        )
      ) %>%
      dplyr::select(person_id, site,observation_date,visit_max_flag,visit_severity_flag)  %>%
      distinct()%>%
      mutate(visit_severity_index = 
               case_when(
                 visit_severity_flag=='3' ~ 'Severe',
                 visit_severity_flag=='2' ~ 'Moderate',
                 visit_severity_flag=='1' ~ 'Mild',
                 visit_severity_flag=='0' ~ 'Asymptomatic',
                 TRUE ~ 'Negative'
               ))
  }
  
}

#' compute severity_visit_level for cohort
#' 
#' @param visit_severity_tbl the visit table with severity assignment
#' 
#' @return metadata fields `person_severity_flag`
#' 

compute_person_level_severity <- function(visit_severity_tbl){

  visit_severity_tbl%>%
    group_by(person_id, site)%>%
    summarise(person_severity_flag=max(visit_severity_flag))%>%
    collect()%>%
    mutate(person_severity_index = 
             case_when(
                person_severity_flag=='3' ~ 'Severe',
                person_severity_flag=='2' ~ 'Moderate',
                person_severity_flag=='1' ~ 'Mild',
                person_severity_flag=='0' ~ 'Asymptomatic',
                TRUE ~ 'Negative'
           ))
}




get_moderate_single_codeset<-function(codeset_name,
                                   flag_2_tbl=results_tbl('flag_2'),
                                   person_level_severity=results_tbl('person_level_severity')){
  
  codeset_persons<-person_level_severity%>%
    filter(person_severity_index=='Moderate')%>%
    inner_join(flag_2_tbl,by=c('person_id', 'site'))%>%
    filter(codeset==codeset_name)%>%
    dplyr::select(person_id, site)%>%distinct()
  
  codeset_only_persons<-codeset_persons%>%
    anti_join(
      person_level_severity%>%
        filter(person_severity_index=='Moderate')%>%
        inner_join(flag_2_tbl,by=c('person_id', 'site'))%>%
        filter(!codeset==codeset_name)%>%
        dplyr::select(person_id, site)%>%distinct(),by=c('person_id', 'site'))%>%
    dplyr::select(person_id, site)%>%
    distinct()%>%
    mutate(codeset_only='Yes')
}


