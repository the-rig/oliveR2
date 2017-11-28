#' Title
#'
#' @param bld_sch_name
#' @param wrt_sch_name
#' @param establish_con
#'
#' @return
#' @export
#'
#' @examples
build_visitation_referral_attribute_dim <- function(bld_sch_name = NA
                                                    ,wrt_sch_name = NA
                                                    ,establish_con = FALSE){

  message("* begin visitation_referral_attribute_dim build procedure *")


  if(establish_con){
    message("setting connection to build schema... ", appendLF = FALSE)

    suppressMessages(establish_con_olvr_rplc(set_schema = bld_sch_name))

    message("done")
  }



  message("building safety issues table... ", appendLF = FALSE)

  suppressWarnings(
  tbl_visit_safety_issues <- DBI::dbGetQuery(con
                                             ,"SELECT
                                             \"ServiceReferrals\".id id_referral_visit
                                             ,\"organizationId\" id_organization
                                             ,json_array_elements(\"ServiceReferrals\".\"safetyIssues\") ->> 'issueType'::text AS tx_issue_type
                                             FROM staging.\"ServiceReferrals\"
                                             WHERE \"ServiceReferrals\".\"isCurrentVersion\" = true AND \"ServiceReferrals\".\"deletedAt\" IS NULL")  %>%
    mutate(cd_issue_type = forcats::fct_drop(tx_issue_type)
           ,cd_issue_type = forcats::fct_infreq(cd_issue_type)
           ,cd_issue_type = forcats::fct_anon(cd_issue_type)
           ,tx_issue_type = ifelse(tx_issue_type == "", NA, tx_issue_type)
           ,fl_safety_issue_anger = ifelse(tx_issue_type == "Anger Outbursts", 1, 0)
           ,fl_safety_issue_dv = ifelse(tx_issue_type == "Domestic Violence", 1, 0)
           ,fl_safety_issue_inppr_convo = ifelse(tx_issue_type == "Inappropriate conversation", 1, 0)
           ,fl_safety_issue_inppr_touch = ifelse(tx_issue_type == "Inappropriate touching", 1, 0)
           ,fl_safety_issue_abscond = ifelse(tx_issue_type == "May try to leave with child", 1, 0)
           ,fl_safety_issue_med_cmplx = ifelse(tx_issue_type == "Medically Complex Child", 1, 0)
           ,fl_safety_issue_other = ifelse(tx_issue_type == "Other", 1, 0)
           ,fl_safety_issue_nco = ifelse(tx_issue_type == "Restraining / No Contact Order", 1, 0)
           ,fl_safety_issue_sa = ifelse(tx_issue_type == "Substance Abuse", 1, 0)
           ,fl_safety_issue_threat = ifelse(tx_issue_type == "Threatening behavior", 1, 0)
    ) %>%
    as_data_frame()
)

  message("done")

  message("building visit location table... ", appendLF = FALSE)

  suppressWarnings(
  tbl_visit_locations <- left_join(DBI::dbGetQuery(con
                                                   ,"SELECT
                                                   \"ServiceReferrals\".id id_referral_visit
                                                   ,\"organizationId\" id_organization
                                                   ,json_array_elements(\"ServiceReferrals\".\"visitLocations\") ->> 'visitLocationType'::text AS tx_location_type
                                                   ,json_array_elements(\"ServiceReferrals\".\"visitLocations\") ->> 'visitLocationAddress'::text AS tx_location_address
                                                   ,json_array_elements(\"ServiceReferrals\".\"visitLocations\") ->> 'visitLocationCity'::text AS tx_location_city
                                                   ,json_array_elements(\"ServiceReferrals\".\"visitLocations\") ->> 'visitLocationState'::text AS tx_location_state
                                                   ,json_array_elements(\"ServiceReferrals\".\"visitLocations\") ->> 'visitLocationZip'::text AS tx_location_zip
                                                   FROM staging.\"ServiceReferrals\"
                                                   WHERE \"ServiceReferrals\".\"isCurrentVersion\" = true AND \"ServiceReferrals\".\"deletedAt\" IS NULL") %>%
                                     as_data_frame()
                                   ,tbl(con, "Organizations") %>% rename(id_organization = id) %>% as_data_frame()
                                   ,by = "id_organization") %>%
    mutate(tx_location_type = ifelse(tx_location_type == "", NA, tx_location_type)
           ,cd_location_type = forcats::fct_drop(tx_location_type)
           ,cd_location_type = forcats::fct_infreq(cd_location_type)
           ,cd_location_type = forcats::fct_anon(cd_location_type)
           ,tx_location_address = stringr::str_extract(tx_location_address, "\\d{1,}(\\s{1}\\w{1,})(\\s{1}?\\w{1,})+")
           ,tx_location_address = ifelse(is.na(tx_location_address) &
                                           tx_location_type %in% c("Provider site"
                                                                   ,"Administrative office")
                                         ,billingAddress
                                         ,tx_location_address)
           ,tx_location_city = ifelse(is.na(tx_location_city) &
                                        tx_location_type %in% c("Provider site"
                                                                ,"Administrative office")
                                      ,city
                                      ,tx_location_city)
           ,tx_location_state = ifelse(is.na(tx_location_state) &
                                         tx_location_type %in% c("Provider site"
                                                                 ,"Administrative office")
                                       ,state
                                       ,tx_location_state)
    ) %>%
    select(id_referral_visit
           ,id_organization
           ,tx_location_type
           ,cd_location_type
           ,tx_location_address
           ,tx_location_city
           ,tx_location_state) %>%
    as_data_frame()
  )

  message("done")

  message("building visit referral table... ", appendLF = FALSE)

suppressWarnings(
  tbl_visit_referrals <- tbl(con, "ServiceReferrals") %>%
  select(id_referral_visit = id
           ,fosterParents
           ,caseAidesOrIntern
           ,relatives
           ,timeNegotiable
           ,visitsPerWeek
           ,hoursPerVisit
           ,languageRequirementNeed
           ,serviceType) %>%
  as_data_frame() %>%
  filter(isCurrentVersion
           ,is.na(deletedAt))
  )
  message("done")

  message("building visit visit attribute table... ", appendLF = FALSE)

  suppressWarnings(
  visitation_referral_attribute_dim_full <- full_join(
    tbl_visit_safety_issues %>% select(-id_organization)
    ,tbl_visit_locations %>% select(id_referral_visit, cd_location_type)
    ,by = "id_referral_visit"
  ) %>% right_join(tbl_visit_referrals
                   ,by = "id_referral_visit") %>%
    mutate(fl_attempted_fp_visit = fosterParents
           ,fl_attempted_intern_visit = caseAidesOrIntern
           ,fl_attempted_relative_visit = relatives
           ,fl_community_visit_acceptable = ifelse(as.character(cd_location_type) %in% c("9", "8", "7", "5", "3")
                                                   ,TRUE, NA)
           ,fl_community_visit_acceptable = ifelse(!(as.character(cd_location_type) %in% c("9", "8", "7", "5", "3"))
                                                   ,FALSE, fl_community_visit_acceptable)
           ,fl_dshs_office_visit_acceptable = ifelse(as.character(cd_location_type) %in% c("9", "8", "7", "5", "3")
                                                     ,TRUE, NA)
           ,fl_dshs_office_visit_acceptable = ifelse(!(as.character(cd_location_type) %in% c("2"))
                                                     ,FALSE, fl_dshs_office_visit_acceptable)
           ,fl_caregiver_home_visit_acceptable = NA
           ,fl_visit_facility_visit_acceptable = ifelse(as.character(cd_location_type) %in% c("7")
                                                        ,TRUE, NA)
           ,fl_visit_facility_visit_acceptable = ifelse(!(as.character(cd_location_type) %in% c("7"))
                                                        ,FALSE, fl_visit_facility_visit_acceptable)
           ,fl_relative_home_visit_acceptable = ifelse(as.character(cd_location_type) %in% c("3")
                                                       ,TRUE, NA)
           ,fl_relative_home_visit_acceptable = ifelse(!(as.character(cd_location_type) %in% c("3"))
                                                       ,FALSE, fl_relative_home_visit_acceptable)
           ,fl_parental_home_visit_acceptable = ifelse(as.character(cd_location_type) %in% c("1")
                                                       ,TRUE, NA)
           ,fl_parental_home_visit_acceptable = ifelse(!(as.character(cd_location_type) %in% c("1"))
                                                       ,FALSE, fl_parental_home_visit_acceptable)
           ,fl_negotiable_visit_time = ifelse(is.na(timeNegotiable), NA, timeNegotiable)
           ,fl_negotiable_visit_time = ifelse(!is.na(fl_negotiable_visit_time) &
                                                fl_negotiable_visit_time == "Yes"
                                              ,TRUE
                                              ,fl_negotiable_visit_time)
           ,fl_negotiable_visit_time = ifelse(!is.na(fl_negotiable_visit_time) &
                                                fl_negotiable_visit_time == "No"
                                              ,FALSE
                                              ,fl_negotiable_visit_time)
           ,fl_transport_required = ifelse(serviceType %in% c("Parent / Child with Transportation"
                                                              ,"Sibling with Transportation"
                                                              ,"Transportation Only")
                                           ,TRUE
                                           ,FALSE)
           ,fl_interpreter_required = ifelse(languageRequirementNeed == "Interpreter or specified"
                                             ,TRUE
                                             ,NA)
           ,fl_interpreter_required = ifelse(languageRequirementNeed == "None"
                                             ,FALSE
                                             ,fl_interpreter_required)
           ,fl_sfty_iss_anger_outburst = ifelse(as.character(cd_issue_type) %in% c("09")
                                                ,TRUE
                                                ,NA)
           ,fl_sfty_iss_anger_outburst = ifelse(!(as.character(cd_issue_type) %in% c("09"))
                                                ,FALSE
                                                ,fl_sfty_iss_anger_outburst)
           ,fl_sfty_iss_inappr_touch = ifelse(as.character(cd_issue_type) %in% c("11")
                                              ,TRUE
                                              ,NA)
           ,fl_sfty_iss_inappr_touch = ifelse(!(as.character(cd_issue_type) %in% c("11"))
                                              ,FALSE
                                              ,fl_sfty_iss_inappr_touch)
           ,fl_sfty_iss_inappr_convo = ifelse(as.character(cd_issue_type) %in% c("11")
                                              ,TRUE
                                              ,NA)
           ,fl_sfty_iss_inappr_convo = ifelse(!(as.character(cd_issue_type) %in% c("11"))
                                              ,FALSE
                                              ,fl_sfty_iss_inappr_convo)
           ,fl_sfty_iss_try_to_leave = ifelse(as.character(cd_issue_type) %in% c("01")
                                              ,TRUE
                                              ,NA)
           ,fl_sfty_iss_try_to_leave = ifelse(!(as.character(cd_issue_type) %in% c("01"))
                                              ,FALSE
                                              ,fl_sfty_iss_try_to_leave)
           ,fl_sfty_iss_threat_beh = ifelse(as.character(cd_issue_type) %in% c("03")
                                            ,TRUE
                                            ,NA)
           ,fl_sfty_iss_threat_beh = ifelse(!(as.character(cd_issue_type) %in% c("03"))
                                            ,FALSE
                                            ,fl_sfty_iss_threat_beh)
           ,fl_sfty_iss_med_cmplx_ch = ifelse(as.character(cd_issue_type) %in% c("04")
                                              ,TRUE
                                              ,NA)
           ,fl_sfty_iss_med_cmplx_ch = ifelse(!(as.character(cd_issue_type) %in% c("04"))
                                              ,FALSE
                                              ,fl_sfty_iss_med_cmplx_ch)
           ,fl_sfty_iss_no_cntct_ord = ifelse(as.character(cd_issue_type) %in% c("10")
                                              ,TRUE
                                              ,NA)
           ,fl_sfty_iss_no_cntct_ord = ifelse(!(as.character(cd_issue_type) %in% c("10"))
                                              ,FALSE
                                              ,fl_sfty_iss_no_cntct_ord)
           ,fl_sfty_iss_dv = ifelse(as.character(cd_issue_type) %in% c("07")
                                    ,TRUE
                                    ,NA)
           ,fl_sfty_iss_dv = ifelse(!(as.character(cd_issue_type) %in% c("07"))
                                    ,FALSE
                                    ,fl_sfty_iss_dv)
           ,qt_visit_duration_hrs = as.integer(hoursPerVisit)
           ,qt_visits_per_week = as.integer(visitsPerWeek)
    ) %>%
    group_by(id_referral_visit) %>%
    summarise_at(.vars = vars(starts_with("fl_"))
                 ,.funs = max
                 ,na.rm = TRUE) %>%
    mutate_at(.vars = vars(starts_with("fl_"))
              ,.funs = funs(ifelse(is.infinite(.), 0, .)))
  )
  message("done")

  message("reducing full table to unique dimensions... ", appendLF = FALSE)

  suppressMessages(
  visitation_referral_attribute_dim <- visitation_referral_attribute_dim_full %>%
    select(-id_referral_visit) %>%
    distinct() %>%
    group_by_all() %>%
    { mutate(ungroup(.), id_visitation_referral_attribute_dim = group_indices(.)) }
  )

  message("done")

  message("creating lookup table for unique dimensions... ", appendLF = FALSE)

  suppressMessages(
  visitation_referral_attribute_fact_and_dim <- inner_join(visitation_referral_attribute_dim
                                                           ,visitation_referral_attribute_dim_full) %>%
    select(id_visitation_referral_attribute_dim
           ,id_visitation_referral_fact = id_referral_visit) %>%
    as_data_frame()
  )

  assign("visitation_referral_attribute_fact_and_dim"
         ,visitation_referral_attribute_fact_and_dim, envir = .GlobalEnv)

  message("done")


  if(establish_con){
    message("switching to write schema... ", appendLF = FALSE)

    suppressMessages(DBI::dbSendQuery(con, dbplyr::build_sql("SET search_path TO ", wrt_sch_name)))

    message("done")
  }

  message("send visitation_referral_attribute_dim to db... ", appendLF = FALSE)

  job_status <- DBI::dbWriteTable(conn = con
                                  ,name = "visitation_referral_attribute_dim"
                                  ,value = visitation_referral_attribute_dim
                                  ,overwrite = TRUE
                                  ,row.names = FALSE)

  # TODO: document the table as was done for calendar_dim
  # col_desc <- c(
  #   id_visitation_referral_fact = "PK"
  #   ,id_provider_dim_pcv = "test"
  #   ,id_calendar_dim_opd = "test"
  #   ,id_calendar_dim_created = "test"
  #   ,id_calendar_dim_received = "test"
  #   ,id_calendar_dim_assigned = "test"
  #   ,id_calendar_dim_scheduled = "test"
  #   ,id_calendar_dim_inprogress = "test"
  #   ,id_visitation_referral_attribute_dim = "test"
  # )
  #
  # for (i in colnames(visitation_referral_attribute_dim)){
  #   col_comment_olvr_rplc(sch_name = "independent"
  #                         ,tbl_name = "visitation_referral_attribute_dim"
  #                         ,col_name = i
  #                         ,col_comment = as.character(col_desc[i]))
  # }

  # meta_visitation_referral_attribute_dim <- dplyr::as_data_frame(dplyr::data_frame(col_desc, col = names(col_desc)))
  #
  # save(meta_visitation_referral_attribute_dim
  #      ,file = "data/meta_visitation_referral_attribute_dim.rda")

  if (job_status) {
    message("done")
  } else {
    message("unable to build and write table")
  }
}

