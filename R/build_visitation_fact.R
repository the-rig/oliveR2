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
build_visitation_fact <- function(bld_sch_name = NA
                                           ,
                                           wrt_sch_name = NA
                                           ,
                                           establish_con = FALSE) {

  message("* begin visitation_fact build procedure *")

  if (establish_con) {
    message("setting connection to build schema... ", appendLF = FALSE)

    suppressMessages(establish_con_olvr_rplc(set_schema = bld_sch_name))

    message("done")
  }

  message("building visit report table... ", appendLF = FALSE)

  suppressWarnings(
    tbl_visit_reports <- tbl(con, "VisitReports") %>%
      filter(isCurrentVersion
             , is.na(deletedAt)
             , !is.na(approvedAt)) %>%
      select(
        id_visitation_referral_fact = serviceReferralId,
        id_visitation_fact = id,
        dt_calendar_dim_visit_start = date,
        causedBy,
        cancellationType
      ) %>%
      mutate(fl_cancelled_same_day = ifelse(cancellationType == "Cancelled (less than 24 hour notice)" |
                                              cancellationType == "No-show"
                                            ,TRUE
                                            ,FALSE)
             ,fl_cancelled_same_day = ifelse(is.na(cancellationType)
                                             ,NA
                                             ,fl_cancelled_same_day)
             ,fl_cancelled_not_same_day = ifelse(cancellationType == "Cancelled (more than 24 hour notice)" |
                                               cancellationType == "Missed / Cancelled"
                                             ,TRUE
                                             ,FALSE)
             ,fl_cancelled_not_same_day = ifelse(is.na(cancellationType)
                                             ,NA
                                             ,fl_cancelled_not_same_day)
             ,fl_cancelled_by_provider = ifelse(causedBy == "Provider"
                                                ,TRUE
                                                ,FALSE)
             ,fl_cancelled_by_provider = ifelse(is.na(causedBy)
                                                ,NA
                                                ,fl_cancelled_by_provider)
      ) %>% as_data_frame()
  )
  message("done")


  message("switching to independent schema... ", appendLF = FALSE)

  suppressMessages(DBI::dbSendQuery(con, dbplyr::build_sql("SET search_path TO ", "independent")))

  message("done")

  message("loading visitation_referral_fact table... ", appendLF = FALSE)

  suppressWarnings(
    visitation_referral_fact <- tbl(con, "visitation_referral_fact") %>%
      as_data_frame()
  )
  message("done")

  message("combining all tables... ", appendLF = FALSE)

  suppressMessages(
    visitation_fact <- list(
      tbl_visit_referral_case,
      tbl_visit_reports,
      visitation_referral_fact
    ) %>%
      Reduce(function(dtf1, dtf2)
        left_join(dtf1, dtf2, by = "id_visitation_referral_fact"), .) %>%
      mutate(
        id_calendar_dim_visit_start = as.integer(format(ymd(dt_calendar_dim_visit_start), "%Y%m%d"))
      ) %>%
      select(
        id_visitation_fact,
        id_visitation_referral_fact,
        id_calendar_dim_visit_start,
        id_case,
        fl_cancelled_not_same_day,
        fl_cancelled_same_day,
        fl_cancelled_by_provider,
        id_provider_dim_pcv,
        -causedBy,
        -cancellationType
      ) %>%
      as_data_frame() %>%
      filter(!is.na(id_visitation_fact))
  )
  message("done")

  if(establish_con){
    message("switching to write schema... ", appendLF = FALSE)

    suppressMessages(DBI::dbSendQuery(con, dbplyr::build_sql("SET search_path TO ", wrt_sch_name)))

    message("done")
  }

  message("send visitation_fact to db... ", appendLF = FALSE)


  visitation_fact <- visitation_fact %>%
    mutate(id_calendar_dim_table_update = as.integer(format(now(), "%Y%m%d")))

  job_status <- DBI::dbWriteTable(
    conn = con,
    name = "visitation_fact",
    value = visitation_fact,
    overwrite = TRUE,
    row.names = FALSE
  )


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
  # for (i in colnames(visitation_referral_fact)){
  #   col_comment_olvr_rplc(sch_name = "independent"
  #                         ,tbl_name = "visitation_referral_fact"
  #                         ,col_name = i
  #                         ,col_comment = as.character(col_desc[i]))
  # }
  #
  # meta_visitation_referral_fact <- dplyr::as_data_frame(dplyr::data_frame(col_desc, col = names(col_desc)))

  # save(meta_visitation_referral_fact
  #      ,file = "data/meta_visitation_referral_fact.rda")

  if (job_status) {
    message("done")
  } else {
    message("unable to build and write table")
  }
}

