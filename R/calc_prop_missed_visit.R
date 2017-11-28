#' Title
#'
#' @param contract_network
#' @param measurement_name
#' @param obs_window_start
#' @param obs_window_stop
#' @param bld_sch_name
#' @param establish_con
#' @param tz
#'
#' @return
#' @export
#'
#' @examples
calc_prop_missed_visit <- function(contract_network = 2
                            ,measurement_name = "missed_visit"
                            ,tz = 'America/Los_Angeles'
                            ,obs_window_start = as.Date(ifelse(ymd(20170920) + days(180) > rollback(today())
                                                               ,ymd(20170920), rollback(today()) - days(180))
                                                        ,origin = "1970-01-01")
                            ,obs_window_stop = rollback(today())
                            ,bld_sch_name = "independent"
                            ,establish_con = TRUE){

  message(paste0("* begin build procedure for "
                 ,measurement_name
                 ," metric *")
  )

  if (establish_con) {
    message("set connection to build schema... ", appendLF = FALSE)

    suppressMessages(establish_con_olvr_rplc(set_schema = bld_sch_name))

    message("done")
  }

  message("get table build date... ", appendLF = FALSE)
  table_build_date <- tbl(con, "visitation_referral_fact") %>%
    select(id_calendar_dim_table_update) %>%
    as_data_frame() %>%
    distinct() %>%
    .$id_calendar_dim_table_update
  table_build_date <- lubridate::ymd(table_build_date, tz = tz)
  message("done")

  if (!is.na(contract_network)) {
    message("gather network contracts... ", appendLF = FALSE)

    DBI::dbSendQuery(con, dbplyr::build_sql("SET search_path TO ", 'staging'))

    tbl_network_contracts <- tbl(con, "OrganizationContracts") %>%
      filter(contractOwnerId == contract_network
             ,is.na(deletedAt)) %>%
      arrange(desc(updatedAt)) %>%
      distinct(contractOwnerId
               ,contractedOrganizationId) %>%
      select(id_organization = contractedOrganizationId) %>%
      as_data_frame()

    message("done")
  }

  ##### INSERT MEASUREMENT HERE ######

  DBI::dbSendQuery(con, dbplyr::build_sql("SET search_path TO ", 'independent'))

  dat <- tbl(con, "visitation_fact") %>%
    as_data_frame() %>%
    mutate(dt_calendar_dim_visit_start = ymd(id_calendar_dim_visit_start)) %>%
    filter(ifelse(is.na(fl_cancelled_not_same_day) | fl_cancelled_not_same_day == FALSE
                  ,TRUE, FALSE)) %>%
    select(id_visitation_fact
           ,id_provider_dim_pcv
           ,fl_cancelled_same_day
           ,fl_cancelled_not_same_day
           ,dt_calendar_dim_visit_start)

  ###### STOP HERE ########

  if (!is.na(contract_network)) {
    message("restrict to network contracts... ", appendLF = FALSE)

    dat_hack <- dat %>%
      filter(id_provider_dim_pcv %in% tbl_network_contracts$id_organization) %>%
      rename(date_marker = dt_calendar_dim_visit_start
             ,measure_value = fl_cancelled_same_day) %>%
      mutate(measure_value = ifelse(measure_value, 1, measure_value)
             ,measure_value = ifelse(!measure_value, 0, measure_value))

    message("done")
  }

  file_path_long <- paste0(system.file('extdata'
                                       ,package = 'oliveR2')
                           ,'/'
                           ,paste0("prop_long_", measurement_name, ".rds")
  )

  readr::write_rds(dat_hack, file_path_long)

  if (!is.na(obs_window_start)) {
    message("applying observation window filter... ", appendLF = FALSE)

    dat <- dat %>%
      filter(dt_calendar_dim_visit_start >= obs_window_start)

    message("done")
  }

  if (!is.na(obs_window_stop)) {
    message("apply observation window filter... ", appendLF = FALSE)

    dat <- dat %>%
      filter(dt_calendar_dim_visit_start <= obs_window_stop)

    message("done")
  }

  message(")xxxxx[;;;;;;;;;> kill connections...", appendLF = FALSE)

  kill_pg_connections()

  message("aggregate measurements for providers... ", appendLF = FALSE)

  dat_measurement <- dat %>%
    select(id_provider_dim_pcv, fl_cancelled_same_day) %>%
    group_by(id_provider_dim_pcv) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    mutate_all(funs(ifelse(is.nan(.), NA, .))) %>%
    mutate(measurement_name = measurement_name
           ,table_build_date = table_build_date
           ,obs_window_start = obs_window_start
           ,obs_window_stop = obs_window_stop
           ,measure_value = fl_cancelled_same_day) %>%
    select(-fl_cancelled_same_day)

  file_path_short1 <- paste0(system.file('extdata'
                                         ,package = 'oliveR2')
                             ,'/'
                             ,paste0("prop_", measurement_name, ".rds")
  )

  readr::write_rds(dat_measurement, file_path_short1)

  message("done")

  message("aggregate measurements for network... ", appendLF = FALSE)

  dat_measurement_all <- dat %>%
    select(fl_cancelled_same_day) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    mutate_all(funs(ifelse(is.nan(.), NA, .))) %>%
    mutate(measurement_name = measurement_name
           ,table_build_date = table_build_date
           ,obs_window_start = obs_window_start
           ,obs_window_stop = obs_window_stop
           ,measure_value = fl_cancelled_same_day) %>%
    select(-fl_cancelled_same_day)

  file_path_short2 <- paste0(system.file('extdata'
                                         ,package = 'oliveR2')
                             ,'/'
                             ,paste0("prop_network_", measurement_name, ".rds")
  )

  readr::write_rds(dat_measurement_all, file_path_short2)

  message("done")

}
