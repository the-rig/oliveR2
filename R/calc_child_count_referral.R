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
calc_child_count_referral <- function(contract_network = 2
                            ,measurement_name = "child_count"
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
  table_build_date <- ymd(table_build_date, tz = tz)
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

  dat <- tbl(con, "visitation_referral_fact") %>%
    as_data_frame() %>%
    mutate(dt_calendar_dim_created = ymd(id_calendar_dim_created)) %>%

    select(id_visitation_referral_fact
           ,dt_calendar_dim_created
           ,id_provider_dim_pcv
           ,qt_child_count)

  ###### STOP HERE ########

  if (!is.na(contract_network)) {
    message("restrict to network contracts... ", appendLF = FALSE)

    dat <- dat %>%
      filter(id_provider_dim_pcv %in% tbl_network_contracts$id_organization) %>%
      rename(date_marker = dt_calendar_dim_created
             ,measure_value = qt_child_count)
    message("done")
  }

  file_path_long <- paste0(system.file('extdata'
                                       ,package = 'oliveR2')
                           ,'/'
                           ,paste0("count_long_", measurement_name, ".rds")
  )

  readr::write_rds(dat, file_path_long)

  if (!is.na(obs_window_start)) {
    message("applying observation window filter... ", appendLF = FALSE)

    dat <- dat %>%
      filter(date_marker >= obs_window_start)

    message("done")
  }

  if (!is.na(obs_window_stop)) {
    message("apply observation window filter... ", appendLF = FALSE)

    dat <- dat %>%
      filter(date_marker <= obs_window_stop)

    message("done")
  }

  message(")xxxxx[;;;;;;;;;> kill connections...", appendLF = FALSE)

  kill_pg_connections()

  message("aggregate measurements for providers... ", appendLF = FALSE)

  dat_measurement <- dat %>%
    select(id_provider_dim_pcv, measure_value) %>%
    group_by(id_provider_dim_pcv) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    mutate_all(funs(ifelse(is.nan(.), NA, .))) %>%
    mutate(measurement_name = measurement_name
           ,table_build_date = table_build_date
           ,obs_window_start = obs_window_start
           ,obs_window_stop = obs_window_stop)

  file_path_short1 <- paste0(system.file('extdata'
                                         ,package = 'oliveR2')
                             ,'/'
                             ,paste0("count_", measurement_name, ".rds")
  )

  readr::write_rds(dat_measurement, file_path_short1)

  message("done")

  message("aggregate measurements for network... ", appendLF = FALSE)

  dat_measurement_all <- dat %>%
    select(measure_value) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    mutate_all(funs(ifelse(is.nan(.), NA, .))) %>%
    mutate(measurement_name = measurement_name
           ,table_build_date = table_build_date
           ,obs_window_start = obs_window_start
           ,obs_window_stop = obs_window_stop)

  file_path_short2 <- paste0(system.file('extdata'
                                         ,package = 'oliveR2')
                             ,'/'
                             ,paste0("count_network_", measurement_name, ".rds")
  )

  readr::write_rds(dat_measurement_all, file_path_short2)

  message("done")

}
