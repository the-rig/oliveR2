#' Title
#'
#' @param df_file
#' @param org_id
#' @param org_col
#' @param window_start
#' @param window_end
#'
#' @return
#' @export
#'
#' @examples
measure_extract_time_series <- function(df_file = "chron_long_assigned_to_scheduled.rds"
                                        ,org_id = 9
                                        ,org_col = "id_provider_dim_pcv"
                                        ,window_start = ymd(20170301)
                                        ,window_end = rollback(today())) {

  file_path1 <- paste0(system.file('extdata'
                                  ,package = 'oliveR2')
                      ,'/'
  )

  df <- readr::read_rds(paste0(file_path1, df_file))


  if (!all(is.na(org_col), is.na(org_id))) {

    file_path2 <- paste0(system.file('extdata'
                                     ,package = 'oliveR2')
                         ,'/'
                         ,"ref_lookup_network_contractors.rds"
    )

    df_orgs <- readr::read_rds(file_path2)

    org_id_lazy <- lazyeval::interp(~y == x, .values=list(y = as.name(org_col), x = org_id))

    full_window_raw <- seq(window_start
                           ,by='1 month'
                           ,length=as.numeric(as.period(interval(window_start, window_end)), units = "months"))

    full_window_pretty_months <- lubridate::month(full_window_raw, label = TRUE)

    full_window_raw_months <- lubridate::month(full_window_raw)

    full_window_years <- lubridate::year(full_window_raw)

    full_window <- tibble(month = full_window_raw_months
                          ,year = full_window_years) %>%
      expand(month, year, org = df_orgs$org_id)

    names(full_window) <- c(names(full_window)[1:2], org_col)

    ts_out <- left_join(
      full_window %>%
        mutate(date = as.Date(paste(year, month, '1', sep = "-")))
      ,df %>%
        filter(ymd(date_marker) < window_end
               ,ymd(date_marker) >= window_start) %>%
        filter(!is.na(measure_value)
               ,measure_value < 2020
               ,!is.na(date_marker)) %>%
        mutate(date_marker = ymd(date_marker)) %>%
        filter(year(date_marker) < 2020) %>%
        select(date_marker, measure_value, id_provider_dim_pcv) %>%
        group_by(id_provider_dim_pcv) %>%
        tidyquant::tq_transmute(
          select     = measure_value,
          mutate_fun = apply.monthly,
          FUN        = mean,
          na.rm      = TRUE,
          col_rename = "mean_value"
        ) %>% mutate(month = month(date_marker)
                     ,year = year(date_marker))
    ) %>%
      select(one_of(org_col, "date", "mean_value")) %>%
      tidyr::nest(-one_of(org_col)) %>%
      mutate(ts = map(.x = data,
                      .f = timetk::tk_ts,
                      select = mean_value,
                      frequency = 12,
                      start = c(year(window_start),month(window_start)))) %>%
      filter_(org_id_lazy)

  } else {

    full_window_raw <- seq(window_start
                           ,by='1 month'
                           ,length=as.numeric(as.period(interval(window_start, window_end)), units = "months"))

    full_window_pretty_months <- lubridate::month(full_window_raw, label = TRUE)

    full_window_raw_months <- lubridate::month(full_window_raw)

    full_window_years <- lubridate::year(full_window_raw)

    full_window <- tibble(month = full_window_raw_months
                          ,year = full_window_years)

    ts_out <- left_join(
      full_window %>%
        mutate(date = as.Date(paste(year, month, '1', sep = "-")))
      ,df %>%
        filter(ymd(date_marker) < window_end
               ,ymd(date_marker) >= window_start) %>%
        filter(!is.na(measure_value)
               # need to paramaterize this at some point
               # we may eventually have measures in which this is not an outlier
               ,measure_value < 2020
               ,!is.na(date_marker)) %>%
        mutate(date_marker = ymd(date_marker)) %>%
        filter(year(date_marker) < 2020) %>%
        select(date_marker, measure_value) %>%
        tidyquant::tq_transmute(
          select     = measure_value,
          mutate_fun = apply.monthly,
          FUN        = mean,
          na.rm      = TRUE,
          col_rename = "mean_value"
        ) %>% mutate(month = month(date_marker)
                     ,year = year(date_marker))
    ) %>%
      select(one_of("date", "mean_value")) %>%
      nest() %>%
      mutate(ts = map(.x = data,
                      .f = timetk::tk_ts,
                      select = mean_value,
                      frequency = 12,
                      start = c(year(window_start),month(window_start))))
  }

  if (nrow(ts_out) == 0) {
    ts_out <- ts(rep(NA, length(full_window_raw)), frequency = 12, start = c(year(window_start),month(window_start)))
    ts_out <- list(ts_out)
    ts_out <- data_frame(ts = ts_out)
  } else if (length(ts_out$ts[[1]]) != length(full_window_raw)) {

    ts_out2 <- left_join(tibble(month = full_window_pretty_months, year = full_window_years)
              ,data.frame(ts_out$ts[[1]], month=as.numeric(time(ts_out$ts[[1]]))) %>%
                mutate(month = as.numeric(month)
                       ,month = format(date_decimal(month), "%Y-%m-%d")
                       ,year = year(month)
                       ,month = month(month, label = TRUE)) %>%
                as_data_frame()
    ) %>% .$mean_value
    ts_out <- ts_out2
    ts_out <- data_frame(ts = list(ts(ts_out2, start = c(year(window_start),month(window_start)), frequency = 12)))

  }

  ts_out$ts[[1]]

}
