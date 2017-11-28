#' Title
#'
#' @param from_date
#' @param to_date
#' @param country_filter
#' @param region_filter
#' @param tz
#' @param wrt_sch_name
#' @param establish_con
#' @param vector_out
#'
#' @return
#' @export
#'
#' @examples
build_calendar_dim <- function(from_date = '01-01-2011'
                               ,to_date = '01-01-2100'
                               ,country_filter = 'usa'
                               ,region_filter = 'Washington'
                               ,tz = 'UTC'
                               ,wrt_sch_name = NA
                               ,establish_con = FALSE
                               ,vector_out = FALSE){

  options(tz=tz)
  Sys.setenv(TZ=tz)
  message("* begin calendar_dim build procedure *")

  if (establish_con) {
    message("setting connection to build schema... ", appendLF = FALSE)

    suppressMessages(establish_con_olvr_rplc(set_schema = wrt_sch_name))

    message("done")
  }

  message("connect to holiday api and get holidates... ", appendLF = FALSE)

  # connect to the Enrico holiday service
  content_url <- paste0("http://www.kayaposoft.com/enrico/json/v1.0/?action=getPublicHolidaysForDateRange&="
                        ,"&fromDate="
                        ,from_date
                        ,"&toDate="
                        ,to_date
                        ,"&country="
                        ,country_filter
                        ,"&region="
                        ,region_filter)

  # get request to the generated URL
  GET_content <- httr::GET(content_url)

  # Build a vector of holiday dates (holidates) from the content of the GET method
  # this is a parse of the returned JSON from the holiday API
  holidays <- httr::content(GET_content
                      ,as = "text"
                      ,encoding = 'UTF-8') %>%
    tidyjson::as.tbl_json() %>%
    tidyjson::gather_array() %>%
    tidyjson::enter_object("date") %>%
    tidyjson::spread_values(day = tidyjson::jstring("day")
                  ,month = tidyjson::jstring("month")
                  ,year = tidyjson::jstring("year")) %>%
    mutate(holidate = paste0(year
                             ,stringr::str_pad(month, 2, pad = "0")
                             ,stringr::str_pad(day, 2, pad = "0"))
           ,holidate = ymd(holidate, tz = tz)) %>%
    .$holidate

  message("done")

  message("get weekend dates... ", appendLF = FALSE)

  # build a vector of weekend intervals
  # We start with a vector of all days
  day_seq <- seq(from = lubridate::mdy(from_date, tz = tz)
                 ,to = lubridate::mdy(to_date, tz = tz)
                 ,by = "days")

  # Then filter the weekends by name
  weekend_days <- day_seq[wday(day_seq, label = TRUE) %in%
                            c("Sat", "Sun")]

  all_days <- data_frame(all_days = seq(mdy(from_date, tz = tz)
                                             ,mdy(to_date, tz = tz), by = "day")) %>%
    mutate(id_calendar_dim = as.integer(format(all_days, "%Y%m%d", tz = tz)))

  weekend_days <- data_frame(weekend_days = weekend_days) %>%
    mutate(id_calendar_dim = as.integer(format(weekend_days, "%Y%m%d", tz = tz)))

  holidays <- data_frame(holidays = holidays) %>%
    mutate(id_calendar_dim = as.integer(format(holidays, "%Y%m%d", tz = tz)))

  message("done")

  if (vector_out) {
    date_vector <- c(weekend_days$id_calendar_dim, holidays$id_calendar_dim)
    return(date_vector)
  } else {
    message("send calendar_dim to db... ", appendLF = FALSE)

    suppressMessages(
      calendar_dim <- left_join(all_days, weekend_days, by = "id_calendar_dim") %>%
        left_join(holidays) %>%
        mutate(calendar_date = all_days
               ,calendar_date_weekend = weekend_days
               ,calendar_date_holiday = holidays
               ,calendar_year = year(calendar_date)
               ,calendar_month = month(calendar_date)
               ,calendar_quarter = quarter(calendar_date)
               ,calendar_day = day(calendar_date)
               ,fl_pre_timeline = ifelse(calendar_date < lubridate::ymd(20171020, tz = tz)
                                         ,TRUE
                                         ,FALSE)) %>%
        select(id_calendar_dim
               ,calendar_date
               ,calendar_year
               ,calendar_month
               ,calendar_quarter
               ,calendar_day
               ,calendar_date_weekend
               ,calendar_date_holiday
               ,fl_pre_timeline)
    )

    job_status <- DBI::dbWriteTable(conn = con
                                    ,name = "calendar_dim"
                                    ,value = calendar_dim
                                    ,overwrite = TRUE
                                    ,row.names = FALSE)

    # col_desc <- c(
    #   id_calendar_dim = "PK - an 8-digit integer representation of the date formatted as YYYYMMDD"
    #   ,calendar_date = "A date representation of the primary key"
    #   ,calendar_year = "An integer representation of the month of the PK (1-12)"
    #   ,calendar_month = "An integer representation of the calendar quarter of the PK (1-4)"
    #   ,calendar_quarter = "An integer representation of the day of the PK (1-365)"
    #   ,calendar_date_weekend = "A nullable date field with values on any Saturday or Sunday"
    #   ,calendar_date_holiday = "A nullable date field with values on any holiday"
    #   ,fl_pre_timeline = "A boolean flag which is TRUE when the PK is < 20171020"
    # )
    #
    # for (i in colnames(calendar_dim)){
    #   col_comment_olvr_rplc(sch_name = "independent"
    #                         ,tbl_name = "calendar_dim"
    #                         ,col_name = i
    #                         ,col_comment = as.character(col_desc[i]))
    # }
    #
    # meta_calendar_dim <- dplyr::as_data_frame(dplyr::data_frame(col_desc, col = names(col_desc)))
    #
    # save(meta_calendar_dim
    #      ,file = "data/meta_calendar_dim.rda")

    if (job_status) {
      message("done")
    } else {
      message("unable to build and write table")
    }
  }

}
