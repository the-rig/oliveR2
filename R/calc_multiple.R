#' Title
#'
#' @param obs_window_start
#' @param obs_window_stop
#'
#' @return
#' @export
#'
#' @examples
calc_multiple <- function(obs_window_start = ymd(20170301)
                          ,obs_window_stop = rollback(today())){

  message("* CALCULATE METRIC TABLES *")

  #METRICS TO ADD
  ## New Referrals Accepted X  --Acceptance
  ## Average Number of Children on Accepted Referral X --Acceptance
  ## New Referrals Scheduled X --Scheduling
  # Active Referrals
  ## Visit Reports
  ## Visit Attendance Rate
  ## Visit No Show Rate
  ## Visit Canceled Rate (including both greater than 24 hrs and less than 24 hrs)


  calc_function_list <- tribble(
    ~f,                                          ~params,
    ## Average Days from Date of Referral to date FIN receives --Receipts
    "calc_date_diff_metric",                        list(start = "created"
                                                       ,stop = "received"),
  ## Average Days from Date of Referral to Date Provider Accepted X --Acceptance
  ## % of Referrals Accepted within 1 Day from Referral Date X --Acceptance
  "calc_date_diff_metric",                        list(start = "created"
                                                     ,stop = "assigned"
                                                     ,target = 1),
  # ## Average Days from date FIN receives Referral to Provider Accepted X --Acceptance
  # ## % of Referrals Accepted within 1 Day from Received Date X --Acceptance
  "calc_date_diff_metric",                        list(start = "received"
                                                     ,stop = "assigned"
                                                     ,target = 1),
  ## Average Number of Days from Referral Date to Scheduled by Provider X --Scheduling
  ## % of Referrals Scheduled within 3 Days from Referral Date X --Scheduling
  "calc_date_diff_metric",                        list(start = "created"
                                                     ,stop = "agreed"),
  ## Average Number of Days from Accepted by Provider to Scheduled (PM #1) X --Scheduling DONE
  ## % of Referrals Scheduled within 3 Days from date Accepted by provider (PM #1) X --Scheduling DONE
  "calc_date_diff_metric",                        list(start = "assigned"
                                                     ,stop = "agreed"),
  ## Average number of days from FIN received to Scheduled by provider X DONE
  "calc_date_diff_metric",                        list(start = "received"
                                                     ,stop = "agreed"),
  ## Average Days from Accepted by Provider to First Scheduled Visit (PM #2)
  ## % of Referrals with a Scheduled Visit within 7 Days of provider Accepting referral (PM #2)
  "calc_date_diff_metric",                        list(start = "assigned"
                                                     ,stop = "scheduled"
                                                     ,target = 7),
  ## Average Days from Accepted by Provider to In Progress
  ## % of Referrals In Progress within 7 Days of provider Accepting referral
  "calc_date_diff_metric",                        list(start = "assigned"
                                                     ,stop = "inprogress"
                                                     ,target = 7),
  "calc_prop_missed_visit",                        list(),
  "calc_prop_missed_visit_prvdr_fault",            list()
  )

  out <- purrr::invoke_map(calc_function_list$f
                    ,calc_function_list$params
                    ,obs_window_start = obs_window_start
                    ,obs_window_stop = obs_window_stop)

  establish_con_olvr_rplc()

  calc_na_contracts()

}


