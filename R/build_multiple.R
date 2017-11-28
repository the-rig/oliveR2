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
build_multiple <- function(bld_sch_name = "staging"
                                           ,wrt_sch_name = "independent"
                                           ,establish_con = TRUE){

  message("* BUILD ANALYTIC TABLES *")

  build_function_list <- tibble::tribble(
    ~f,                                          ~params,
    "build_calendar_dim",                        list(wrt_sch_name = wrt_sch_name
                                                      ,establish_con = establish_con),
    "build_visitation_referral_attribute_dim",   list(bld_sch_name = bld_sch_name
                                                      ,wrt_sch_name = wrt_sch_name
                                                      ,establish_con = establish_con),
    "build_visitation_referral_fact",            list(bld_sch_name = bld_sch_name
                                                      ,wrt_sch_name = wrt_sch_name
                                                      ,establish_con = establish_con)
  )

  out <- purrr::invoke_map(build_function_list$f
                    ,build_function_list$params)
}


