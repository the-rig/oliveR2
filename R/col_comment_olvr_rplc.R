#' Title
#'
#' @param sch_name
#' @param establish_con
#' @param tbl_name
#' @param col_name
#' @param col_comment
#'
#' @return
#' @export
#'
#' @examples
col_comment_olvr_rplc <- function(sch_name = NA
                                  ,establish_con = FALSE
                                  ,tbl_name = NA
                                  ,col_name = NA
                                  ,col_comment = NA){

  if(establish_con){
    suppressMessages(establish_con_olvr_rplc(set_schema = sch_name))
  }

  job_status <- DBI::dbSendQuery(con
                                 ,dbplyr::build_sql("comment on column "
                                                    ,ident(sch_name)
                                                    ,"."
                                                    ,ident(tbl_name)
                                                    ,"."
                                                    ,ident(col_name)
                                                    ," is "
                                                    ,col_comment))
}
