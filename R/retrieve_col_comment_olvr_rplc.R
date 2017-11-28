#' Title
#'
#' @param sch_name
#' @param establish_con
#' @param tbl_name
#' @param col_idx
#'
#' @return
#' @export
#'
#' @examples
retrieve_col_comment_olvr_rplc <- function(sch_name = NA
                                  ,establish_con = FALSE
                                  ,tbl_name = NA
                                  ,col_idx = NA){

DBI::dbGetQuery(con, dbplyr::build_sql("
(SELECT
  distinct col_description((SELECT
                            distinct attrelid
                            FROM pg_attribute
                            WHERE attrelid = (SELECT oid FROM pg_class WHERE relname = 'calendar_dim')),"
,col_idx
,")
                                              FROM pg_class
                                              WHERE relkind = 'r'"))
}
