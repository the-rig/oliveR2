#' Title
#'
#' @param set_schema
#' @param connection_name
#'
#' @return
#' @export
#'
#' @examples
establish_con_olvr_rplc <- function(set_schema = 'staging', connection_name = "con"){
  lst <- ls()
  rpsql_cons <- lst[sapply(lst,function(var) any(class(get(var))=='PostgreSQLConnection'))]
  suppressWarnings(
    if (any(rpsql_cons==connection_name)) {
      con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                            dbname = Sys.getenv("OLIVER_REPLICA_DBNAME"),
                            host = Sys.getenv("OLIVER_REPLICA_HOST"),
                            port = Sys.getenv("OLIVER_REPLICA_PORT"),
                            user = Sys.getenv("OLIVER_REPLICA_USER"),
                            password = Sys.getenv("OLIVER_REPLICA_PASSWORD"))
      suppressMessages(
        absorb <- DBI::dbSendQuery(con, dbplyr::build_sql("SET search_path TO ", set_schema))
      )
      assign(connection_name, con, envir = .GlobalEnv)
      message(paste0("dropped connection and established new DBI connection named "
                     ,connection_name
                     ," - search_path set to "
                     ,set_schema))
    } else {
      con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                            dbname = Sys.getenv("OLIVER_REPLICA_DBNAME"),
                            host = Sys.getenv("OLIVER_REPLICA_HOST"),
                            port = Sys.getenv("OLIVER_REPLICA_PORT"),
                            user = Sys.getenv("OLIVER_REPLICA_USER"),
                            password = Sys.getenv("OLIVER_REPLICA_PASSWORD"))
      suppressMessages(
        absorb <- DBI::dbSendQuery(con, dbplyr::build_sql("SET search_path TO ", set_schema))
      )
      assign(connection_name, con, envir = .GlobalEnv)
      message(paste0("established new DBI connection named "
                     ,connection_name
                     ," - search_path set to "
                     ,set_schema))
    }
  )
}
