#' Title
#'
#' @return
#' @export
#'
#' @examples
kill_pg_connections <- function () {

  all_cons <- dbListConnections(PostgreSQL())

  for(con in all_cons) {
    out <- dbDisconnect(con)
  }

  message(paste0(length(all_cons), " connections killed."))

}
