#' Title
#'
#' @param libname
#' @param pkgname
#'
#' @return
#' @export
#'
#' @examples
.onAttach <- function(libname, pkgname) {

  get_oliver_replica_dbname <- function() {
    default <- "oliver_replica"
    OLIVER_REPLICA_DBNAME <- Sys.getenv("OLIVER_REPLICA_DBNAME")
    if(nchar(OLIVER_REPLICA_DBNAME) == 0){
      if (interactive()) {
        y <- readline(paste0("OLIVER_REPLICA_DBNAME environment variable not set; assuming, "
                             ,default
                             ,".\n"
                             ,"Enter an alternative and press Enter, or just Hit Enter/Return to continue: ")
        )
      }
      Sys.setenv(OLIVER_REPLICA_DBNAME = ifelse(y == '', default, y))
    }
  }

  get_oliver_replica_host <- function() {
    default <- "localhost"
    OLIVER_REPLICA_HOST <- Sys.getenv("OLIVER_REPLICA_HOST")
    if(nchar(OLIVER_REPLICA_HOST) == 0){
      if (interactive()) {
        y <- readline(paste0("OLIVER_REPLICA_HOST environment variable not set; assuming, "
                             ,default
                             ,".\n"
                             ,"Enter an alternative and press Enter, or just Hit Enter/Return to continue: ")
        )
      }
      Sys.setenv(OLIVER_REPLICA_HOST = ifelse(y == '', default, y))
    }
  }

  get_oliver_replica_port <- function() {
    default <- "5433"
    OLIVER_REPLICA_PORT <- Sys.getenv("OLIVER_REPLICA_PORT")
    if(nchar(OLIVER_REPLICA_PORT) == 0){
      if (interactive()) {
        y <- readline(paste0("OLIVER_REPLICA_PORT environment variable not set; assuming, "
                             ,default
                             ,".\n"
                             ,"Enter an alternative and press Enter, or just Hit Enter/Return to continue: ")
        )
      }
      Sys.setenv(OLIVER_REPLICA_PORT = ifelse(y == '', default, y))
    }
  }

  get_oliver_replica_user <- function() {
    default <- Sys.getenv("USER")
    OLIVER_REPLICA_USER <- Sys.getenv("OLIVER_REPLICA_USER")
    if(nchar(OLIVER_REPLICA_USER) == 0){
      if (interactive()) {
        y <- readline(paste0("OLIVER_REPLICA_USER environment variable not set; assuming, "
                             ,default
                             ,".\n"
                             ,"Enter an alternative and press Enter, or just Hit Enter/Return to continue: ")
        )
      }
      Sys.setenv(OLIVER_REPLICA_USER = ifelse(y == '', default, y))
    }
  }

  get_oliver_replica_password <- function() {
    default <- ""
    OLIVER_REPLICA_PASSWORD <- Sys.getenv("OLIVER_REPLICA_PASSWORD")
    if(nchar(OLIVER_REPLICA_PASSWORD) == 0){
      if (interactive()) {
        y <- readline(paste0("OLIVER_REPLICA_PASSWORD environment variable not set. \n"
                             ,"Enter a password and press Enter, or just Hit Enter/Return to continue: ")
        )
      }
      Sys.setenv(OLIVER_REPLICA_PASSWORD = ifelse(y == '', default, y))
    }
  }

  get_oliver_replica_jitter <- function() {
    default <- TRUE
    OLIVER_REPLICA_JITTER <- Sys.getenv("OLIVER_REPLICA_JITTER")
    if(nchar(OLIVER_REPLICA_JITTER) == 0){
      if (interactive()) {
        y <- readline(paste0("OLIVER_REPLICA_JITTER environment variable not set; assuming, "
                             ,default
                             ,".\n"
                             ,"Enter an alternative and press Enter, or just Hit Enter/Return to continue: ")
        )
      }
      Sys.setenv(OLIVER_REPLICA_JITTER = ifelse(y == '', default, y))
    }
  }


  get_oliver_replica_dbname()
  get_oliver_replica_port()
  get_oliver_replica_host()
  get_oliver_replica_jitter()
  get_oliver_replica_password()
  get_oliver_replica_user()

}
