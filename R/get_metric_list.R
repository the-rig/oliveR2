#' Title
#'
#' @param group_id
#'
#' @return
#' @export
#'
#' @examples
get_metric_list <- function(group_id){
  suppressWarnings(get_ppm_metrics(org_id = group_id))
}

