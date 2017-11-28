#' Title
#'
#' @param df_file
#' @param measure_col
#' @param org_col
#' @param org_id
#' @param round_plc
#' @param units
#' @param primary_label
#' @param secondary_label
#' @param tertiary_label
#'
#' @return
#' @export
#'
#' @examples
measure_extract_single <- function(df_file
                                   ,measure_col
                                   ,org_col
                                   ,org_id
                                   ,round_plc = 0
                                   ,units = "Days"
                                   ,primary_label = ""
                                   ,secondary_label = ""
                                   ,tertiary_label = NA) {

  file_path <- paste0(system.file('extdata'
                                  ,package = 'oliveR2')
                      ,'/'
  )

  df <- readr::read_rds(paste0(file_path, df_file))

  if (all(is.na(org_col), is.na(org_id))) {

    value <- df %>%
      select(measure_col) %>%
      as.numeric() %>%
      round(round_plc)

  } else {

    org_id_lazy <- interp(~y == x, .values=list(y = as.name(org_col), x = org_id))

    value <- df %>%
      filter_(org_id_lazy) %>%
      select(measure_col) %>%
      as.numeric() %>%
      round(round_plc)

  }

    tibble(value, units, primary_label, secondary_label)

}
