#' Title
#'
#' @param org_id
#' @param df_file
#' @param measure_col
#' @param org_col
#' @param round_plc
#' @param widget
#'
#' @return
#' @export
#'
#' @examples
sink_helper_single <- function(org_id
                               ,df_file = "chron_assigned_to_agreed.rds"
                               ,measure_col = "duration"
                               ,org_col = "id_provider_dim_pcv"
                               ,round_plc = 1
                               ,multiplier = 1
                               ,color = ""
                               ,widget = "gauge"
                               ,units = "Days"
                               ,primary_label = "for Days Until Visit is Expected to Occur (from Acceptance Date)"
                               ,secondary_label = "Until Visit is Expected to Occur (from Acceptance Date)") {

  measure <- measure_extract_single(df_file = df_file
                               ,measure_col = measure_col
                               ,org_col = org_col
                               ,org_id = org_id
                               ,round_plc = round_plc
                               ,primary_label = primary_label
                               ,secondary_label = secondary_label
                               ,units = units)

  if (widget == "valueBox") {
    ifelse(is.na(measure$value)
           , paste0('
                    ```{r}
                    valueBox(
                    "Metric Unavailable"
                    ,color = "'
                    ,color
                    ,'",caption = "'
                    ,measure$secondary_label
                    ,'")
                    ``` ')
           ,paste0('
                   ```{r}
                   valueBox(paste('
                   ,measure$value*multiplier
                   ,',"'
                   ,units
                   ,'")
                   ,color = "'
                   ,color
                   ,'",caption = "'
                   ,measure$primary_label
                   ,'")
                   ``` '))
  } else if (widget == "gauge") {

    ifelse(is.na(measure$value)
           ,paste0('
                    ```{r}
                    ``` ')
           ,paste0('
                   ```{r}
                   gauge('
                   ,measure$value*multiplier
                   ,', min = 0, max = 100, symbol = "%"
                   ,gaugeSectors(colors = "'
                   ,color
                   ,'"))
                   ``` '))
  }

}
