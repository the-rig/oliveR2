#' Title
#'
#'
#' @return
#' @export
#'
#' @examples
sink_portable_dashboard <- function(){

  message("* BUILD PORTABLE DASHBOARDS *")

  file_path2 <- paste0(system.file('extdata'
                                   ,package = 'oliveR2')
                       ,'/'
                       ,"ref_lookup_network_contractors.rds"
  )

  df_orgs <- readr::read_rds(file_path2)

  file_path3 <- paste0(system.file('extdata'
                                   ,package = 'oliveR2')
                       ,'/'
  )

  sink(paste0(file_path3, "pcv_report.Rmd"))

  cat("---
title: \"Parent-Child Visitation Portable Dashboard\"
output:
 flexdashboard::flex_dashboard:
      theme: lumen
      keep_md: true
      self_contained: true
---
")
cat("Days to Receipt {data-navmenu=\"Network-Level Stats\"}
=====================================
")
cat('Row {data-height=600, data-width=300} \n')
cat('-------------------------------------')
cat("\n")
cat("\n")
cat("###")
cat("\n")

cat(sink_helper_single(org_id = NA
                       ,org_col = NA
                       ,df_file = "chron_network_created_to_received.rds"
                       ,measure_col = "measure_value"
                       ,round_plc = 1
                       ,units = " Days"
                       ,primary_label = "Until Visit is Received (from Creation Date)"
                       ,secondary_label = "for Days Until Visit is Received (from Creation Date))"
                       ,widget = "valueBox"
                       ,color = "#73A404"))

cat("\n")
cat("\n")
cat("### 1-Day Receipt Rate (from Creation Date)")
cat("\n")

cat(sink_helper_single(org_id = NA
                       ,org_col = NA
                       ,df_file = "chron_network_created_to_received.rds"
                       ,measure_col = "fl_met_goal"
                       ,round_plc = 2
                       ,multiplier = 100
                       ,color = "#73A404"
                       ,widget = "gauge"))

cat("\n")
cat("\n")
cat("### Receipt Trends")
cat("\n")



cat(paste0('
           ```{r}
           library(dygraphs)

           `from Creation` <- measure_extract_time_series(df_file = "chron_long_created_to_received.rds"
           ,org_col = NA
           ,org_id = NA)

           dygraph(`from Creation`, main = "Metric Trends (Days to Receipt from Creation)") %>%
           dyOptions(colors = c("#73A404"), pointSize = 3, strokeWidth = 3,
           connectSeparatedPoints = TRUE) %>%
           dyEvent("2017-09-20", "Timeline Release", labelLoc = "top") %>%
           dyLegend(show = "onmouseover", hideOnMouseOut = TRUE, width = 400)
           ``` '))

cat("\n")
cat("\n")

cat("Days to Acceptance {data-navmenu=\"Network-Level Stats\"}
=====================================
    ")
cat('Row {data-height=600, data-width=300} \n')
cat('-------------------------------------')
cat("\n")
cat("\n")
cat("###")
cat("\n")

cat(sink_helper_single(org_id = NA
                       ,org_col = NA
                       ,df_file = "chron_network_received_to_assigned.rds"
                       ,measure_col = "measure_value"
                       ,round_plc = 1
                       ,units = " Days"
                       ,primary_label = "Until Visit is Accepted (from Receipt Date)"
                       ,secondary_label = "for Days Until Visit is Accepted (from Receipt Date))"
                       ,widget = "valueBox"
                       ,color = "#A43504"))

cat("\n")
cat("\n")
cat("### 1-Day Acceptance Rate (from Receipt Date)")
cat("\n")

cat(sink_helper_single(org_id = NA
                       ,org_col = NA
                       ,df_file = "chron_network_received_to_assigned.rds"
                       ,measure_col = "fl_met_goal"
                       ,round_plc = 2
                       ,multiplier = 100
                       ,color = "#A43504"
                       ,widget = "gauge"))

cat("\n")
cat("\n")
cat("###")
cat("\n")

cat(sink_helper_single(org_id = NA
                       ,org_col = NA
                       ,df_file = "chron_network_created_to_assigned.rds"
                       ,measure_col = "measure_value"
                       ,round_plc = 1
                       ,units = " Days"
                       ,primary_label = "Until Visit is Accepted (from Creation Date)"
                       ,secondary_label = "for Days Until Visit is Accepted (from Creation Date))"
                       ,widget = "valueBox"
                       ,color = "#A27D11"))

cat("\n")
cat("\n")
cat("### 1-Day Acceptance Rate (from Creation Date)")
cat("\n")

cat(sink_helper_single(org_id = NA
                       ,org_col = NA
                       ,df_file = "chron_network_created_to_assigned.rds"
                       ,measure_col = "fl_met_goal"
                       ,round_plc = 2
                       ,multiplier = 100
                       ,color = "#A27D11"
                       ,widget = "gauge"))

cat("\n")
cat("\n")

cat('Row {data-height=400} \n')
cat('-------------------------------------')
cat("\n")
cat("\n")
cat("### Acceptance Trends")
cat("\n")



cat(paste0('
           ```{r}
           library(dygraphs)

           `from Receipt` <- measure_extract_time_series(df_file = "chron_long_received_to_assigned.rds"
           ,org_col = NA
           ,org_id = NA)

           `from Creation` <- measure_extract_time_series(df_file = "chron_long_created_to_assigned.rds"
           ,org_col = NA
           ,org_id = NA)

           performance_ts <- cbind(`from Receipt`
           ,`from Creation`)
           dygraph(performance_ts, main = "Metric Trends (Days to Acceptance)") %>%
           dyOptions(colors = c("#A43504", "#A27D11"), pointSize = 3, strokeWidth = 3,
           connectSeparatedPoints = TRUE) %>%
           dyEvent("2017-09-20", "Timeline Release", labelLoc = "top") %>%
           dyLegend(show = "onmouseover", hideOnMouseOut = TRUE, width = 400)
           ``` '))

cat("\n")
cat("\n")
cat("###")
cat("\n")

cat(paste0('
           ```{r}
           library(dygraphs)

           New <- ts(rnorm(8, 15, 2), start = c(2016, 10), end = c(2017, 10), frequency = 12)

           dygraph(New, main = "New Referrals (Simulated Data)") %>%
           dyOptions(colors = c("#B58900"), pointSize = 3, strokeWidth = 3)
           ``` '))

cat("\n")
cat("\n")

cat("Days to Schedule {data-navmenu=\"Network-Level Stats\"}
=====================================
    ")
cat('Row {data-height=600, data-width=300} \n')
cat('-------------------------------------')
cat("\n")
cat("\n")
cat("###")
cat("\n")

cat(sink_helper_single(org_id = NA
                       ,org_col = NA
                       ,df_file = "chron_network_created_to_agreed.rds"
                       ,measure_col = "measure_value"
                       ,round_plc = 1
                       ,units = " Days"
                       ,primary_label = "Until Visit is Scheduled (from Creation Date)"
                       ,secondary_label = "for Days Until Visit is Scheduled (from Creation Date))"
                       ,widget = "valueBox"
                       ,color = "#0473A4"))

cat("\n")
cat("\n")
cat("### 3-Day Scheduling Rate (from Creation Date)")
cat("\n")

cat(sink_helper_single(org_id = NA
                       ,org_col = NA
                       ,df_file = "chron_network_created_to_agreed.rds"
                       ,measure_col = "fl_met_goal"
                       ,round_plc = 2
                       ,multiplier = 100
                       ,color = "#0473A4"
                       ,widget = "gauge"))

cat("\n")
cat("\n")
cat("###")
cat("\n")

cat(sink_helper_single(org_id = NA
                       ,org_col = NA
                       ,df_file = "chron_network_received_to_agreed.rds"
                       ,measure_col = "measure_value"
                       ,round_plc = 1
                       ,units = " Days"
                       ,primary_label = "Until Visit is Scheduled (from Receipt Date)"
                       ,secondary_label = "for Days Until Visit is Scheduled (from Receipt Date))"
                       ,widget = "valueBox"
                       ,color = "#0416A4"))

cat("\n")
cat("\n")
cat("### 3-Day Scheduling Rate (from Receipt Date)")
cat("\n")

cat(sink_helper_single(org_id = NA
                       ,org_col = NA
                       ,df_file = "chron_network_received_to_agreed.rds"
                       ,measure_col = "fl_met_goal"
                       ,round_plc = 2
                       ,multiplier = 100
                       ,color = "#0416A4"
                       ,widget = "gauge"))

cat("\n")
cat("\n")

cat('Row {data-height=400} \n')
cat('-------------------------------------')
cat("\n")
cat("\n")
cat("### Scheduling Trends")
cat("\n")



cat(paste0('
           ```{r}
           library(dygraphs)

           `from Receipt` <- measure_extract_time_series(df_file = "chron_long_received_to_agreed.rds"
                            ,org_col = NA
           ,org_id = NA)

           `from Creation` <- measure_extract_time_series(df_file = "chron_long_created_to_agreed.rds"
                            ,org_col = NA
           ,org_id = NA)

           performance_ts <- cbind(`from Receipt`
           ,`from Creation`)
           dygraph(performance_ts, main = "Metric Trends (Days to Schedule)") %>%
           dyOptions(colors = c("#0423A4", "#0473A4"), pointSize = 3, strokeWidth = 3,
           connectSeparatedPoints = TRUE) %>%
dyEvent("2017-09-20", "Timeline Release", labelLoc = "top") %>%
dyLegend(show = "onmouseover", hideOnMouseOut = TRUE, width = 400)
           ``` '))

cat("\n")
cat("\n")
cat("###")
cat("\n")

cat(paste0('
           ```{r}
           library(dygraphs)

           New <- ts(rnorm(8, 15, 2), start = c(2016, 10), end = c(2017, 10), frequency = 12)

           dygraph(New, main = "New Referrals (Simulated Data)") %>%
           dyOptions(colors = c("#B58900"), pointSize = 3, strokeWidth = 3)
           ``` '))

cat("\n")
cat("\n")

cat("Case Composition {data-navmenu=\"Trends and Demographics\"}
=====================================
### This section of the portable dashboard is under development. Please see the **Provider-Level Scheduling Stats** for a prototype of this product.

Refferal Trends {data-navmenu=\"Trends and Demographics\"}
=====================================
### This section of the portable dashboard is under development. Please see the **Provider-Level Scheduling Stats** for a prototype of this product.
")
  for(org_id in df_orgs$org_id){
    cat("\n")
    cat('\n', as.character(df_orgs[df_orgs$org_id == org_id,"name"]), '{data-orientation=rows, data-navmenu="Provider-Level Scheduling Stats"} \n')
    cat("=====================================")
    cat("\n")
    cat('Row {data-height=600, data-width=300} \n')
    cat('-------------------------------------')
    cat("\n")
    cat("\n")
    cat("###")
    cat("\n")

    cat(sink_helper_single(org_id = org_id
                           ,df_file = "chron_assigned_to_agreed.rds"
                           ,measure_col = "measure_value"
                           ,org_col = "id_provider_dim_pcv"
                           ,round_plc = 1
                           ,widget = "valueBox"
                           ,units = " Days"
                           ,color = "#0416A4"
                           ,primary_label = "Until Visit is Scheduled (from Acceptance Date)"
                           ,secondary_label = "for Days Until Visit is Scheduled (from Acceptance Date)"))

    cat("\n")
    cat("\n")
    cat("### 3-Day Scheduling Rate (from Acceptance Date)")
    cat("\n")

    cat(sink_helper_single(df_file = "chron_assigned_to_agreed.rds"
                           ,measure_col = "fl_met_goal"
                           ,org_col = "id_provider_dim_pcv"
                           ,round_plc = 2
                           ,multiplier = 100
                           ,color = "#0416A4"
                           ,widget = "gauge"
                           ,org_id = org_id))

    cat("\n")
    cat("\n")
    cat("###")
    cat("\n")

    cat(sink_helper_single(df_file = "chron_assigned_to_scheduled.rds"
                       ,measure_col = "measure_value"
                       ,org_col = "id_provider_dim_pcv"
                       ,round_plc = 1
                       ,color = "#771086"
                       ,widget = "valueBox"
                       ,units = " Days"
                       ,primary_label = "for Days Until Visit is Expected to Occur (from Acceptance Date)"
                       ,secondary_label = "Until Visit is Expected to Occur (from Acceptance Date)"
                       ,org_id = org_id))

    cat("\n")
    cat("\n")
    cat("### 7-Day Expectation Rate (from Acceptance Date)")
    cat("\n")

    cat(sink_helper_single(df_file = "chron_assigned_to_scheduled.rds"
                           ,measure_col = "fl_met_goal"
                           ,org_col = "id_provider_dim_pcv"
                           ,round_plc = 2
                           ,multiplier = 100
                           ,color = "#771086"
                           ,widget = "gauge"
                           ,org_id = org_id))

    cat("\n")
    cat("\n")
    cat("###")
    cat("\n")

    cat(sink_helper_single(df_file = "chron_assigned_to_inprogress.rds"
                           ,measure_col = "measure_value"
                           ,org_col = "id_provider_dim_pcv"
                           ,round_plc = 1
                           ,color = "#FC4F1E"
                           ,widget = "valueBox"
                           ,units = " Days"
                           ,primary_label = "for Days Until Visit is In-Progress (from Acceptance Date)"
                           ,secondary_label = "Until Visit is In-Progress (from Acceptance Date)"
                           ,org_id = org_id))

    cat("\n")
    cat("\n")

    cat("\n")
    cat("\n")
    cat("### 7-Day In-Progress Rate (from Acceptance Date)")
    cat("\n")

    cat(sink_helper_single(df_file = "chron_assigned_to_inprogress.rds"
                           ,measure_col = "fl_met_goal"
                           ,org_col = "id_provider_dim_pcv"
                           ,round_plc = 2
                           ,multiplier = 100
                           ,color = "#FC4F1E"
                           ,widget = "gauge"
                           ,org_id = org_id))

    cat("\n")
    cat("\n")

    cat('Row {data-height=400} \n')
    cat('-------------------------------------')
    cat("\n")
    cat("\n")
    cat(paste0("### ", as.character(df_orgs[df_orgs$org_id == org_id,'name'])))
    cat("\n")



    cat(paste0('
               ```{r}
library(dygraphs)

  Inprogress <- measure_extract_time_series(df_file = "chron_long_assigned_to_inprogress.rds"
                              ,org_col = "id_provider_dim_pcv"
               ,org_id ='
               ,org_id
               ,')
  Schedule <- measure_extract_time_series(df_file = "chron_long_assigned_to_scheduled.rds"
               ,org_col = "id_provider_dim_pcv"
               ,org_id ='
               ,org_id
               ,')
  Expected <- measure_extract_time_series(df_file = "chron_long_assigned_to_agreed.rds"
               ,org_col = "id_provider_dim_pcv"
               ,org_id ='
               ,org_id
               ,')
               performance_ts <- cbind(Schedule
               ,Expected
               ,Inprogress)
  dygraph(performance_ts, main = "Metric Trends") %>%
               dyOptions(colors = c("#0416A4", "#771086", "#FC4F1E"), pointSize = 3, strokeWidth = 3,
connectSeparatedPoints = TRUE) %>%
dyEvent("2017-09-20", "Timeline Release", labelLoc = "top") %>%
dyLegend(show = "onmouseover", hideOnMouseOut = TRUE, width = 400)
               ``` '))

    cat("\n")
    cat("\n")
    cat("###")
    cat("\n")

    cat(paste0('
               ```{r}
library(dygraphs)

               New <- ts(rnorm(8, 15, 2), start = c(2016, 10), end = c(2017, 10), frequency = 12)

  dygraph(New, main = "New Referrals (Simulated Data)") %>%
    dyOptions(colors = c("#B58900"), pointSize = 3, strokeWidth = 3)
               ``` '))

  }
  for(org_id in df_orgs$org_id){
    cat("\n")
    cat('\n', as.character(df_orgs[df_orgs$org_id == org_id,"name"]), '{data-orientation=rows, data-navmenu="Provider-Level Report Stats"} \n')
    cat("=====================================")
    cat("\n")
    cat('Row {data-height=600, data-width=300} \n')
    cat('-------------------------------------')
    cat("\n")
    cat("\n")
    cat("###")
    cat("\n")

    cat(sink_helper_single(org_id = org_id
                           ,df_file = "prop_missed_visit.rds"
                           ,measure_col = "measure_value"
                           ,org_col = "id_provider_dim_pcv"
                           ,round_plc = 2
                           ,multiplier = 100
                           ,widget = "valueBox"
                           ,units = "%"
                           ,color = "#FC4A1A"
                           ,primary_label = "24-Hour Cancellation Rate"
                           ,secondary_label = "for 24-Hour Cancellation Rate"))

    cat("\n")
    cat("\n")
    cat("###")
    cat("\n")

    cat(sink_helper_single(org_id = org_id
                           ,df_file = "prop_missed_visit_by_pvd.rds"
                           ,measure_col = "measure_value"
                           ,org_col = "id_provider_dim_pcv"
                           ,round_plc = 2
                           ,multiplier = 100
                           ,widget = "valueBox"
                           ,units = "%"
                           ,color = "#F7B733"
                           ,primary_label = "Among 24-Hour Cancellations, Rate of Provider Cause"
                           ,secondary_label = "for Provider-Caused 24-Hour Cancellation Rate"))

    cat("\n")
    cat("\n")
    cat('Row {data-height=400} \n')
    cat('-------------------------------------')
    cat("\n")
    cat("\n")
    cat(paste0("### ", as.character(df_orgs[df_orgs$org_id == org_id,'name'])))
    cat("\n")

    cat(paste0('
               ```{r}
               library(dygraphs)
               `Provider Cancels` <- measure_extract_time_series(df_file = "prop_long_missed_visit.rds"
               ,org_col = "id_provider_dim_pcv"
               ,org_id ='
               ,org_id
               ,')*100

               Cancels <- measure_extract_time_series(df_file = "prop_long_missed_visit_by_pvd.rds"
               ,org_col = "id_provider_dim_pcv"
               ,org_id ='
               ,org_id
               ,')*100

           performance_ts <- cbind(Cancels
               ,`Provider Cancels`)

               dygraph(performance_ts, main = "Metric Trends (24-Hour Cancellation Rates)") %>%
               dyOptions(colors = c("#FC4A1A", "#F7B733"), pointSize = 3, strokeWidth = 3,
               connectSeparatedPoints = TRUE) %>%
               dyLegend(show = "onmouseover", hideOnMouseOut = TRUE, width = 400)
               ``` '))

    cat("\n")
    cat("\n")

#     cat("
#     ### Average Number of Children on Accepted Referral
#     ### Visit Reports
#     ### Visit Attendance Rate
#     ### Visit No Show Rate
#     ### Visit Canceled Rate (including both greater than 24 hrs and less than 24 hrs)
# ")
#     cat("\n")
#     cat("#### Please see the **Provider-Level Scheduling Stats** for a prototype of this product.")
  }
  sink()

  file_path4 <- paste0(system.file('extdata'
                                   ,package = 'oliveR2')
                       ,'/'
  )

  rmarkdown::render(paste0(file_path4, "pcv_report.Rmd"), "all")

  # reset sink

  for(i in seq_len(sink.number())){
    sink(NULL)
  }

      }
