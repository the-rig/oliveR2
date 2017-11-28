#' Title
#'
#' @return
#' @export
#'
#' @examples
calc_na_contracts <- function() {
  network_contractors <- inner_join(tbl(con, "OrganizationContracts") %>%
                                      filter(contractOwnerId == 2
                                             ,is.na(deletedAt)) %>%
                                      arrange(desc(updatedAt)) %>%
                                      distinct(contractOwnerId
                                               ,contractedOrganizationId)
                                    ,tbl(con, "Organizations")
                                    ,by = c("contractedOrganizationId" = "id")
  ) %>%
    select(org_id = contractedOrganizationId
           ,name) %>%
    as_data_frame()

  file_path <- paste0(system.file('extdata'
                                  ,package = 'oliveR2')
                      ,'/'
                      ,"ref_lookup_network_contractors.rds"
  )

  readr::write_rds(network_contractors, file_path)
}

