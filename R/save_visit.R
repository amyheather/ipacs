#' Save output from visit-based simulation
#'
#' Extract results for each date for number in queue, occupancy, wait and costs and then save to csv
#'
#' Extract results from visit-based simulation output for NIQ, OCC, wait and
#' costs, then append together, along with dates. Save this to a csv using the
#' output filename specified.
#'
#' @param visit_filename string - path to save output csv to
#' @param arr_rates_visit tbc
#' @param outcomes tbc
#' @param visits_based_output tbc
#'
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom utils write.csv
#'
#' @return saves csv to specified location
#' @export
save_visit <- function(visit_filename,
                       arr_rates_visit = parent.frame()$arr_rates_visit,
                       outcomes = parent.frame()$outcomes,
                       visits_based_output = parent.frame()$visits_based_output) {
  # Specify outcomes of interest
  outcomes <- c("niq", "occ", "wait", "cost")
  # Create list of dates
  ptvisits_list <- list(arr_rates_visit["date"])
  # Loop through outcomes
  for (i in seq_along(outcomes)){
    extract <- visits_based_output[c("day", "scen_", outcomes[i])] %>%
      pivot_wider(names_from = "scen_", values_from = as.name(outcomes[i])) %>%
      select(-.data$day)
    colnames(extract) <- paste0(colnames(extract), "__", outcomes[i])
    # First item is dates, so save at i+1
    ptvisits_list[[i + 1]] <- extract
  }
  means_output_v <- do.call("cbind", ptvisits_list)
  write.csv(means_output_v, visit_filename, row.names = FALSE)
}
