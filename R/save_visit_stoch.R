#' Save stochastic output from visit-based simulation
#'
#' Correct date formatting, and save the quantiles to csv for optional
#' stochastic report, saving in long format for plotting
#'
#' @param visit_stoch_filename String - path to save output csv to
#' @param visits_based_output_q Dataframe with stochastic results from
#'  visit-based simulation
#'
#' @importFrom utils write.csv
#'
#' @return Saves csv to specified location
#' @export
save_visit_stoch <- function(
    visit_stoch_filename,
    visits_based_output_q = parent.frame()$visits_based_output_q) {
  # Correct date formatting
  visits_based_output_q$date <- as.Date(visits_based_output_q$date)
  # Save to CSV
  write.csv(visits_based_output_q, visit_stoch_filename, row.names = FALSE)
}
