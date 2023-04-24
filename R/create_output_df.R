#' Create output dataframe
#'
#' Create blank dataframe for output for visit-based simulation after warm-up
#'
#' @param nrow Integer - number of rows
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' create_output_df(nrow = 3)
create_output_df <- function(nrow) {
  df <- data.frame(
    RUNX = integer(), # Run number x
    node = character(), # Scenario
    day = integer(), # Output per day
    q_length = integer(), # Number of patients in queue
    n_slots_used = numeric(),
    patients_in_service = numeric(),
    res_used = numeric(), # Used slots
    res_idle = numeric(), # Idle slots
    in_sys = numeric()) # Number of patients in system
  df[nrow, ] <- NA
  return(df)
}
