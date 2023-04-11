#' Create patient dataframe
#'
#' Create blank dataframe to populate for patients in visit-based simulation
#'
#' @param nrow integer - number of rows
#'
#' @return dataframe
#' @export
#'
#' @examples
#' create_patient_df(nrow = 3)
create_patient_df <- function(nrow) {
  df <- data.frame(
    id = integer(), # Patient ID
    los = integer(), # Length of stay
    arrival_time = integer(), # Day in the simulation the entity arrived
    start_service = integer(), # Day actual service started
    end_service = integer(), # Day service ended
    wait_time = integer(), # Number of days spent in the queue
    exit = logical(), # Boolean variable, TRUE if the entity has left the system
    stringsAsFactors = FALSE)
  df[nrow, ] <- NA
  return(df)
}
