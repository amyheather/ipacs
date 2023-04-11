#' Create wait time dataframe
#'
#' Create blank dataframe to store wait times for visit-based simulation
#'
#' @return dataframe
#' @export
#'
#' @examples
#' create_wait_df()
create_wait_df <- function() {
  df <- data.frame(
    RUNX = integer(),
    day_ = integer(),
    scen_ = character(),
    start_service = integer(),
    waittime = integer(),
    stringsAsFactors = TRUE
  )
  return(df)
}
