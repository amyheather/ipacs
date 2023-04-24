#' Create summary dataframe
#'
#' Create blank summary dataframe for visit-based simulation
#'
#' @param nrow Integer - number of rows
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' create_summary_df(nrow = 3)
create_summary_df <- function(nrow) {
  df <- data.frame(
    LOS = integer(),
    ISR = integer(),
    nruns = integer(),
    sim_length = integer(),
    warm_up = integer(),
    capacity = integer(),
    mean_wait = numeric(),
    q_length = numeric(),
    res_used = numeric(),
    res_idle = numeric(),
    in_sys = numeric()
  )
  df[nrow, ] <- NA
  return(df)
}
