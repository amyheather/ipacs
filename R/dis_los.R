#' Sample from length of stay (LOS) distribution
#'
#' @description
#' Calculates maximum LOS from mean and SD of LOS distribution.
#' If LOS is lnorm, sample from rlnorm distribution until you get a value
#' that is >=0 and <= the max LOS. Uses do.call() as we need to input
#' mean and SD for rlnorm() from a list
#'
#' @param z Integer - refers to the current scenario and location as the
#'  simulation loops through pathway_vector_visit, e.g. 4
#' @param los_dist String - the distribution type, e.g. "lnorm" - default is
#'  from srv_dist_visit
#' @param los_lnorm_mean_sd Float vector - the mean and SD for the lnorm
#'  distribution, e.g. c(1.52 1.32) - default is from srv_params_visit
#' @param los_norm_mean Float - the mean for the other distribution type, e.g.
#'  10.23 - default is from mean_los_visit
#' @param los_norm_sd Float - the SD for the other distribution type, e.g. 3 -
#'  default is from sd_los_visit
#'
#' @importFrom stats rlnorm rnorm
#'
#' @return as.integer(x) - the length of stay as an integer
#' @export
#'
#' @examples
#' dis_los(z = 1,
#'         los_dist = "lnorm",
#'         los_lnorm_mean_sd = c(2.197225, 1.180702),
#'         los_norm_mean = 18.07,
#'         los_norm_sd = 3)
dis_los <- function(z = parent.frame()$z,
                    los_dist = parent.frame()$srv_dist_visit[[z]],
                    los_lnorm_mean_sd = parent.frame()$srv_params_visit[[z]],
                    los_norm_mean = parent.frame()$mean_los_visit[[z]],
                    los_norm_sd = parent.frame()$sd_los_visit[[z]]){
  # Calculate maximum length of stay
  max_los <- los_norm_mean + los_norm_sd * 3

  # Sample from distribution (lnorm or rnorm) until you get a value
  # that is >=0 and <= the max LOS
  if (los_dist == "lnorm") {
    x <- round(do.call(rlnorm, c(list(1, los_lnorm_mean_sd))))
    while (x <= 0 || x >= max_los) {
      x <- round(do.call(rlnorm, c(list(1, los_lnorm_mean_sd))))
    }
  } else {
    x <- round(do.call(paste0("r", los_dist),
                       c(list(1, los_norm_mean, los_norm_sd)
                       )))
    while (x <= 0 || x >= max_los) {
      x <- round(do.call(paste0("r", los_dist),
                         c(list(1, los_norm_mean, los_norm_sd)
                         )))
    }
  }
  return(as.integer(x))
}
