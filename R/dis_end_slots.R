#' Sample end number of visits
#'
#' @description
#' Sample from normal distribution to find the number of visits required on
#' patient's final day in pathway.
#'
#' @details
#' End service rate (ESR) or final visit rate (FVR) have a normal
#' distribution. Based on the provided mean and SD for that distribution,
#' samples to get ESR/FVR. It cannot be (a) less than 0, or (b) more than
#' mean+SD*3.
#'
#' @param z String - scenario
#' @param end_sr Float - mean for ESR distribution - default from end_sr vector
#' @param sd_esr float - standard deviation for ESR distribution - default from
#'  sd_esr vector
#'
#' @importFrom stats rnorm
#'
#' @return x Integer - end service rate
#' @export
#'
#' @examples
#' dis_end_slots(z = "P1_B_BCap_Blos_BArr", end_sr = 1, sd_esr = 0.5)
dis_end_slots <- function(z = parent.frame()$z,
                          end_sr = parent.frame()$end_sr[z],
                          sd_esr = parent.frame()$sd_esr[z]) {
  # Sample from normal distribution with mean and SD provided
  x <- round(rnorm(1, mean = end_sr, sd = sd_esr))
  max_v <- end_sr + (sd_esr * 3)
  # Check if it meets conditions - if not, sample again until it does
  while (x <= 0 || x > max_v) {
    x <- round(rnorm(1, mean = end_sr, sd = sd_esr))
  }
  return(x)
}
