#' Sample initial number of visits
#'
#' @description
#' Sample from normal distribution to find the number of visits required when
#' patient enters pathway.
#'
#' @details
#' Initial service rate (ISR) or initial visit rate (IVR) have a normal
#' distribution. Based on the provided mean and SD for that distribution,
#' samples to get ISR/IVR. It cannot be (a) less than 0, (b) more than
#' mean+SD*3, or (c) more than n_slots
#'
#' @param z string - scenario
#' @param isr float - mean for ISR distribution
#' @param sd_isr float - standard deviation for ISR distribution
#' @param n_slots float - number of visit slots available per day
#'
#' @importFrom stats rnorm
#'
#' @return x integer - ISR
#' @export
#'
#' @examples
#' dis_init_slots(z = "P1_B_BCap_Blos_BArr", isr = 4, sd_isr = 0.5, n_slots = 230)
dis_init_slots <- function(z = parent.frame()$z,
                           isr = parent.frame()$isr[z],
                           sd_isr = parent.frame()$sd_isr[z],
                           n_slots = parent.frame()$n_slots[z]) {
  # Sample from normal distribution with mean and SD provided
  x <- round(rnorm(n = 1, mean = isr, sd = sd_isr))
  max_v <- isr + (sd_isr * 3)
  # Check if it meets conditions - if not, sample again until it does
  while (x <= 0 || x > max_v || x > n_slots) {
    x <- round(rnorm(n = 1, mean = isr, sd = sd_isr))
  }
  return(x)
}
