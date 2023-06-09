#' Calculate mu and sigma
#'
#' Calculates mu and sigma, and stores both within los_params, within the
#' length of stay dataframe (losA).
#'
#' @param est_method Integer - indicates estimation method to use, either 1 or 2
#' @param losA Dataframe - imported from excel, contains length of stay
#'  parameters
#' @param sd_los Float - standard deviation for length of stay distribution
#'
#' @return losA dataframe with new mu, sigma and los_params columns.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' losA <- calculate_mu_sigma(est_method = 1, losA = losA, sd_los = 3)
#' }
calculate_mu_sigma <- function(est_method = parent.frame()$est_method,
                               losA = parent.frame()$losA,
                               sd_los = parent.frame()$sd_los){
  # Delete columns if they already exist
  losA <- losA[, -which(names(losA) %in% c("mu", "sigma", "los_params"))]

  # This was the method being used within the excel spreadsheet
  # And hence, outputs used for testing_and_linting are from this method
  if (est_method == 1) {
    losA["mu"] <- log(losA["median"])
    losA["sigma"] <- sqrt(2*(log(losA["mean_los"])-losA["mu"]))
    losA["los_params"] <- with(losA, paste(mu, sigma, sep=" , "))

  # This was method sent from Alison
  # It is also the same as the method available online at the following blog:
  # https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/comment-page-1/
  } else if (est_method == 2) {
    gen_mu <- function(mean, stdv) {
      phi <- (stdv ^ 2 + mean ^ 2) ^ 0.5
      mu <- log(mean ^ 2 / phi)
      return (mu)
    }
    gen_sigma <- function(mean, stdv) {
      phi <- (stdv ^ 2 + mean ^ 2) ^ 0.5
      mu <- log(mean ^ 2 / phi)
      sigma <- (log(phi ^ 2 / mean ^ 2)) ^ 0.5
      return (sigma)
    }
    losA$mu <- lapply(losA$mean_los, function(x) gen_mu(x, sd_los))
    losA$sigma <- lapply(losA$mean_los, function(x) gen_sigma(x, sd_los))
    losA$los_params <- with(losA, paste(mu, sigma, sep=" , "))

  # Stop and return error method is have not set est_method
  } else {
    stop("est_method should be equal to 1 or 2")
  }

  # Return the updated losA dataframe
  return(losA)
}

