#' Shorten length of stay
#'
#' Reduces length of stay for patients already in service
#'
#' Samples from uniform distribution between 1 and the length of stay
#' (produced by dis_los()). Rounds to nearest integer. Assumption: All patients
#' stay at least 1 day.
#'
#' @param long_los float - length of stay (output by dis_los())
#'
#' @importFrom stats runif
#'
#' @return Integer - shortened length of stay
#' @export
#'
#' @examples
#' dis_los2(18.34)
dis_los2 <- function(long_los) {
  x <- round(runif(n = 1, min = 1, max = long_los))
  return(as.integer(x))
}
