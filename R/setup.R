#' Load setup
#'
#' @return Runs setup.R script from inst/scripts
#' @export
#'
#' @examples
#' load_setup()
load_setup <- function() {
  source(system.file(package = "ipacs", "scripts/setup.R"))
}
