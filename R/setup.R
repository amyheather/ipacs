#' Load setup
#'
#' @param input_filename A string (name of file with model inputs)
#'
#' @return Runs setup.R script from inst/scripts
#'
#' @import tidyverse
#' @importFrom readxl read_excel
#'
#' @export
load_setup <- function(input_filename) {
  source(system.file(package = "ipacs", "scripts/setup.R"))
}
