#' Import file (test function)
#'
#' @param filename string
#' @param sheet string
#'
#' @importFrom readxl read_excel
#'
#' @return file dataframe
#'
#' @export
import_file <- function(filename, sheet){
  file <- readxl::read_excel(filename, sheet=sheet)
  return(file)
}
