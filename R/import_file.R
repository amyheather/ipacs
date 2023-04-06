#' Imports the sheets from the excel file with the model parameters
#' File should contain sheets "arrivals", "initial conditions", "capacity",
#' "los" and "costs"
#'
#' @param filename string
#'
#' @importFrom readxl read_excel
#'
#' @return list(df_names, df_list)
#'
#' @export
import_file <- function(filename){
  sheets <- c("arrivals", "initial conditions", "capacity", "los", "costs")
  df_names <- c("arrivals_all", "init_conds", "capacity", "losA", "costs")
  df_list <- lapply(sheets, function(x) readxl::read_excel(filename, sheet = x))
  return(list(df_names, df_list))
}
