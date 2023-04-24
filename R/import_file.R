#' Import simulation excel file
#'
#' Imports each sheet from the excel file that contain parameters for the
#' simulation. File should contain sheets "arrivals", "initial conditions",
#' "capacity", "los" and "costs". Returns these as dataframes, along with
#' list of dataframe names.
#'
#' @param filename String - path to excel file
#'
#' @importFrom readxl read_excel
#'
#' @return List of length 2. Contains list of dataframe names and list of
#' dataframes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' import_file("ipacs_model_parameters.xlsx")
#' }
import_file <- function(filename){
  sheets <- c("arrivals", "initial conditions", "capacity", "los", "costs")
  df_names <- c("arrivals_all", "init_conds", "capacity", "losA", "costs")
  df_list <- lapply(sheets, function(x) readxl::read_excel(filename, sheet = x))
  return(list(df_names, df_list))
}
