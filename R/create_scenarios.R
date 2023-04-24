#' Create scenarios
#'
#' Combine arrivals_all, capacity, losA and init_conds to create scenarios and
#' arr_scenarios
#'
#' @param scenarios TRUE or FALSE. If set to TRUE, will find rows with
#' distinct node and sc_arr
#' @param arrivals_all Dataframe with arrivals information, imported from excel
#' @param capacity Dataframe with capacity information, imported from excel
#' @param losA Dataframe with length of stay information
#' @param init_conds Dataframe information regarding initial conditions
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr rename distinct mutate
#' @importFrom tidyr pivot_wider
#' @importFrom purrr reduce
#' @importFrom rlang .data
#'
#' @return Scenarios dataframe
#' @export
create_scenarios <- function(scenarios,
                             arrivals_all = parent.frame()$arrivals_all,
                             capacity = parent.frame()$capacity,
                             losA = parent.frame()$losA,
                             init_conds = parent.frame()$init_conds){
  # Rename arrivals, and if creating "scenarios", also filter to distinct rows
  arr <- arrivals_all %>% rename(sc_arr = "scenario")
  if (scenarios == TRUE) {
    arr <- arr %>% distinct(.data$node, .data$sc_arr, .keep_all = TRUE)
  }
  # Combine dataframes, create summary scenario column (S), and pivot wider
  df <- list(
    arr,
    capacity %>% rename(s_cap = "scenario"),
    losA %>% rename(s_los = "scenario"),
    init_conds) %>%
    reduce(merge, by = "node", all = TRUE) %>%
    mutate(S = paste0(.data$node, "_", .data$s_cap, "_",
                      .data$s_los, "_", .data$sc_arr)) %>%
    pivot_wider(names_from = "measure", values_from = "value")
  return(df)
}
