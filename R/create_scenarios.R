#' Create scenarios
#'
#' Combine arrivals_all, capacity, losA and init_conds
#' to create scenarios and arr_scenarios
#'
#' @param type string "scenarios" or NULL
#' @param arrivals_all dataframe
#' @param capacity dataframe
#' @param losA dataframe
#' @param init_conds dataframe
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr rename distinct mutate
#' @importFrom tidyr pivot_wider
#' @importFrom purrr reduce
#' @importFrom rlang .data
#'
#' @return scenarios dataframe
#' @export
create_scenarios <- function(type, arrivals_all, capacity,
                             losA, init_conds){
  arr <- arrivals_all %>% rename(sc_arr = .data$scenario)
  if (type == "scenarios") {
    arr <- arr %>% distinct(.data$node, .data$sc_arr, .keep_all = TRUE)
  }
  scenarios <- list(
    arr,
    capacity %>% rename(s_cap = .data$scenario),
    losA %>% rename(s_los = .data$scenario),
    init_conds) %>%
    reduce(merge, by = "node", all = TRUE) %>%
    mutate(S = paste0(.data$node, "_",  .data$s_cap, "_",
                      .data$s_los, "_", .data$sc_arr)) %>%
    pivot_wider(names_from = .data$measure, values_from = .data$value)
  return(scenarios)
}
