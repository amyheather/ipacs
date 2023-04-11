#' Set-up for simulation
#'
#' Filters scenarios, arr_scenarios and costs to visit or bed pathways.
#' Extracts parameters from scenarios dataframe.
#' Pivots subset of arr_scenarios.
#' Creates vector with each scenario name.
#'
#' @param model_type string - either "visit" or "bed"
#' @param scenarios dataframe
#' @param arr_scenarios dataframe
#' @param costs dataframe
#' @param sd_los dataframe
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select arrange
#' @importFrom stringr str_detect
#' @importFrom tidyr separate
#'
#' @return list(return_new_names, return_objects)
#' @export
sim_setup <- function(model_type,
                      scenarios = parent.frame()$scenarios,
                      arr_scenarios = parent.frame()$arr_scenarios,
                      costs = parent.frame()$costs,
                      sd_los = parent.frame()$sd_los){
  # Set parameters depending on model type
  if (model_type == "visit") {
    pathway <- "P1"
  } else if (model_type == "bed") {
    pathway <- c("P2|P3")
  } else {
    warning("Function input should be 'visit' or 'bed'")
  }

  # Extract appropriate rows from scenarios, arr_scenarios and costs
  scenarios <- scenarios[str_detect(scenarios$node, pathway),]
  arr_scenarios <- arr_scenarios[str_detect(arr_scenarios$node, pathway),]
  costs <- costs[str_detect(costs$node, pathway),]

  # Extract parameters from scenarios dataframe
  init_occ <- as.list(scenarios$occ)
  init_niq <- as.list(scenarios$dtoc)
  srv_dist <- as.list(scenarios$los_dist)
  cap <- as.integer(as.list(scenarios$capacity))
  loss <- as.list(rep(0, nrow(scenarios)))

  # Parameters for sampling length of stay when have a log-normal distribution
  srv_params <- scenarios %>%
    separate(.data$los_params, into = c("mu", "sigma"), sep = ",", convert = TRUE) %>%
    select(.data$mu, .data$sigma) %>%
    unname() %>%
    t() %>%
    data.frame() %>%
    as.list()

  # Parameters for sampling length of stay when have a normal distribution
  mean_los <- as.list(scenarios$mean_los)
  sd_los <- as.list(rep(sd_los, nrow(scenarios)))

  # Select arrivals, date and scenario, then pivot so each row is a date
  # and arrivals on that date, with columns for each scenario
  arr_rates <- arr_scenarios %>%
    select(.data$arrivals, .data$date, .data$S) %>%
    pivot_wider(names_from = "S", values_from = "arrivals") %>%
    arrange(.data$date) %>%
    as.data.frame()

  # Create vector with each scenario name (dput is just to print to screen)
  pathway_vector <- dput(colnames(arr_rates %>% select(-.data$date)))

  # Create list of names objects to return, use mget() to get the objects,
  # and create list with names specific to the model type
  return_names <- c("scenarios", "costs",
                    "init_occ", "init_niq", "srv_dist", "cap", "loss",
                    "srv_params", "mean_los", "sd_los",
                    "arr_rates", "pathway_vector")
  return_objects <- mget(return_names)
  return_new_names <- paste0(return_names, "_", model_type)
  return(list(return_new_names, return_objects))
}
