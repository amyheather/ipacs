# Set up ----------------------------------------------------------------------
# Sim_length, scenarios and arr_scenarios are used for visit based and
# bed based simulation, so set-up in a separate script

# Name of file with model parameters (should be stored in model_inputs folder)
input_filename <- "IPACS_20230214_fix.xlsx"

# Set manual parameters
sd_los <- 3 # estimate of standard deviation for length of stay distribution
nruns <- as.integer(5) # number of runs for each simulation
sd_isr <- 0.5 # initial service/visit rate
sd_esr <- 0.5 # end service rate/final visit rate
temp_seed <- 1
warmup <- 0

# Choose method for estimation of mu and sigma (either 1 or 2)
# See set_up.R for the difference between the methods
est_method <- 1

# Create list of (1) dataframes to create, and (2) sheets to import from
# Then import each sheet and save to the relevant dataframe
input_list <- list(c("arrivals_all", "arrivals"),
                   c("init_conds", "initial conditions"),
                   c("capacity", "capacity"),
                   c("losA", "los"),
                   c("costs", "costs"))
for (x in input_list){
  assign(x[1], readxl::read_excel(
    system.file(package = "ipacs", paste0("model_inputs/", input_filename)),
    sheet = x[2]))
}

# Create mu, sigma and los_params columns
# Filter dataframe (to remove those columns if already exist)
losA[, -which(names(losA) %in% c("mu", "sigma", "los_params"))]
if (est_method == 1) {
  # This was the method being used within the excel spreadsheet
  # And hence, outputs used for testing_and_linting are from this method
  losA["mu"] <- log(losA["median"])
  losA["sigma"] <- sqrt(2*(log(losA["mean_los"])-losA["mu"]))
  losA["los_params"] <- with(losA, paste(mu, sigma, sep=" , "))
} else if (est_method == 2) {
  # This was method sent from Alison
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
  # This is the same as the commented method below
  # Source: https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/comment-page-1/
  # gen_mu <- function(mean, stdv) {
  #   mu <- log(mean ^ 2 / sqrt(stdv ^ 2 + mean ^ 2))
  #   return (mu)
  # }
  # gen_sigma <- function(mean, stdv) {
  #   sigma <- sqrt(log(1 + (stdv ^ 2 / mean ^ 2)))
  #   return (sigma)
  # }
  # losA$mu <- lapply(losA$mean_los, function(x) gen_mu(x, sd_los))
  # losA$sigma <- lapply(losA$mean_los, function(x) gen_sigma(x, sd_los))
  # losA$los_params <- with(losA, paste(mu, sigma, sep=" , "))
} else {
  stop("est_method should be equal to 1 or 2")
}

# Set run time as the number of unique dates in arrivals (used in visit-based)
sim_length <- as.integer(length(unique(arrivals_all$date)))

# Create scenarios
scenarios <- list(arrivals_all %>%
                    rename(sc_arr = scenario) %>%
                    distinct(node, sc_arr, .keep_all = TRUE),
                  capacity %>% rename(s_cap = scenario),
                  losA %>% rename(s_los = scenario),
                  init_conds) %>%
  reduce(merge, by = "node", all = TRUE) %>%
  mutate(S = paste0(node, "_",  s_cap, "_", s_los, "_", sc_arr)) %>%
  pivot_wider(names_from = measure, values_from = value)

# Create arr_scenarios
arr_scenarios <- list(arrivals_all %>% rename(sc_arr = scenario),
                      capacity %>% rename(s_cap = scenario),
                      losA %>% rename(s_los = scenario),
                      init_conds) %>%
  reduce(merge, by = "node", all = TRUE) %>%
  mutate(S = paste0(node, "_", s_cap, "_", s_los, "_", sc_arr)) %>%
  pivot_wider(names_from = measure, values_from = value)
