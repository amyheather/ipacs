#' Add patient
#'
#' Add patient to visit-based simulation pathway
#'
#' When run add_patient(), it will increment the ID and n_pat, find length of
#' stay, initial visit rate and end visit rate. It will save these results in a
#' dataframe, and then check availability in service and modify resources
#' accordingly.
#'
#' @param in_system boolean (TRUE or FALSE) - is the patient already in the
#' system? If so, will produce adjusted length of stay and visit rates.
#' @param id integer
#' @param npat integer
#' @param req_visits tbc
#' @param patients dataframe
#' @param resources dataframe
#'
#' @importFrom utils tail
#'
#' @return List with updated id, npat, req_visits, patients, resources objects
#' @export
add_patient <- function(in_system,
                        id = parent.frame()$id,
                        npat = parent.frame()$npat,
                        req_visits = parent.frame()$req_visits,
                        patients = parent.frame()$patients,
                        resources = parent.frame()$resources){
  # Increment ID and npat
  id <- id + 1
  npat <- npat + 1

  if (in_system == FALSE) {
    # Find LOS and required visits
    los <- dis_los()
    init_slots <- dis_init_slots()
    end_slots <- dis_end_slots()
    visit_vector <-
      round(seq(from = init_slots,
                to = end_slots,
                length.out = los)) #full visit seq
  } else if (in_system == TRUE) {
    # Create temporary LOS using dis_los() (so it is longer)
    # Then get shorter LOS using dis_los2() which trims the templos
    # Then get end_slots (final number of visits) and init_slots (initial
    # number). Create sequence, then sample from tail for length of the
    # shorter LOS. This means patients already in system have a shorter LOS
    # and start from a later point that init_slots.
    # Then save vector of required visits
    # e.g.
    # temp vector: 4 4 4 3 3 3 3 2 2 2 2 2 1 1 1
    # final vector: 2 1 1 1
    templos <- dis_los()
    los <- dis_los2(templos)
    init_slots <- dis_init_slots()
    end_slots <- dis_end_slots()
    temp_visit_vector <- round(seq(from = init_slots,
                                   to = end_slots,
                                   length.out = templos))
    visit_vector <- tail(temp_visit_vector, los)
  }

  req_visits[[id]] <- visit_vector

  # Save to patients_inqueue dataframe
  patients$id[npat] <- id
  patients$los[npat] <- los
  patients$arrival_time[npat] <- t
  patients$start_service[npat] <- NA
  patients$end_service[npat] <- NA
  patients$wait_time[npat] <- 0
  patients$exit[npat] <- FALSE

  # Planning service, check resources
  # Create temporary t for incrementing when no resources available
  tt <- t
  # Create adjusted LOS
  los_adj <- patients$los[npat] - 1
  # While start_service = NA
  while (is.na(patients$start_service[npat]) == TRUE) {
    # If resources columns (from tt to LOS-1) are >= req_visits
    if (all(resources[tt:(tt + los_adj), ] >= req_visits[[id]]) == TRUE) {
      patients$start_service[npat] <- tt
      patients$end_service[npat] <- tt + los_adj
      # Decrease capacity
      resources[tt:(tt + los_adj), ] <-
        resources[tt:(tt + los_adj), ] - req_visits[[id]]
    } else {
      # If no sufficient resources, check for starting on the next day
      tt <- tt + 1
    }
  }

  # Return changed objects that are needed (e.g. things we increment on
  # or dataframes we use elsewhere)
  return(list(id, npat, req_visits, patients, resources))
}
