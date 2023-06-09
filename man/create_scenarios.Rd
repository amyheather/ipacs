% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_scenarios.R
\name{create_scenarios}
\alias{create_scenarios}
\title{Create scenarios}
\usage{
create_scenarios(
  scenarios,
  arrivals_all = parent.frame()$arrivals_all,
  capacity = parent.frame()$capacity,
  losA = parent.frame()$losA,
  init_conds = parent.frame()$init_conds
)
}
\arguments{
\item{scenarios}{TRUE or FALSE. If set to TRUE, will find rows with
distinct node and sc_arr}

\item{arrivals_all}{Dataframe with arrivals information, imported from excel}

\item{capacity}{Dataframe with capacity information, imported from excel}

\item{losA}{Dataframe with length of stay information}

\item{init_conds}{Dataframe information regarding initial conditions}
}
\value{
Scenarios dataframe
}
\description{
Combine arrivals_all, capacity, losA and init_conds to create scenarios and
arr_scenarios
}
