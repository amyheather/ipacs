% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_setup.R
\name{sim_setup}
\alias{sim_setup}
\title{Set-up for simulation}
\usage{
sim_setup(
  model_type,
  scenarios = parent.frame()$scenarios,
  arr_scenarios = parent.frame()$arr_scenarios,
  costs = parent.frame()$costs,
  sd_los = parent.frame()$sd_los
)
}
\arguments{
\item{model_type}{String - either "visit" or "bed"}

\item{scenarios}{Dataframe with information about scenarios}

\item{arr_scenarios}{Dataframe with information about arrival scenarios}

\item{costs}{Dataframe with costs for community and acute for each location
and pathway}

\item{sd_los}{Float - standard deviation of length of stay distribution}
}
\value{
List of object names and objects
}
\description{
Filters scenarios, arr_scenarios and costs to visit or bed pathways.
Extracts parameters from scenarios dataframe.
Pivots subset of arr_scenarios.
Creates vector with each scenario name.
}
