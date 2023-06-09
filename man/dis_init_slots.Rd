% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dis_init_slots.R
\name{dis_init_slots}
\alias{dis_init_slots}
\title{Sample initial number of visits}
\usage{
dis_init_slots(
  z = parent.frame()$z,
  isr = parent.frame()$isr[z],
  sd_isr = parent.frame()$sd_isr[z],
  n_slots = parent.frame()$n_slots[z]
)
}
\arguments{
\item{z}{String - scenario}

\item{isr}{Float - mean for ISR distribution - default is from isr vector}

\item{sd_isr}{Float - standard deviation for ISR distribution - default is
from sd_isr vector}

\item{n_slots}{Float - number of visit slots available per day - default is
from n_slots vector}
}
\value{
Integer - initial service rate
}
\description{
Sample from normal distribution to find the number of visits required when
patient enters pathway.
}
\details{
Initial service rate (ISR) or initial visit rate (IVR) have a normal
distribution. Based on the provided mean and SD for that distribution,
samples to get ISR/IVR. It cannot be (a) less than 0, (b) more than
mean+SD*3, or (c) more than n_slots
}
\examples{
dis_init_slots(z = "P1_B_BCap_Blos_BArr", isr = 4, sd_isr = 0.5, n_slots = 230)
}
