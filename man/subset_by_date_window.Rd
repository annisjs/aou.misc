% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset_by_date_window.R
\name{subset_by_date_window}
\alias{subset_by_date_window}
\title{Subset fitbit data using an anchor date and a date range}
\usage{
subset_by_date_window(
  dat,
  anchor_date,
  before,
  after,
  date_col = "date",
  id_col = "person_id",
  return_all = TRUE
)
}
\arguments{
\item{anchor_date}{the column name to be used as anchor}

\item{before}{time range in the past}

\item{after}{time range in the future}

\item{date_col}{date column for the subset}

\item{id_col}{grouping variable for anchoring}

\item{return_all}{either to return all dates for the subset of patients or just in the given window}

\item{fitbit_dat}{fitbit data data table}
}
\description{
Subset fitbit data using an anchor date and a date range
}
\examples{
\dontrun{
fitbit_dat <- subset_by_date_window(fitbit_dat, "covid_date", c(100,100))
}
}
