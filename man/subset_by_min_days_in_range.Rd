% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset_by_min_days_in_range.R
\name{subset_by_min_days_in_range}
\alias{subset_by_min_days_in_range}
\title{Subsets time series data using a minimum of records during given period relative to an anchor column}
\usage{
subset_by_min_days_in_range(
  fitbit_dat,
  range_start,
  range_end,
  min_days_count,
  date_col,
  anchor_date,
  return_all = TRUE
)
}
\arguments{
\item{range_start}{start of time range - can be positive or negative}

\item{range_end}{end of time range - can be positive or negative}

\item{min_days_count}{minimum number of records to require for subsetting}

\item{date_col}{date column to use for each measurement}

\item{return_all}{either to return all dates for the subset of patients or just in the given window}

\item{dat}{a data.table representing the time series data with at least the following columns person_id, date}

\item{anchor_col_name}{the name of the column that is the anchor for the range}
}
\description{
Subsets time series data using a minimum of records during given period relative to an anchor column
}
\examples{
\dontrun{
fitbit_dat <- read_bucket("datasets/fitbit.csv")
subset_by_min_days_in_range(fitbit_dat, -365,0, 180, date)
}
}
