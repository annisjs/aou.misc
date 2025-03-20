#' Subsets time series data using a minimum of records during given period relative to an anchor column
#' @param dat a data.table representing the time series data with at least the following columns person_id, date
#' @param range_start start of time range - can be positive or negative
#' @param range_end end of time range - can be positive or negative
#' @param min_days_count minimum number of records to require for subsetting
#' @param date_col date column to use for each measurement
#' @param anchor_col_name the name of the column that is the anchor for the range
#' @param return_all either to return all dates for the subset of patients or just in the given window
#' @examples 
#'\dontrun{
#' fitbit_dat <- read_bucket("datasets/fitbit.csv")
#' subset_by_min_days_in_range(fitbit_dat, -365,0, 180, date)
#' }
#' @import data.table
#' @export
subset_by_min_days_in_range = function(fitbit_dat, range_start, range_end, min_days_count, date_col, anchor_date, return_all=TRUE){
  dat = fitbit_dat
  dat[, tte := as.numeric(get(date_col) - get(anchor_col_name))]
  dat_in_range = dat[(tte >= range_start) & (tte <= range_end)]
  if(!return_all){
    dat = merge(dat_in_range, dat_in_range[, .(days_in_range = .N), .(person_id)],
                by="person_id")
  }else{
    dat = merge(dat, dat_in_range[, .(days_in_range = .N), .(person_id)],
                by="person_id")
    
  }
  dat = dat[days_in_range >= min_days_count]
  dat$tte = NULL
  dat$days_in_range = NULL
  return(dat)
}
