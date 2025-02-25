#' Subset fitbit data using an anchor date and a date range
#' 
#' @param fitbit_dat fitbit data data table
#' @param anchor_date the column name to be used as anchor
#' @param before time range in the past
#' @param after time range in the future
#' @param date_col date column for the subset 
#' @param id_col grouping variable for anchoring
#' @param return_all either to return all dates for the subset of patients or just in the given window
#' @examples 
#'\dontrun{
#' fitbit_dat <- subset_by_date_window(fitbit_dat, "covid_date", c(100,100))
#' }
#' @import data.table
#' @export
subset_by_date_window <- function(dat, anchor_date, before, after, date_col="date",
                                       id_col = "person_id", return_all = TRUE)
{
  dat[, max_date:=max(get(date_col)), .(get(id_col))]
  dat[, min_date:=min(get(date_col)), .(get(id_col))]
  dat <- dat[((get(anchor_date) - min_date) > before) & ((max_date - get(anchor_date)) > after)]
  if(!return_all){
    dat = dat[((get(anchor_date) - get(date_col))<before) & ((get(date_col) - get(anchor_date))<after)]
  }
  dat[, min_date := NULL]
  dat[, max_date := NULL]
  dat
}
