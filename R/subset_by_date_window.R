#' Subset fitbit data using an anchor date and a date range
#' 
#' @param fitbit_dat fitbit data data table
#' @param anchor_date the column name to be used as anchor
#' @param before time range in the past
#' @param after time range in the future
#' @param date_column date column for the subset 
#' @param group_by grouping variable for anchoring
#' @examples 
#'\dontrun{
#' fitbit_dat <- subset_by_date_window(fitbit_dat, "covid_date", c(100,100))
#' }
#' @import data.table
#' @export
subset_by_date_window <- function(dat, anchor_date, date_range, date_col="date",
                                  id_col = "person_id")
{
  dat[, max_date:=max(get(date_col)), .(get(id_col))]
  dat[, min_date:=min(get(date_col)), .(get(id_col))]
  dat <- dat[(max_date - get(anchor_date) > date_range[[1]]) & (get(anchor_date) - min_date > date_range[[2]])]
  dat[, min_date := NULL]
  dat[, max_date := NULL]
  dat
}
