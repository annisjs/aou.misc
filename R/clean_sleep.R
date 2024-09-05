#' Cleans sleep data
#' @param sleep_dat a data.table representing the sleep_summary table containing at least the following columns person_id, is_main_sleep, minute_asleep
#' @param data_of_birth a data.table containing person_id and date_of_birth columns
#' @examples 
#'\dontrun{
#' library(aou.phenotyper2)
#' output_folder <- "datasets"
#' sleep(output_folder)
#' demographics(output_folder)
#' fitbit_dat <- read_bucket("datasets/fitbit.csv")
#' demographics_dat <- read_bucket("datasets/demographics.csv")
#' sleep_cleaned <- clean_fitbit(fitbit_dat,demographics[,c("person_id","date_of_birth")])
#' }
#' @import data.table
#' @export
clean_sleep <- function(sleep_dat, date_of_birth)
{
  
  cat("Initial sleep cohort N:",length(unique(sleep_dat$person_id)))
  cat("\nDays:",nrow(sleep_dat))
  
  cat("\n\nRemoving is_main_sleep is FALSE")
  sleep_dat <- sleep_dat[is_main_sleep == TRUE]
  cat("\nN:",length(unique(sleep_dat$person_id)))
  cat("\nDays:",nrow(sleep_dat))
  
  cat("\n\nRemoving days with > 1440 minutes asleep")
  sleep_dat <- sleep_dat[minute_asleep <= 1440]
  cat("\nN:",length(unique(sleep_dat$person_id)))
  cat("\nDays:",nrow(sleep_dat))
  
  cat("\n\nRemoving days with 0 minutes asleep")
  sleep_dat <- sleep_dat[minute_asleep > 0]
  cat("\nN:",length(unique(sleep_dat$person_id)))
  cat("\nDays:",nrow(sleep_dat))
  
  cat("\n\nRemoving nights where age < 18")
  sleep_dat <- merge(sleep_dat, date_of_birth[,c("person_id","date_of_birth")],by="person_id")
  sleep_dat[, age :=  as.numeric(as.Date(sleep_date) - as.Date(date_of_birth)) / 365.25]
  sleep_dat <- sleep_dat[age >= 18]
  sleep_dat$date_of_birth = NULL
  sleep_dat$age = NULL
  cat("\nN:",length(unique(sleep_dat$person_id)))
  cat("\ndays:",nrow(sleep_dat))
  
  cat("\n\nRemoving subjects > 30% days < 4 hours sleep")
  sleep_dat[, proportion_lt_4hr_asleep := mean(minute_asleep < 4*60),.(person_id)]
  sleep_dat <- sleep_dat[proportion_lt_4hr_asleep <= .30]
  cat("\nN:",length(unique(sleep_dat$person_id)))
  cat("\nDays:",nrow(sleep_dat))  
  
  sleep_dat
}
