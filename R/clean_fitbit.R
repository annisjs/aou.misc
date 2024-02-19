#' Cleans fitbit data
#' @param fitbit_dat a data.table representing the activity_summary table containing at least the following columns person_id, date, steps
#' @param wear_time a data.table from the output of the aou.phenotyper2::wear_time algorithm
#' @param data_of_birth a data.table containing person_id and date_of_birth columns
#' @examples 
#'\dontrun{
#' library(aou.phenotyper2)
#' output_folder <- "datasets"
#' fitbit(output_folder)
#' demographics(output_folder)
#' wear_time(output_folder)
#' fitbit_dat <- read_bucket("datasets/fitbit.csv")
#' wear_time_dat <- read_bucket("datasets/wear_time.csv")
#' demographics_dat <- read_bucket("datasets/demographics.csv")
#' fitbit_cleaned <- clean_fitbit(fitbit_dat,wear_time_dat,demographics[,c("person_id","date_of_birth")])
#' }
#' @export
clean_fitbit <- function(fitbit_dat,wear_time,date_of_birth)
{
  fitbit_dat <- merge(fitbit_dat,wear_time,by=c("person_id","date"))
  cat("\nInitial cohort")
  cat("\nN: ",length(unique(fitbit_dat$person_id)))
  cat("\nDays: ",nrow(fitbit_dat))

  cat("\n\nRemoving days where wear time < 10 hrs.")
  fitbit_dat <- fitbit_dat[wear_time >= 10]
  cat("\nN: ",length(unique(fitbit_dat$person_id)))
  cat("\nDays: ",nrow(fitbit_dat))

  cat("\n\nRemoving days where step count < 100.")
  fitbit_dat <- fitbit_dat[steps >= 100]
  cat("\nN: ",length(unique(fitbit_dat$person_id)))
  cat("\nDays: ",nrow(fitbit_dat))

  cat("\n\nRemoving days where step counts > 45,000.")
  fitbit_dat <- fitbit_dat[steps <= 45000]
  cat("\nN: ",length(unique(fitbit_dat$person_id)))
  cat("\nDays: ",nrow(fitbit_dat))

  cat("\n\nRemoving days where age < 18.")
  fitbit_dat <- merge(fitbit_dat,date_of_birth,by="person_id",all.x=TRUE)
  fitbit_dat[,age := as.numeric(as.Date(date) - as.Date(date_of_birth))/365.25]
  fitbit_dat <- fitbit_dat[age >= 18]
  cat("\nN: ",length(unique(fitbit_dat$person_id)))
  cat("\nDays: ",nrow(fitbit_dat))
  fitbit_dat
}