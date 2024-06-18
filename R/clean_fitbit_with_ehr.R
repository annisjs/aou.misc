#' Query and clean fitbit data and combine with ehr
#' 
#' @param output_folder the folder to check for cached queries
#' @param bucket the bucket to check for cached queries
#' @examples 
#'\dontrun{
#' fitbit_cleaned_combined <- clean_fitbit_with_ehr()
#' }
#' @import data.table
#' @import aou.phenotyper2
#' @export
clean_fitbit_with_ehr <- function(output_folder="datasets", bucket=NULL)
{
  dat = multi_query(c("fitbit", "demographics", "wear_time", "last_medical_encounter"), output_folder=output_folder, bucket=bucket)
  fitbit_dat <- clean_fitbit(dat$fitbit, dat$wear_time, dat$demographics[,c("person_id","date_of_birth")])
  fitbit_dat <- merge(fitbit_dat,dat$last_medical_encounter,by="person_id")
  fitbit_ehr_cohort <- fitbit_dat[,c("person_id")]
  fitbit_ehr_cohort <- fitbit_ehr_cohort[!duplicated(fitbit_ehr_cohort)]
  cat("\nFitbit + EHR cohort:",nrow(fitbit_ehr_cohort))
  fitbit_dat
}