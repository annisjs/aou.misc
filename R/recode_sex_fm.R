#' Recode sex to Male, Female
#' @param sex character vector corresponding to the gender or sex column in the person table. Sets missing/skipped responses to NA.
#' @export
recode_sex_fm <- function(sex)
{
  cat("Found the following levels for ")
  print(table(sex))
  cat("\nMapping:")
  cat("\nMale   -> Male")
  cat("\nFemale -> Female")
  cat("\nDefault is NA")
  data.table::fcase(sex == "Male"    , "Male"
                    ,sex == "Female" , "Female"
                    ,default =       as.character(NA))
}