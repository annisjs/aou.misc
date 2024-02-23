#' Recode sex to Male, Female
#' @param a character vector corresponding to the gender or sex column in the person table. Sets missing/skipped responses to NA.
#' @export
recode_sex_fm <- function(sex)
{
  cat("Found the following levels for ")
  print(table(sex))
  cat("\nMapping:")
  cat("\nNot male, not female, prefer not to answer, or skipped -> NA")
  cat("\nNo matching concept                                    -> NA")
  cat("\nMale                                                   -> Male")
  cat("\nFemale                                                 -> Female")
  cat("\nDefault is NA")
  data.table::fcase(sex == "Not male, not female, prefer not to answer, or skipped", as.character(NA)
                    ,sex == "No matching concept"                                  , as.character(NA)
                    ,sex == "Male"                                                 , "Male"
                    ,sex == "Female"                                               , "Female"
                    ,default =                                                       as.character(NA))
}