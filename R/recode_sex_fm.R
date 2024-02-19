#' Recode sex to Male, Female
#' @param a character vector corresponding to the gender or sex column in the person table. Sets missing/skipped responses to NA.
#' @export
recode_sex_fm <- function(sex)
{
  ifelse(sex == "Not male, not female, prefer not to answer, or skipped" |
           sex == "No matching concept",
         NA,
         ifelse(sex == "Male",
                "Male",
                "Female"))
}