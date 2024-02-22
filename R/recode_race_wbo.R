#' Recode race to White, Black, Other
#' @param race a character vector corresponding to the race column in the person table. Sets missing/skipped responses to NA.
#' @export
recode_race_wbo <- function(race)
{
  ifelse(race == "I prefer not to answer" |
         race == "None Indicated" |
         race == "PMI: Skip",
       NA,
       ifelse(race == "Black or African American",
              "Black",
              ifelse(race == "White",
                     "White",
                     "Other")))
}
