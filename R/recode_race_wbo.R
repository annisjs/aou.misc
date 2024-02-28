#' Recode race to White, Black, Other
#' @param race a character vector corresponding to the race column in the person table. Sets missing/skipped responses to NA.
#' @export
recode_race_wbo <- function(race)
{
    cat("Found the following levels for ")
    print(table(race))
    cat("\nMapping:")
    cat("\nI prefer not to answer    -> NA")
    cat("\nNone Indicated            -> NA")
    cat("\nPMI: Skip                 -> NA")
    cat("\nBlack or African American -> Black")
    cat("\nWhite                     -> White")
    cat("\nDefault is 'Other'")
    data.table::fcase(race == "I prefer not to answer"    , as.character(NA)
                      ,race == "None Indicated"           , as.character(NA)
                      ,race == "PMI: Skip"                , as.character(NA)
                      ,race == "Black or African American", "Black"
                      ,race == "White"                    , "White"
                      ,default =                            "Other"
                      )

}
