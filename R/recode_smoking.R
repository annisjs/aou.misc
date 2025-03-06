#' Recode smoking status
#' @param smoking_status character vector corresponding to the smoking status
#' @export
recode_smoking <- function(smoking_status)
{
    cat("Found the following levels for ")
    print(table(smoking_status))
    cat("\nMapping:")
    cat("\n100 Cigs Lifetime: No","100 Cigs Lifetime: No")
    cat("\n100 Cigs Lifetime: Yes","100 Cigs Lifetime: Yes")
    cat("\nDefault is NA")
    data.table::fcase(
        smoking_status == "PMI: Dont Know", as.character(NA),
        smoking_status == "PMI: Skip", as.character(NA),
        smoking_status == "PMI: Prefer Not To Answer", as.character(NA),
        smoking_status == "100 Cigs Lifetime: No","100 Cigs Lifetime: No",
        smoking_status == "100 Cigs Lifetime: Yes","100 Cigs Lifetime: Yes",
        default = as.character(NA)
    )
}