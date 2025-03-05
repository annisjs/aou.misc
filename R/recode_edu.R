#' Recode education
#' @param education_status character vector corresponding to the education level
#' @export
recode_edu <- function(education_status)
{
  cat("Found the following levels for ")
  print(table(education_status))
  cat("\nMapping:")
  cat("\nCollege graduate or advanced degree ->college")
  cat("\nHighest Grade: Five Through Eight ->no_college")
  cat("\nHighest Grade: Nine Through Eleven ->no_college")
  cat("\nHighest Grade: Twelve Or GED ->no_college")
  cat("\nHighest Grade: Advanced Degree ->college")
  cat("\nHighest Grade: College One to Three ->some_college")
  cat("\nHighest Grade: Never Attended ->no_college")
  cat("\nHighest Grade: One Through Four ->no_college")
  cat("\nLess than a high school degree or equivalent ->no_college")
  data.table::fcase(
        education_status == "PMI: Skip", as.character(NA),
        education_status == "PMI: Prefer Not To Answer", as.character(NA),
        education_status == "College graduate or advanced degree","college",
        education_status == "Highest Grade: Five Through Eight","no_college",
        education_status == "Highest Grade: Nine Through Eleven","no_college",
        education_status == "Highest Grade: Twelve Or GED","no_college",
        education_status == "Highest Grade: Advanced Degree","college",
        education_status == "Highest Grade: College One to Three","some_college",
        education_status == "Highest Grade: Never Attended","no_college",
        education_status == "Highest Grade: One Through Four","no_college",
        education_status == "Less than a high school degree or equivalent","no_college",
        default = as.character(NA)
      )
}