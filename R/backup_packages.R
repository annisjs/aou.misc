#' Backup packages to bucket
#' @examples 
#' \dontrun{
#' backup_packages()
#' }
#' @export
backup_packages <- function()
{
  package_path <- file.path(Sys.getenv("WORKSPACE_BUCKET"),"packages")
  system(str_glue('gsutil -m cp -r /home/jupyter/packages/* {package_path} 2>&1'), intern = TRUE)
}